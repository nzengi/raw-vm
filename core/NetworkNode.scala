package core

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure, Try}
import java.util.{Timer, TimerTask}
import java.security.{KeyPair, MessageDigest}
import java.util.UUID
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap

// P2P Mesaj Türleri
sealed trait NetworkMessage {
  def messageId: String
  def sender: String
}

// Düğüm keşfetme mesajı
case class DiscoveryMessage(
  messageId: String,
  sender: String,
  nodeEndpoint: String,
  timestamp: Long
) extends NetworkMessage

// DAG Düğümü yayma mesajı
case class DAGNodeBroadcast(
  messageId: String,
  sender: String,
  node: DAGNode
) extends NetworkMessage

// İşlem havuzu mesajı
case class TransactionPoolMessage(
  messageId: String,
  sender: String,
  transactions: Seq[Transaction]
) extends NetworkMessage

// Durum senkronizasyon mesajı
case class StateSyncRequest(
  messageId: String,
  sender: String,
  objectIds: Seq[String],
  fromVersion: Long
) extends NetworkMessage

case class StateSyncResponse(
  messageId: String,
  sender: String,
  requestId: String,
  objects: Seq[ObjectData]
) extends NetworkMessage

// Tavsiye edilen gas fiyatı mesajı
case class GasPriceMessage(
  messageId: String,
  sender: String,
  gasPrice: GasPrice,
  timestamp: Long
) extends NetworkMessage

/**
 * Ağ düğümü mesaj türleri
 */
object MessageType {
  val HANDSHAKE = 0x01
  val PEERS_REQUEST = 0x02
  val PEERS_RESPONSE = 0x03
  val TRANSACTION = 0x04
  val BLOCK = 0x05
  val NODE_REQUEST = 0x06
  val NODE_RESPONSE = 0x07
  val LATEST_HEIGHT_REQUEST = 0x08
  val LATEST_HEIGHT_RESPONSE = 0x09
  val NODES_AT_HEIGHT_REQUEST = 0x0A
  val NODES_AT_HEIGHT_RESPONSE = 0x0B
  val OBJECT_REQUEST = 0x0C
  val OBJECT_RESPONSE = 0x0D
  val SYNC_STATUS_REQUEST = 0x0E
  val SYNC_STATUS_RESPONSE = 0x0F
}

// Ağ Düğümü (P2P Ağının bir parçası)
class NetworkNode(
  val nodeId: String,
  val endpoint: String,
  val keyPair: KeyPair,
  val bootstrapNodes: Seq[String] = Seq.empty
)(implicit val ec: ExecutionContext) {
  import MessageType._
  
  // Bağlantılı düğümler
  private val peers = mutable.Set[String]()
  
  // Mesaj işleyiciler
  private val messageHandlers = mutable.Map[Class[_], NetworkMessage => Unit]()
  
  // Bekleyen istekler
  private val pendingRequests = mutable.Map[String, Promise[NetworkMessage]]()
  
  // DAG Konsensus ve Nesne Deposu
  val dagConsensus = new DAGConsensus(nodeId, Map.empty) // Basitleştirilmiş
  val objectStore = new ObjectStore()
  val zkSystem = new ZKSystem()
  
  // İşlem ücretleri yöneticisi
  val feeManager = new FeeManager()
  
  // İşlem havuzu
  private val transactionPool = TransactionPool.create(10000)
  
  // İşlem sıra numarası takibi (adres -> son nonce)
  private val nonceTracker = mutable.Map[String, Long]()
  
  // İşlem havuzu doluluk oranı (0.0 - 1.0)
  private var transactionPoolLoad: Double = 0.0
  
  // Istek-yanıt kanal eşlemeleri
  private val requestChannels = new ConcurrentHashMap[String, AnyRef]()
  
  // İstek timeout değeri (milisaniye)
  private val requestTimeout = 5000
  
  // Zamanlayıcı
  private val timer = new Timer("NetworkNode", true)
  
  case class PeerInfo(
    id: String,
    endpoint: String,
    lastSeen: Long,
    isActive: Boolean = true
  )
  
  // Düğümü başlat
  def start(): Unit = {
    // Bootstrap düğümlerine bağlan
    if (bootstrapNodes.nonEmpty) {
      connectToBootstrapNodes()
    }
    
    // Periyodik görevleri başlat
    schedulePeriodicTasks()
    
    // Mesaj işleyicileri kaydet
    registerMessageHandlers()
    
    println(s"Node $nodeId started at $endpoint")
  }
  
  // Mesaj işleyicileri kaydet
  private def registerMessageHandlers(): Unit = {
    registerHandler[DiscoveryMessage] { message =>
      handleDiscovery(message)
    }
    
    registerHandler[DAGNodeBroadcast] { message =>
      handleDAGNodeBroadcast(message)
    }
    
    registerHandler[TransactionPoolMessage] { message =>
      handleTransactionPoolMessage(message)
    }
    
    registerHandler[StateSyncRequest] { message =>
      handleStateSyncRequest(message)
    }
    
    registerHandler[StateSyncResponse] { message =>
      handleStateSyncResponse(message)
    }
    
    registerHandler[GasPriceMessage] { message =>
      handleGasPriceMessage(message)
    }
  }
  
  // İşleyici kaydet
  private def registerHandler[T <: NetworkMessage](handler: T => Unit): Unit = {
    messageHandlers(classOf[T]) = message => handler(message.asInstanceOf[T])
  }
  
  // Mesaj işle
  private def handleMessage(message: NetworkMessage): Unit = {
    // İsteği çöz
    pendingRequests.get(message.messageId).foreach { promise =>
      promise.success(message)
      pendingRequests.remove(message.messageId)
    }
    
    // İşleyiciyi bul ve çalıştır
    messageHandlers.get(message.getClass).foreach(_(message))
  }
  
  // Gas fiyatı mesajını işle
  private def handleGasPriceMessage(message: GasPriceMessage): Unit = {
    // Diğer düğümün gas fiyatını al, ancak şimdilik hiçbir şey yapma
    // Gerçek bir sistemde, farklı düğümlerin gas fiyatlarını 
    // kendi fiyatlarınızı güncellemeyi düşünmek için kullanabilirsiniz
    println(s"Received gas price from ${message.sender}: ${message.gasPrice.basePrice}")
  }
  
  // Keşif mesajını işle
  private def handleDiscovery(message: DiscoveryMessage): Unit = {
    val peer = PeerInfo(
      message.sender,
      message.nodeEndpoint,
      System.currentTimeMillis()
    )
    
    peers.add(message.sender)
    
    // Cevap gönder
    if (message.sender != nodeId) {
      sendDiscovery(message.sender)
    }
  }
  
  // DAG düğüm broadcast mesajını işle
  private def handleDAGNodeBroadcast(message: DAGNodeBroadcast): Unit = {
    if (message.sender != nodeId) {
      val result = dagConsensus.addNode(message.node)
      if (result) {
        // İşlem havuzundan onaylanan işlemleri kaldır
        val processedTxIds = message.node.transactions.map(_.id).toSet
        transactionPool.removeTransactions(processedTxIds)
        
        // Başarılı ise diğer düğümlere yay
        broadcastMessage(message, excludeNodeId = message.sender)
      }
    }
  }
  
  // İşlem havuzu mesajını işle
  private def handleTransactionPoolMessage(message: TransactionPoolMessage): Unit = {
    if (message.sender != nodeId) {
      // İşlemleri havuza ekle
      val addedTxs = message.transactions.filter { tx =>
        // İşlemi doğrula
        validateTransaction(tx) && transactionPool.addTransaction(tx)
      }
      
      // Bazı işlemler eklendiyse, diğer düğümlere yay (ama göndereni hariç tut)
      if (addedTxs.nonEmpty) {
        val forwardMessage = TransactionPoolMessage(
          UUID.randomUUID().toString,
          nodeId,
          addedTxs
        )
        
        broadcastMessage(forwardMessage, excludeNodeId = message.sender)
      }
    }
  }
  
  // İşlemi doğrula
  private def validateTransaction(tx: Transaction): Boolean = {
    // 1. İşlemin nonce'unu kontrol et
    val currentNonce = nonceTracker.getOrElse(tx.sender, 0L)
    if (tx.nonce <= currentNonce) {
      println(s"Transaction ${tx.id} has invalid nonce: ${tx.nonce} <= $currentNonce")
      return false
    }
    
    // 2. ZK kanıtlarını doğrula (varsa)
    if (tx.zkProof.exists(proof => !zkSystem.verifyProof(proof))) {
      println(s"Transaction ${tx.id} has invalid ZK proof")
      return false
    }
    
    // 3. Gas maliyetini ve bakiye yeterliliğini kontrol et
    val gasResult = dagConsensus.validateTransactionFee(tx)
    if (!gasResult.success) {
      println(s"Transaction ${tx.id} failed gas check: ${gasResult.error.getOrElse("Unknown error")}")
      return false
    }
    
    // 4. İşlemin referans verdiği nesneleri kontrol et
    val allObjectsExist = tx.objectRefs.forall { objId =>
      objectStore.getObject(objId).isDefined
    }
    
    if (!allObjectsExist) {
      println(s"Transaction ${tx.id} references non-existent objects")
      return false
    }
    
    // Geçerli
    true
  }
  
  // Durum senkronizasyon isteğini işle
  private def handleStateSyncRequest(message: StateSyncRequest): Unit = {
    // İstenen nesneleri al
    val objects = message.objectIds.flatMap { id =>
      objectStore.getObject(id)
    }
    
    // Cevap gönder
    val response = StateSyncResponse(
      UUID.randomUUID().toString,
      nodeId,
      message.messageId,
      objects
    )
    
    sendMessageToPeer(message.sender, response)
  }
  
  // Durum senkronizasyon yanıtını işle
  private def handleStateSyncResponse(message: StateSyncResponse): Unit = {
    // Alınan nesneleri işle
    message.objects.foreach { obj =>
      objectStore.addObject(obj)
    }
  }
  
  // Periyodik görevleri zamanla
  private def schedulePeriodicTasks(): Unit = {
    // Eş keşfi (30 saniyede bir)
    timer.schedule(new TimerTask {
      override def run(): Unit = discoverPeers()
    }, 0, 30000)
    
    // İşlem havuzu durumunu yayınlama (60 saniyede bir)
    timer.schedule(new TimerTask {
      override def run(): Unit = broadcastTransactionPoolStatus()
    }, 60000, 60000)
    
    // Düğüm durumu yayınlama (15 saniyede bir)
    timer.schedule(new TimerTask {
      override def run(): Unit = broadcastNodeStatus()
    }, 15000, 15000)
  }
  
  // İşlem havuzu durumunu yayınla
  private def broadcastTransactionPoolStatus(): Unit = {
    // Havuz istatistiklerini al
    val poolStats = transactionPool.getStatistics
    val loadFactor = transactionPool.getLoadFactor
    
    // Gas fiyatını güncelle
    feeManager.updateNetworkLoad(loadFactor)
    
    // Yeni işlemleri yayınla
    broadcastPendingTransactions()
  }
  
  // Bekleyen işlemleri yayınla
  private def broadcastPendingTransactions(): Unit = {
    if (peers.isEmpty) return
    
    // Her eşe maksimum 100 işlem gönder
    val maxTxPerPeer = 100
    
    // Tüm bekleyen işlemleri al (öncelik sıralı)
    val pendingTxs = transactionPool.getAllPendingTransactions
    
    if (pendingTxs.isEmpty) return
    
    // İşlemleri gruplara böl (yüksek öncelikli işlemler daha sık paylaşılsın)
    val txChunks = pendingTxs.grouped(maxTxPerPeer).toList
    
    // Her eşe farklı bir işlem grubu gönder
    peers.zipWithIndex.foreach { case (peer, idx) =>
      val chunkIndex = idx % math.max(1, txChunks.size)
      val txsToSend = txChunks(chunkIndex)
      
      if (txsToSend.nonEmpty) {
        val message = TransactionPoolMessage(
          messageId = UUID.randomUUID().toString,
          sender = nodeId,
          transactions = txsToSend
        )
        
        sendMessageToPeer(peer, message)
      }
    }
  }
  
  // Bootstrap düğümlerine bağlan
  private def connectToBootstrapNodes(): Unit = {
    bootstrapNodes.foreach { endpoint =>
      connectToPeer(endpoint)
    }
  }
  
  // Keşif mesajı gönder
  private def sendDiscovery(targetPeerId: String): Unit = {
    val message = DiscoveryMessage(
      UUID.randomUUID().toString,
      nodeId,
      endpoint,
      System.currentTimeMillis()
    )
    
    sendMessageToPeer(targetPeerId, message)
  }
  
  // Keşif mesajını tüm düğümlere broadcast et
  private def broadcastDiscovery(): Unit = {
    val message = DiscoveryMessage(
      UUID.randomUUID().toString,
      nodeId,
      endpoint,
      System.currentTimeMillis()
    )
    
    broadcastMessage(message)
  }
  
  // DAG düğümünü yay
  private def broadcastDAGNode(node: DAGNode): Unit = {
    val message = DAGNodeBroadcast(
      UUID.randomUUID().toString,
      nodeId,
      node
    )
    
    broadcastMessage(message)
  }
  
  // Mesajı bir düğüme gönder
  private def sendMessageToPeer(peerId: String, message: NetworkMessage): Future[Boolean] = {
    if (peers.contains(peerId)) {
      // Gerçek uygulamada burada TCP/IP üzerinden gönderim olacak
      // Burada simüle ediyoruz
      
      Future {
        // Simülasyon: gerçek bir uygulamada burada ağ üzerinden gönderim olacak
        println(s"Sending ${message.getClass.getSimpleName} to $peerId")
        true
      }
    } else {
      Future.successful(false)
    }
  }
  
  // Mesajı tüm düğümlere yay
  private def broadcastMessage(message: NetworkMessage, excludeNodeId: String = ""): Unit = {
    val activePeers = peers.filter(_ != excludeNodeId)
    
    activePeers.foreach { peerId =>
      sendMessageToPeer(peerId, message)
    }
  }
  
  // İşlem ekle
  def addTransaction(tx: Transaction): Boolean = {
    if (validateTransaction(tx)) {
      val added = transactionPool.addTransaction(tx)
      
      if (added) {
        // Nonce değerini güncelle
        val currentNonce = nonceTracker.getOrElse(tx.sender, 0L)
        if (tx.nonce > currentNonce) {
          nonceTracker(tx.sender) = tx.nonce
        }
        
        // İşlemi diğer düğümlere yay
        val message = TransactionPoolMessage(
          UUID.randomUUID().toString,
          nodeId,
          Seq(tx)
        )
        
        broadcastMessage(message)
        
        // Havuz yük durumuna göre gas fiyatını güncelle
        val loadFactor = transactionPool.getLoadFactor
        feeManager.updateNetworkLoad(loadFactor)
      }
      
      added
    } else {
      false
    }
  }
  
  // İşlem oluştur 
  def createTransaction(
    sender: String,
    objectRefs: Seq[String],
    data: Array[Byte],
    zkProof: Option[Array[Byte]] = None,
    priority: Boolean = false
  ): Transaction = {
    // Nonce hesapla
    val nonce = getNextNonce(sender)
    
    // Gas tahmini hesapla
    val gasEstimation = estimateGas(data, objectRefs)
    
    // Önerilen ücreti al
    val recommendedFee = feeManager.getRecommendedFee(gasEstimation, priority)
    
    // İşlem oluştur
    val tx = Transaction(
      id = sender + "-" + nonce + "-" + Instant.now.getEpochSecond,
      sender = sender,
      objectRefs = objectRefs,
      data = data,
      zkProof = zkProof,
      fee = recommendedFee,
      nonce = nonce
    )
    
    tx
  }
  
  // İşlem havuzunu getir
  def getPendingTransactions: Seq[Transaction] = {
    // Varsayılan olarak 100 işlem al (blok üretimi için)
    getPendingTransactions(100)
  }
  
  // Belirli sayıda bekleyen işlem al
  def getPendingTransactions(count: Int): Seq[Transaction] = {
    transactionPool.getTransactions(count)
  }
  
  // İşlem havuzu istatistiklerini al
  def getTransactionPoolStatistics: Map[String, Long] = {
    transactionPool.getStatistics
  }
  
  // İşlemi havuzdan sil
  def removeTransaction(txId: String): Boolean = {
    transactionPool.updateTransactionStatus(txId, TxStatus.Dropped)
  }
  
  // İşlemi onaylanmış olarak işaretle
  def confirmTransaction(txId: String, blockHeight: Long): Boolean = {
    transactionPool.updateTransactionStatus(txId, TxStatus.Confirmed, Some(blockHeight))
  }
  
  // İşlemi başarısız olarak işaretle
  def failTransaction(txId: String): Boolean = {
    transactionPool.updateTransactionStatus(txId, TxStatus.Failed)
  }
  
  // İşlem durumunu al
  def getTransactionStatus(txId: String): Option[TxStatus] = {
    transactionPool.getTransactionStatus(txId)
  }
  
  // İşlem detaylarını al
  def getTransaction(txId: String): Option[Transaction] = {
    transactionPool.getTransaction(txId)
  }
  
  // İşlem onaylama callback'ini ayarla
  def setTransactionConfirmationCallback(callback: (Transaction, Long) => Unit): Unit = {
    transactionPool.setConfirmCallback(callback)
  }
  
  // İşlem öncelik stratejisini ayarla
  def setTransactionPriorityStrategy(strategy: String): Unit = {
    val priorityStrategy = strategy.toLowerCase match {
      case "fee" => new FeeBasedPriority()
      case "waittime" => new WaitTimeBasedPriority()
      case _ => new HybridPriorityStrategy()
    }
    
    transactionPool.setPriorityStrategy(priorityStrategy)
  }
  
  // Sonraki nonce değerini hesapla
  private def getNextNonce(sender: String): Long = {
    val currentNonce = transactionPool
      .filter(_.sender == sender)
      .map(_.nonce)
      .maxOption
      .getOrElse(-1L)
    
    currentNonce + 1
  }
  
  // Gas tahmini yap
  private def estimateGas(data: Array[Byte], objectRefs: Seq[String]): Long = {
    // Temel gas
    var gas = 21000L
    
    // Veri için gas
    gas += data.length * 16L
    
    // Nesne referansları için gas
    gas += objectRefs.size * 200L
    
    gas
  }
  
  // İşlem havuzunu temizle
  private def cleanTransactionPool(): Unit = {
    // Zaman aşımına uğramış işlemleri temizle
    val currentTime = Instant.now.getEpochSecond
    val timeoutThreshold = 3600 // 1 saat
    
    val expiredTransactions = transactionPool.filter { tx =>
      currentTime - tx.timestamp > timeoutThreshold
    }
    
    transactionPool.removeTransactions(expiredTransactions.map(_.id))
    
    // Havuz doluluk oranını güncelle
    updateTransactionPoolLoad()
  }
  
  // İşlem havuzu doluluk oranını güncelle
  def updateTransactionPoolLoad(): Unit = {
    val maxPoolSize = 10000 // Örnek olarak
    transactionPoolLoad = Math.min(1.0, transactionPool.size.toDouble / maxPoolSize)
    
    // Gas fiyatını güncelle
    feeManager.updateNetworkLoad(transactionPoolLoad)
  }
  
  // ZK Sistemi
  class ZKSystem {
    // ZK işlemleri burada uygulanır
  }
  
  // BLOK SENKRONİZASYONU İÇİN METOTLAR
  
  /**
   * Bir eşten en son yükseklik bilgisini iste
   * @param peer Eş adresi
   * @return En son yükseklik (bazı durumlarda None olabilir)
   */
  def requestLatestHeight(peer: String): Option[Int] = {
    val message = createMessage(LATEST_HEIGHT_REQUEST, Array.emptyByteArray)
    val futureResponse = sendMessageToPeer(peer, message)
    
    // Senkron çağrı için dönüştür (gerçek uygulamada asenkron kullanın)
    try {
      val responseOption = scala.concurrent.Await.result(futureResponse, 
                            scala.concurrent.duration.Duration(requestTimeout, "millis"))
      
      responseOption.map { responseData =>
        val heightStr = new String(responseData)
        Try(heightStr.toInt).getOrElse(0)
      }
    } catch {
      case _: Exception => None
    }
  }
  
  /**
   * Belirli bir yükseklikteki düğüm kimliklerini iste
   * @param peer Eş adresi
   * @param height Yükseklik
   * @return Düğüm kimlikleri
   */
  def requestNodeIdsAtHeight(peer: String, height: Int): Seq[String] = {
    val requestData = height.toString.getBytes
    val message = createMessage(NODES_AT_HEIGHT_REQUEST, requestData)
    val futureResponse = sendMessageToPeer(peer, message)
    
    // Senkron çağrı için dönüştür (gerçek uygulamada asenkron kullanın)
    try {
      val responseOption = scala.concurrent.Await.result(futureResponse, 
                            scala.concurrent.duration.Duration(requestTimeout, "millis"))
      
      responseOption.map { responseData =>
        val nodeIdsStr = new String(responseData)
        nodeIdsStr.split(",").toSeq
      }.getOrElse(Seq.empty)
    } catch {
      case _: Exception => Seq.empty
    }
  }
  
  /**
   * Belirli bir düğümü iste
   * @param peer Eş adresi
   * @param nodeId Düğüm kimliği
   * @return Düğüm (bazı durumlarda None olabilir)
   */
  def requestNode(peer: String, nodeId: String): Option[DAGNode] = {
    val requestData = nodeId.getBytes
    val message = createMessage(NODE_REQUEST, requestData)
    val futureResponse = sendMessageToPeer(peer, message)
    
    // Senkron çağrı için dönüştür (gerçek uygulamada asenkron kullanın)
    try {
      val responseOption = scala.concurrent.Await.result(futureResponse, 
                            scala.concurrent.duration.Duration(requestTimeout, "millis"))
      
      responseOption.flatMap { responseData =>
        deserializeNode(responseData)
      }
    } catch {
      case e: Exception => 
        println(s"Error requesting node $nodeId from peer $peer: ${e.getMessage}")
        None
    }
  }
  
  /**
   * DAG düğümünü deserilize et
   * @param data Seri hale getirilmiş düğüm verisi
   * @return Düğüm nesnesi (başarısız olursa None)
   */
  private def deserializeNode(data: Array[Byte]): Option[DAGNode] = {
    // Gerçek uygulamada JSON, Protobuf veya özel bir format kullanılır
    // Bu örnek basitleştirilmiştir
    
    try {
      val nodeStr = new String(data)
      val parts = nodeStr.split("\\|")
      
      if (parts.length < 7) return None
      
      val id = parts(0)
      val parentIds = parts(1).split(",").filter(_.nonEmpty).toSet
      val creator = parts(2)
      val timestamp = parts(3).toLong
      val transactions = Seq.empty[Transaction] // Gerçekte deserilize edilmeli
      val signature = parts(5).getBytes
      val height = parts(6).toInt
      val round = if (parts.length > 7) parts(7).toInt else 0
      
      Some(DAGNode(
        id = id,
        parentIds = parentIds,
        creator = creator,
        timestamp = timestamp,
        transactions = transactions,
        signature = signature,
        height = height,
        round = round
      ))
    } catch {
      case e: Exception =>
        println(s"Error deserializing node: ${e.getMessage}")
        None
    }
  }
  
  /**
   * DAG'daki en son yüksekliği al
   * @return En son yükseklik
   */
  def getLatestHeight: Int = {
    // DAG'daki en son yüksekliği al
    // Bu örnekte basitleştirme amacıyla rastgele bir değer döndürüyoruz
    50
  }
  
  // Mesaj oluştur
  private def createMessage(messageType: Int, data: Array[Byte]): Array[Byte] = {
    // Mesaj formatı: [messageType(1 byte)][nodeId(32 bytes)][timestamp(8 bytes)][dataLength(4 bytes)][data(N bytes)]
    // Basitleştirilmiş versiyon için yalnızca veriyi döndürüyoruz
    data
  }
}

// Test kodu
object NetworkNodeTest {
  def main(args: Array[String]): Unit = {
    println("Setting up test network...")
    
    // KeyPair yardımcı sınıfı (Blockchain.scala içinde tanımlanmıştır)
    val generateKeyPair = () => {
      val keyGen = java.security.KeyPairGenerator.getInstance("RSA")
      keyGen.initialize(2048)
      keyGen.generateKeyPair()
    }
    
    // Bootstrap düğümünü oluştur
    val bootstrapKeyPair = generateKeyPair()
    val bootstrapNode = new NetworkNode(
      nodeId = "bootstrap-node",
      endpoint = "127.0.0.1:9000",
      keyPair = bootstrapKeyPair
    )
    bootstrapNode.start()
    println("Bootstrap node started")
    
    // Biraz bekle
    Thread.sleep(1000)
    
    // İkinci düğümü oluştur ve bootstrap düğümüne bağlan
    val node2KeyPair = generateKeyPair()
    val node2 = new NetworkNode(
      nodeId = "node-2",
      endpoint = "127.0.0.1:9001",
      keyPair = node2KeyPair,
      bootstrapNodes = Seq("127.0.0.1:9000")
    )
    node2.start()
    println("Node 2 started and connected to bootstrap node")
    
    // Biraz bekle
    Thread.sleep(1000)
    
    // Üçüncü düğümü oluştur ve her iki düğüme de bağlan
    val node3KeyPair = generateKeyPair()
    val node3 = new NetworkNode(
      nodeId = "node-3",
      endpoint = "127.0.0.1:9002",
      keyPair = node3KeyPair,
      bootstrapNodes = Seq("127.0.0.1:9000", "127.0.0.1:9001")
    )
    node3.start()
    println("Node 3 started and connected to both nodes")
    
    // Test işlemi oluştur
    val txData = "Test transaction data".getBytes
    val testTx = bootstrapNode.createTransaction(
      sender = "test-user",
      objectRefs = Seq("test-object-1"),
      data = txData,
      priority = true
    )
    
    // İşlemi ilk düğüme ekle
    bootstrapNode.addTransaction(testTx)
    println(s"Added test transaction ${testTx.id} to bootstrap node")
    println(s"Transaction fee: ${testTx.fee.gasPrice.basePrice} (base) + ${testTx.fee.gasPrice.priorityBoost} (priority boost)")
    println(s"Gas limit: ${testTx.fee.gasLimit}, Max fee: ${testTx.maxFee}")
    
    // Biraz bekle ve diğer düğümlere yayılmasını izle
    println("Waiting for transaction propagation...")
    Thread.sleep(5000)
    
    // DAG düğümlerinin oluşturulmasını bekle
    println("Waiting for DAG node creation...")
    Thread.sleep(5000)
    
    println("Test completed")
  }
} 