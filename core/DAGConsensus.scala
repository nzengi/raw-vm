package core

import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import java.security.MessageDigest
import java.util.{Base64, UUID}
import java.time.Instant
import java.security.{Signature, KeyFactory, PublicKey}
import java.security.spec.X509EncodedKeySpec

// Import Transaction and TransactionFee from the correct package
import core.tx.{Transaction, TransactionFee}

// DAG'daki bir düğümü temsil eder
case class DAGNode(
  id: String,                      // Benzersiz düğüm tanımlayıcısı
  parentIds: Set[String],          // Referans verilen ebeveyn düğümlerin kimliği
  creator: String,                 // Düğümü oluşturan doğrulayıcının adresi
  timestamp: Long,                 // Oluşturma zaman damgası
  transactions: Seq[Transaction],  // Düğümün içerdiği işlemler
  signature: Array[Byte],          // Doğrulayıcı imzası
  height: Int,                     // DAG'daki yükseklik
  round: Int                       // Konsensus turu
) {
  lazy val hash: String = calculateHash()
  
  private def calculateHash(): String = {
    val content = s"$id|${parentIds.mkString(",")}|$creator|$timestamp|${transactions.map(_.id).mkString(",")}"
    val digest = MessageDigest.getInstance("SHA-256").digest(content.getBytes)
    Base64.getEncoder.encodeToString(digest)
  }
  
  def verify(validatorPublicKeys: Map[String, Array[Byte]]): Boolean = {
    // İmza doğrulaması (Ed25519)
    try {
      validatorPublicKeys.get(creator) match {
        case Some(pubKeyBytes) =>
          val keyFactory = KeyFactory.getInstance("EdDSA")
          val publicKey = keyFactory.generatePublic(new X509EncodedKeySpec(pubKeyBytes))
          
          val sig = Signature.getInstance("EdDSA")
          sig.initVerify(publicKey)
          
          // İmzalanacak veriler (hash)
          val hashBytes = this.hash.getBytes
          sig.update(hashBytes)
          
          sig.verify(signature)
          
        case None => false
      }
    } catch {
      case e: Exception => 
        println(s"İmza doğrulama hatası: ${e.getMessage}")
        false
    }
  }
}

// İşlem
case class Transaction(
  id: String,                     // İşlem kimliği
  sender: String,                 // Gönderen adres
  objectRefs: Seq[String],        // Etkilenen nesnelerin referansları
  data: Array[Byte],              // İşlem verisi (serileştirilmiş komutlar)
  zkProof: Option[Array[Byte]] = None,  // Opsiyonel ZK kanıtı
  fee: TransactionFee,            // İşlem ücreti bilgisi
  nonce: Long = 0                 // İşlem sıralaması ve tekrarları önlemek için
) {
  // İşlemin özeti (hash)
  lazy val hash: String = {
    val content = s"$id|$sender|${objectRefs.mkString(",")}|${data.length}|$nonce"
    val digest = MessageDigest.getInstance("SHA-256").digest(content.getBytes ++ data)
    Base64.getEncoder.encodeToString(digest)
  }
  
  // İşlemin maksimum ödeyebileceği toplam ücret
  def maxFee: Long = fee.maxPossibleFee
  
  // İşlemin tahmini ücret bilgisi
  def estimatedFee: Long = fee.estimatedFee
}

// DAG tabanlı konsensus motoru
class DAGConsensus(
  val nodeId: String,
  val validatorSet: Map[String, Long], // Validator -> Stake miktarı
  val slashingConditions: SlashingConditions = SlashingConditions()
) {
  private val nodes = mutable.Map[String, DAGNode]()
  private val tips = mutable.Set[String]() // Şu anki DAG yaprak düğümleri
  private var latestRound: Int = 0
  private var latestFinalizedHeight: Int = 0
  private var lastBlockTime: Long = 0L
  
  // İşlem ücretlendirme yöneticisi
  private val feeManager = new FeeManager()
  
  // İmzalama işlemcisi
  private var signatureProvider: Option[(Array[Byte] => Array[Byte])] = None
  
  // İmzalama fonksiyonunu ayarla
  def setSignatureProvider(signingFunction: Array[Byte] => Array[Byte]): Unit = {
    signatureProvider = Some(signingFunction)
  }
  
  // Düğümü imzala
  private def signNode(node: DAGNode): Array[Byte] = {
    signatureProvider match {
      case Some(signer) => signer(node.hash.getBytes)
      case None => Array.emptyByteArray // İmzalama ayarlanmamışsa boş dizi döndür
    }
  }
  
  // İşlemin ücretini hesapla ve geçerliliğini kontrol et
  def validateTransactionFee(tx: Transaction): GasUsageResult = {
    feeManager.calculateFee(tx)
  }
  
  // Yeni bir DAG düğümü oluştur
  def createNode(transactions: Seq[Transaction]): DAGNode = {
    // İşlem ücretlerini doğrula ve yetersiz ücretli işlemleri filtrele
    val validTransactions = transactions.filter { tx =>
      val result = validateTransactionFee(tx)
      result.success
    }
    
    val parentIds = tips.toSet
    val timestamp = Instant.now().getEpochSecond
    val round = latestRound + 1
    
    // Yüksekliği belirle (maksimum ebeveyn yüksekliği + 1)
    val height = if (parentIds.isEmpty) 0 else {
      parentIds.map(id => nodes.getOrElse(id, throw new IllegalStateException(s"Parent node $id not found"))).map(_.height).max + 1
    }
    
    // Önce imzasız düğümü oluştur
    val unsignedNode = DAGNode(
      id = UUID.randomUUID().toString,
      parentIds = parentIds,
      creator = nodeId,
      timestamp = timestamp,
      transactions = validTransactions,
      signature = Array.emptyByteArray,
      height = height,
      round = round
    )
    
    // Düğümü imzala
    val signature = signNode(unsignedNode)
    
    // İmzalı düğümü oluştur
    val signedNode = unsignedNode.copy(signature = signature)
    
    // Yeni düğümü ekle ve uçları güncelle
    addNode(signedNode)
    
    // İşlem ücretlerini dağıt
    processTransactionFees(validTransactions)
    
    signedNode
  }
  
  // İşlem ücretlerini doğrulayıcılara dağıt
  private def processTransactionFees(transactions: Seq[Transaction]): Unit = {
    if (transactions.isEmpty) return
    
    // Tüm işlemlerin toplam ücretini hesapla
    val totalFees = transactions.map { tx =>
      val result = validateTransactionFee(tx)
      result.fee
    }.sum
    
    // Stake ağırlıklarını normalize et
    val validatorWeights = validatorSet.map { case (id, stake) =>
      id -> stake.toDouble
    }
    
    // Ücretleri doğrulayıcılara dağıt
    val distribution = feeManager.distributeFees(totalFees, validatorWeights)
    
    // Gerçek bir uygulamada burada dağıtım işlemi yapılır
    // Burada sadece loglama yapıyoruz
    distribution.foreach { case (validatorId, amount) =>
      println(s"Validator $validatorId received fee: $amount")
    }
  }
  
  // Başka bir doğrulayıcıdan alınan düğümü ekle
  def addNode(node: DAGNode): Boolean = {
    // Temel doğrulamalar
    if (nodes.contains(node.id)) return false
    if (!validateParents(node)) return false
    
    // Düğüm imzasını doğrula (basitleştirilmiş)
    if (!validateSignature(node)) return false
    
    // Düğümü ekle ve uçları güncelle
    nodes(node.id) = node
    updateTips(node)
    
    // Turu ve zamanı güncelle
    latestRound = math.max(latestRound, node.round)
    lastBlockTime = System.currentTimeMillis()
    
    // Kesinleştirmeyi çalıştır
    runFinalization()
    
    true
  }
  
  // İşlem havuzu doluluğunu güncelle (gas fiyatlandırması için)
  private def updateTransactionPoolLoad(node: DAGNode): Unit = {
    // Örnek olarak, düğümdeki işlem sayısını 100 işleme göre normalize ederek doluluk hesaplıyoruz
    val fullnessRatio = math.min(1.0, node.transactions.size / 100.0)
    feeManager.updateNetworkLoad(fullnessRatio)
  }
  
  private def validateParents(node: DAGNode): Boolean = {
    // Genesis bloğu kontrolü
    if (node.parentIds.isEmpty && node.height == 0) {
      return true
    }
    
    // Normal düğümler için ebeveyn kontrolü
    if (node.parentIds.isEmpty) {
      return false
    }
    
    // Tüm ebeveynlerin varlığını kontrol et
    node.parentIds.forall(nodes.contains)
  }
  
  private def validateSignature(node: DAGNode): Boolean = {
    // İmza boş olmamalı
    if (node.signature.isEmpty) {
      return false
    }
    
    // Burada daha kapsamlı bir imza doğrulaması olacak
    true
  }
  
  private def updateTips(node: DAGNode): Unit = {
    // Ebeveynleri uçlardan kaldır
    tips --= node.parentIds
    // Yeni düğümü uç olarak ekle
    tips += node.id
  }
  
  // HotStuff benzeri kesinleştirme algoritması
  private def runFinalization(): Unit = {
    // İki aşamalı kesinleştirme:
    // 1. 2f+1 doğrulayıcı tarafından görülen düğümler
    // 2. 2f+1 doğrulayıcı tarafından dolaylı olarak referans verilen düğümler
    
    // Kesinleştirme algoritması
    // BullShark benzeri bir algoritmalar zinciri:
    
    // 1. Yeni tiplerin yüksekliklerini kontrol et
    val tipsByHeight = tips.toSeq
      .map(id => nodes(id))
      .groupBy(_.height)
      .map { case (height, nodes) => height -> nodes.toSet }
    
    // 2. Bir yükseklikte yeterli düğüm varsa (2f+1), o yüksekliği kesinleştir
    val totalStake = validatorSet.values.sum
    val twoThirdsStake = (totalStake * 2) / 3 + 1 // 2f+1 için gerekli stake
    
    // Kesinleştirilebilecek yeni yükseklikleri bul
    for ((height, heightNodes) <- tipsByHeight if height > latestFinalizedHeight) {
      val validatorStakesAtHeight = heightNodes
        .map(node => validatorSet.getOrElse(node.creator, 0L))
        .sum
      
      if (validatorStakesAtHeight >= twoThirdsStake) {
        // Bu yüksekliği kesinleştir
        latestFinalizedHeight = height
        
        // Kesinleştirme olayını bildir (gerçek uygulamada bir event olarak yayılır)
        println(s"Height $height finalized with ${heightNodes.size} nodes")
      }
    }
  }
  
  // Önerilen gas fiyatını al
  def getRecommendedGasPrice(priority: Boolean = false): GasPrice = {
    feeManager.gasPriceAdjuster.getRecommendedGasPrice(priority)
  }
  
  // İşlem için önerilen ücreti hesapla
  def estimateTransactionFee(tx: Transaction): TransactionFee = {
    val gasEstimation = GasCalculator.calculateTransactionGas(tx)
    feeManager.getRecommendedFee(gasEstimation)
  }
  
  // Doğrulayıcıların cezalandırma koşulları
  case class SlashingConditions(
    doubleSigningPenalty: Double = 1.0,  // Stake'in %100'ü 
    unavailabilityPenalty: Double = 0.1, // Stake'in %10'u
    timeoutBlocks: Int = 100             // Bu kadar blok boyunca cevap gelmezse cezalandır
  )
  
  /**
   * DAG'daki en yüksek düğüm yüksekliğini al
   * @return En yüksek yükseklik
   */
  def getLatestHeight: Int = {
    if (nodes.isEmpty) 0
    else nodes.values.map(_.height).max
  }
  
  /**
   * Son düğüm ekleme zamanını al
   * @return Son düğüm zamanı (UNIX timestamp)
   */
  def getLastBlockTime: Long = lastBlockTime
  
  /**
   * Tüm düğümleri al
   * @return Tüm DAG düğümleri
   */
  def getAllNodes: Seq[DAGNode] = nodes.values.toSeq
  
  /**
   * Belirli bir yükseklikteki düğümleri al
   * @param height Düğüm yüksekliği
   * @return Bu yükseklikteki düğümler
   */
  def getNodesAtHeight(height: Int): Seq[DAGNode] = {
    nodes.values.filter(_.height == height).toSeq
  }
  
  /**
   * Belirli bir düğümü kimliğine göre al
   * @param nodeId Düğüm kimliği
   * @return Bulunan düğüm (yoksa None)
   */
  def getNodeById(nodeId: String): Option[DAGNode] = {
    nodes.get(nodeId)
  }
  
  /**
   * Düğüm seri hale getirme (serileştirme)
   * @param node Seri hale getirilecek düğüm
   * @return Seri hale getirilmiş veri
   */
  def serializeNode(node: DAGNode): Array[Byte] = {
    // Gerçek uygulamada JSON, Protobuf veya başka bir format kullanılabilir
    // Bu basitleştirilmiş bir örnektir
    val parentIdsStr = node.parentIds.mkString(",")
    val txIdsStr = node.transactions.map(_.id).mkString(",")
    
    val nodeStr = s"${node.id}|$parentIdsStr|${node.creator}|${node.timestamp}|$txIdsStr|${new String(node.signature)}|${node.height}|${node.round}"
    nodeStr.getBytes
  }
} 