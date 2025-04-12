package core

import scala.collection.mutable
import scala.concurrent.{Future, ExecutionContext}
import scala.util.{Try, Success, Failure}
import java.security.{KeyPair, MessageDigest}
import java.util.{Timer, TimerTask, UUID}
import java.time.Instant

// İşlem sonucu
case class TransactionResult(
  txId: String,
  status: Boolean,
  gasUsed: Long,
  fee: Long,
  error: Option[String] = None,
  blockHeight: Option[Long] = None,
  timestamp: Long = System.currentTimeMillis()
)

// Blockchain uygulamasının ana sınıfı
class Blockchain(
  val nodeId: String,
  val keyPair: KeyPair,
  val endpoint: String,
  val bootstrapNodes: Seq[String] = Seq.empty,
  val executionContext: ExecutionContext
)(implicit val ec: ExecutionContext = executionContext) {

  // Bileşenler
  val networkNode = new NetworkNode(nodeId, endpoint, keyPair, bootstrapNodes)
  val objectStore = networkNode.objectStore
  val dagConsensus = networkNode.dagConsensus
  val zkSystem = networkNode.zkSystem
  val stakingSystem = new PoSStakingSystem()
  val feeManager = networkNode.feeManager
  
  // Ödül sistemi
  val rewardSystem = RewardSystem.create(stakingSystem)
  
  // Blok senkronizasyon bileşeni
  val blockSynchronizer = BlockSynchronizer.create(nodeId, dagConsensus, networkNode, executionContext)
  
  // Blockchain durum bilgisi
  private var currentHeight: Long = 0
  private var currentRound: Int = 0
  private var lastBlockTime: Long = System.currentTimeMillis()
  
  // İşlem sonuçları takibi
  private val transactionResults = mutable.Map[String, TransactionResult]()
  
  // Bakiye takibi için basit bir harita
  private val balances = mutable.Map[String, Long]()
  
  // Zamanlayıcı
  private val timer = new Timer(true)
  
  // Genesis durumu
  private var isInitialized = false
  
  // Zinciri başlat
  def start(): Unit = {
    if (!isInitialized) {
      initializeGenesis()
    }
    
    // Ağ düğümünü başlat
    networkNode.start()
    
    // Periyodik görevleri zamanla
    scheduleBlockProduction()
    scheduleStakingUpdates()
    
    println(s"Blockchain node $nodeId started")
    
    // Blok senkronizasyonunu başlat
    if (bootstrapNodes.nonEmpty) {
      println("Starting block synchronization with network...")
      blockSynchronizer.start(fastSyncMode = true)
    }
  }
  
  // Genesis durumunu oluştur
  private def initializeGenesis(): Unit = {
    if (isInitialized) return
    
    val genesisTime = System.currentTimeMillis()
    lastBlockTime = genesisTime
    
    // Genesis validatörünü oluştur
    if (bootstrapNodes.isEmpty) { // Bu bir bootnode ise
      val genesisValidatorId = nodeId
      val initialStake = 1000000L
      
      stakingSystem.registerValidator(
        id = genesisValidatorId,
        name = "Genesis Validator",
        publicKey = keyPair.getPublic.getEncoded,
        initialStake = initialStake,
        commissionRate = 0.05,
        currentTime = genesisTime,
        currentHeight = 0
      )
      
      // Genesis hesabına bakiye ekle
      balances(nodeId) = 10000000L // 10M token
      
      // Hazine adresini ayarla
      rewardSystem.setTreasuryAddress(s"treasury-$nodeId")
    }
    
    // Genesis işlemi oluştur
    val gasPrice = GasPrice(1, 0) // çok düşük gas fiyatı
    val genesisFee = TransactionFee(21000, gasPrice, 1)
    
    val genesisTx = Transaction(
      id = "genesis-tx",
      sender = nodeId,
      objectRefs = Seq.empty,
      data = "Genesis Block".getBytes,
      zkProof = None,
      fee = genesisFee,
      nonce = 0
    )
    
    // Genesis işlem sonucunu kaydet
    val genesisResult = TransactionResult(
      txId = genesisTx.id,
      status = true,
      gasUsed = 21000,
      fee = 21000, // 21000 * 1
      blockHeight = Some(0),
      timestamp = genesisTime
    )
    transactionResults(genesisTx.id) = genesisResult
    
    // Genesis DAG düğümünü oluştur
    val genesisNode = dagConsensus.createNode(Seq(genesisTx))
    
    // Genesis nesnesini oluştur
    val genesisObject = objectStore.createObject(
      creator = nodeId,
      typeTag = "system.GenesisConfig",
      data = s"""
        {
          "networkId": "zk-dag-pos-network-v1",
          "genesisTime": $genesisTime,
          "initialValidators": ["$nodeId"]
        }
      """.getBytes,
      owners = Set(nodeId),
      isShared = true,
      isMutable = false
    )
    
    currentHeight = 1
    currentRound = 0
    isInitialized = true
  }
  
  // Blok üretimi zamanla
  private def scheduleBlockProduction(): Unit = {
    val blockInterval = 5000 // 5 saniye (test için)
    
    val task = new TimerTask {
      override def run(): Unit = {
        tryProduceBlock()
      }
    }
    
    timer.schedule(task, blockInterval, blockInterval)
  }
  
  // Staking güncellemelerini zamanla
  private def scheduleStakingUpdates(): Unit = {
    val stakingUpdateInterval = 60000 // 1 dakika
    
    val task = new TimerTask {
      override def run(): Unit = {
        stakingSystem.processUnstaking(System.currentTimeMillis())
      }
    }
    
    timer.schedule(task, stakingUpdateInterval, stakingUpdateInterval)
  }
  
  // Blok üretmeyi dene
  private def tryProduceBlock(): Unit = {
    // Bu düğüm bu tur için önerici mi kontrol et
    val currentTime = System.currentTimeMillis()
    val roundSeed = s"$currentRound-$currentHeight-${lastBlockTime}".getBytes
    
    stakingSystem.selectProposer(roundSeed, currentTime) match {
      case Some(proposerId) if proposerId == nodeId =>
        // Bu düğüm bu tur için seçildi, blok oluştur
        produceBlock()
        
      case Some(_) =>
        // Başka bir düğüm seçildi, bekle
        // Timeout kontrolü yap
        if (currentTime - lastBlockTime > 15000) { // 15 saniye timeout
          // Timeout gerçekleşti, yeni tur başlat
          currentRound += 1
        }
        
      case None =>
        // Aktif validator yok, bekle
    }
  }
  
  // Blok oluştur
  private def produceBlock(): Unit = {
    // İşlem havuzundan işlemleri al
    val pendingTransactions = networkNode.getPendingTransactions
    
    // DAG düğümü oluştur
    val node = dagConsensus.createNode(pendingTransactions)
    
    // İşlem sonuçlarını güncelle
    node.transactions.foreach { tx =>
      val gasResult = dagConsensus.validateTransactionFee(tx)
      if (gasResult.success) {
        // İşlem ücretini gönderenden düş
        deductBalance(tx.sender, gasResult.fee)
        
        // İşlem sonucunu kaydet
        val result = TransactionResult(
          txId = tx.id,
          status = true,
          gasUsed = gasResult.gasUsed,
          fee = gasResult.fee,
          blockHeight = Some(currentHeight),
          timestamp = System.currentTimeMillis()
        )
        transactionResults(tx.id) = result
      }
    }
    
    // Dünya durumunu güncelle
    currentHeight += 1
    currentRound = 0
    lastBlockTime = System.currentTimeMillis()
    
    // Gas fiyatını güncelle (blok doluluk oranına göre)
    val blockFullness = math.min(1.0, node.transactions.size / 100.0)
    feeManager.updateNetworkLoad(blockFullness)
    
    // Doğrulayıcıları ve oylamaları al
    val attesters = getBlockAttesters(node)
    
    // Blok üretim ödüllerini dağıt
    val totalReward = rewardSystem.distributeBlockRewards(
      blockProducer = nodeId,
      blockHeight = currentHeight,
      attesters = attesters
    )
    
    // İstatistikleri güncelle
    stakingSystem.validators.get(nodeId).foreach { validator =>
      val updatedStatus = validator.status.copy(
        lastProposedBlock = currentHeight,
        proposedBlockCount = validator.status.proposedBlockCount + 1
      )
      
      stakingSystem.validators(nodeId) = validator.copy(
        status = updatedStatus,
        lastUpdated = lastBlockTime
      )
    }
    
    println(s"Block produced at height $currentHeight by $nodeId with ${node.transactions.size} transactions, reward: $totalReward")
  }
  
  // Blok oluşturucular ve onaylayanları al (simülasyon)
  private def getBlockAttesters(node: DAGNode): Set[String] = {
    // Gerçek uygulamada, bu metot aktif doğrulayıcılardan gelen oy (attestation) 
    // bilgilerini alır. Burada basit bir simülasyon yapıyoruz.
    val activeValidators = stakingSystem.getActiveValidators
      .filter(_.id != nodeId)  // Üretici dışındaki doğrulayıcılar
      .map(_.id)
      .toSet
    
    // Aktif doğrulayıcıların bir kısmı (yaklaşık %80'i) onaylıyor olsun
    val attestationRatio = 0.8
    val attestersCount = Math.max(1, (activeValidators.size * attestationRatio).toInt)
    
    if (activeValidators.isEmpty) {
      Set.empty[String]
    } else {
      scala.util.Random.shuffle(activeValidators.toSeq).take(attestersCount).toSet
    }
  }
  
  // Bakiye azalt
  private def deductBalance(address: String, amount: Long): Boolean = {
    stakingSystem.subtractBalance(address, amount)
  }
  
  // Bakiye ekle
  private def addBalance(address: String, amount: Long): Unit = {
    stakingSystem.addBalance(address, amount)
  }
  
  // Bakiye sorgula
  def getBalance(address: String): Long = {
    stakingSystem.getBalance(address)
  }
  
  // Toplam bakiye (stake dahil)
  def getTotalBalance(address: String): Long = {
    stakingSystem.getTotalBalance(address)
  }
  
  // Ödül geçmişini al
  def getRewardHistory(address: String): Seq[RewardRecord] = {
    rewardSystem.getRewardHistory(address)
  }
  
  // Toplam ödül miktarını al
  def getTotalRewards(address: String): Long = {
    rewardSystem.getTotalRewardsForValidator(address)
  }
  
  // İşlem gönder
  def submitTransaction(
    sender: String,
    objectRefs: Seq[String],
    data: Array[Byte],
    zkProof: Option[Array[Byte]] = None,
    priority: Boolean = false
  ): Either[String, Transaction] = {
    // İşlem oluştur
    val tx = networkNode.createTransaction(
      sender = sender,
      objectRefs = objectRefs,
      data = data,
      zkProof = zkProof,
      priority = priority
    )
    
    // Bakiye kontrolü
    val currentBalance = getBalance(sender)
    if (currentBalance < tx.maxFee) {
      return Left(s"Insufficient balance: $currentBalance < ${tx.maxFee}")
    }
    
    // İşlemi ağa ekle
    val success = networkNode.addTransaction(tx)
    
    if (success) {
      Right(tx)
    } else {
      Left("Transaction validation failed")
    }
  }
  
  // İşlem sonucu sorgula
  def getTransactionResult(txId: String): Option[TransactionResult] = {
    transactionResults.get(txId)
  }
  
  // Gas fiyatı önerisi al
  def getRecommendedGasPrice(priority: Boolean = false): GasPrice = {
    networkNode.getRecommendedGasPrice(priority)
  }
  
  // İşlem ücreti tahmini
  def estimateTransactionFee(
    sender: String,
    objectRefs: Seq[String],
    data: Array[Byte],
    zkProof: Option[Array[Byte]] = None,
    priority: Boolean = false
  ): TransactionFee = {
    // Geçici işlem oluştur
    val tempTx = Transaction(
      id = "estimate-" + UUID.randomUUID().toString,
      sender = sender,
      objectRefs = objectRefs,
      data = data,
      zkProof = zkProof,
      fee = TransactionFee(0, GasPrice(0), 0),
      nonce = 0
    )
    
    // Gas maliyetini hesapla
    val gasEstimation = GasCalculator.calculateTransactionGas(tempTx)
    
    // Tavsiye edilen ücreti döndür
    feeManager.getRecommendedFee(gasEstimation, priority)
  }
  
  // Doğrulayıcı ekle
  def registerAsValidator(
    name: String,
    initialStake: Long,
    commissionRate: Double
  ): Either[String, Validator] = {
    // Bakiye kontrolü
    if (getBalance(nodeId) < initialStake) {
      return Left(s"Insufficient balance: ${getBalance(nodeId)} < $initialStake")
    }
    
    stakingSystem.registerValidator(
      id = nodeId,
      name = name,
      publicKey = keyPair.getPublic.getEncoded,
      initialStake = initialStake,
      commissionRate = commissionRate,
      currentTime = System.currentTimeMillis(),
      currentHeight = currentHeight
    ).map { validator =>
      // Stake miktarını bakiyeden düş
      deductBalance(nodeId, initialStake)
      validator
    }
  }
  
  // Stake ekle
  def stake(
    validatorId: String,
    amount: Long
  ): Either[String, Stake] = {
    // Bakiye kontrolü
    if (getBalance(nodeId) < amount) {
      return Left(s"Insufficient balance: ${getBalance(nodeId)} < $amount")
    }
    
    stakingSystem.addStake(
      stakerId = nodeId,
      validatorId = validatorId,
      amount = amount,
      currentTime = System.currentTimeMillis(),
      currentHeight = currentHeight
    ).map { stake =>
      // Stake miktarını bakiyeden düş
      deductBalance(nodeId, amount)
      stake
    }
  }
  
  // Stake çekme talebi oluştur
  def unstake(stakeId: String): Either[String, Long] = {
    stakingSystem.requestUnstake(
      stakerId = nodeId,
      stakeId = stakeId,
      currentTime = System.currentTimeMillis()
    )
  }
  
  // Nesne oluştur
  def createObject(
    typeTag: String,
    data: Array[Byte],
    isShared: Boolean = false,
    isMutable: Boolean = true
  ): ObjectData = {
    objectStore.createObject(
      creator = nodeId,
      typeTag = typeTag,
      data = data,
      owners = Set(nodeId),
      isShared = isShared,
      isMutable = isMutable
    )
  }
  
  // Nesne güncelle
  def updateObject(id: String, updateFn: ObjectData => Array[Byte]): Option[ObjectData] = {
    objectStore.updateObject(id, updateFn)
  }
  
  // Nesne sahipliğini transfer et
  def transferObject(id: String, newOwner: String): Option[ObjectData] = {
    objectStore.transferObject(id, newOwner)
  }
  
  // Zincir bilgilerini al
  def getChainInfo: Map[String, Any] = {
    val syncStatus = blockSynchronizer.getSyncStats
    
    Map(
      "nodeId" -> nodeId,
      "currentHeight" -> currentHeight,
      "currentRound" -> currentRound,
      "lastBlockTime" -> lastBlockTime,
      "activeValidators" -> stakingSystem.getActiveValidators.size,
      "connectedPeers" -> networkNode.peers.size,
      "pendingTransactions" -> networkNode.getPendingTransactions.size,
      "currentGasPrice" -> getRecommendedGasPrice().basePrice,
      "balance" -> getBalance(nodeId),
      "totalBalance" -> getTotalBalance(nodeId),
      "totalRewards" -> getTotalRewards(nodeId),
      "totalStake" -> stakingSystem.getTotalStake,
      "syncStatus" -> syncStatus.status.toString,
      "syncProgress" -> syncStatus.progressPercentage
    )
  }
  
  /**
   * Senkronizasyon durumunu kontrol eder
   * @return Düğüm senkronize ise true
   */
  def isSynchronized: Boolean = blockSynchronizer.isSynced
  
  /**
   * Blok senkronizasyonu durumunu alır
   * @return Senkronizasyon istatistikleri
   */
  def getSyncStatus: SyncStats = blockSynchronizer.getSyncStats
}

// Test kodu
object BlockchainTest {
  def main(args: Array[String]): Unit = {
    // KeyPair oluştur
    val keyPair = KeyPairUtils.generateKeyPair()
    
    // Blockchain düğümünü başlat
    implicit val ec = scala.concurrent.ExecutionContext.global
    
    val blockchain = new Blockchain(
      nodeId = "node-1",
      keyPair = keyPair,
      endpoint = "127.0.0.1:9000",
      bootstrapNodes = Seq.empty,
      executionContext = ec
    )
    
    blockchain.start()
    
    // Temel bilgileri göster
    println(s"Blockchain node started with info:")
    blockchain.getChainInfo.foreach { case (key, value) => 
      println(s"  $key: $value") 
    }
    
    // İşlem oluştur ve gönder
    val txData = "Test transaction".getBytes
    val txFee = blockchain.estimateTransactionFee(
      sender = blockchain.nodeId,
      objectRefs = Seq.empty,
      data = txData,
      priority = true
    )
    
    println(s"Estimated fee: ${txFee.estimatedFee} (limit: ${txFee.gasLimit}, price: ${txFee.gasPrice.total})")
    
    blockchain.submitTransaction(
      sender = blockchain.nodeId,
      objectRefs = Seq.empty,
      data = txData,
      priority = true
    ) match {
      case Right(tx) => 
        println(s"Transaction ${tx.id} submitted successfully")
      case Left(error) => 
        println(s"Transaction submission failed: $error")
    }
  }
}

// Yardımcı sınıflar
object KeyPairUtils {
  def generateKeyPair(): KeyPair = {
    val keyGen = java.security.KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(2048)
    keyGen.generateKeyPair()
  }
} 