package core

import scala.collection.mutable
import scala.concurrent.{Future, Promise, ExecutionContext}
import scala.util.{Try, Success, Failure}
import java.util.concurrent.{ScheduledExecutorService, Executors, TimeUnit}
import java.time.Instant

/**
 * Senkronizasyon durumu
 */
object SyncStatus extends Enumeration {
  type SyncStatus = Value
  val NotSynced, Syncing, FastSync, FullSync, Error = Value
}

import SyncStatus._

/**
 * Senkronizasyon istatistikleri
 * @param startTime Senkronizasyonun başlangıç zamanı
 * @param currentHeight Mevcut yükseklik
 * @param targetHeight Hedef yükseklik
 * @param downloadedNodes İndirilen düğüm sayısı
 * @param validatedNodes Doğrulanan düğüm sayısı
 * @param processedTransactions İşlenen işlem sayısı
 * @param status Senkronizasyon durumu
 * @param syncSpeed Saniyedeki senkronizasyon hızı (düğüm/sn)
 * @param estimatedTimeRemaining Tahmini kalan süre (saniye)
 * @param lastError Son hata mesajı
 */
case class SyncStats(
  startTime: Long = Instant.now.getEpochSecond,
  currentHeight: Long = 0L,
  targetHeight: Long = 0L,
  downloadedNodes: Long = 0L,
  validatedNodes: Long = 0L,
  processedTransactions: Long = 0L,
  status: SyncStatus = NotSynced,
  syncSpeed: Double = 0.0,
  estimatedTimeRemaining: Long = 0L,
  lastError: Option[String] = None
) {
  def progressPercentage: Double = 
    if (targetHeight == 0) 0.0 
    else Math.min(100.0, (currentHeight.toDouble / targetHeight.toDouble) * 100.0)
  
  def elapsedTime: Long = Instant.now.getEpochSecond - startTime
}

/**
 * Blok senkronizasyon mekanizması
 * @param nodeId Yerel düğüm kimliği
 * @param dagConsensus DAG konsensus mekanizması
 * @param networkNode Ağ düğümü
 * @param executionContext Yürütme bağlamı
 */
class BlockSynchronizer(
  val nodeId: String,
  val dagConsensus: DAGConsensus,
  val networkNode: NetworkNode,
  val executionContext: ExecutionContext
)(implicit val ec: ExecutionContext = executionContext) {
  import SyncStatus._
  
  // Senkronizasyon durumu
  private var syncStats = SyncStats()
  
  // Zamanlayıcı
  private val scheduler: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  
  // İndirilen düğümler
  private val downloadedNodes = mutable.Map[String, DAGNode]()
  
  // İndirme kuyruğu (id -> height)
  private val downloadQueue = mutable.PriorityQueue[(String, Int)]()(Ordering.by(_._2))
  
  // İşlenmiş düğümler
  private val processedNodeIds = mutable.Set[String]()
  
  // Son görülen yükseklik
  private var highestSeenHeight: Int = 0
  
  // Senkronizasyon stratejisi (hızlı veya tam)
  private var fastSync: Boolean = true
  
  // Aktif eşler
  private val activePeers = mutable.Map[String, Long]() // peer -> last seen time
  
  // Senkronizasyon çalıştırıcı
  @volatile private var isRunning = false
  
  /**
   * Senkronizasyonu başlatır
   * @param fastSyncMode Hızlı senkronizasyon modu aktif olsun mu?
   */
  def start(fastSyncMode: Boolean = true): Unit = {
    if (isRunning) return
    
    isRunning = true
    fastSync = fastSyncMode
    
    // İstatistikleri sıfırla
    syncStats = SyncStats(
      status = Syncing,
      startTime = Instant.now.getEpochSecond
    )
    
    // Görevi zamanla
    scheduler.scheduleWithFixedDelay(
      new Runnable { 
        def run(): Unit = synchronizationCycle() 
      },
      0, 5, TimeUnit.SECONDS
    )
    
    println(s"[BlockSynchronizer] Blok senkronizasyonu başlatıldı (mod: ${if (fastSync) "Hızlı" else "Tam"})")
  }
  
  /**
   * Senkronizasyonu durdurur
   */
  def stop(): Unit = {
    isRunning = false
    scheduler.shutdown()
    syncStats = syncStats.copy(status = NotSynced)
    println("[BlockSynchronizer] Blok senkronizasyonu durduruldu")
  }
  
  /**
   * Senkronizasyon döngüsü
   */
  private def synchronizationCycle(): Unit = {
    try {
      if (!isRunning) return
      
      // 1. Aktif eşleri güncelle
      updateActivePeers()
      
      // 2. En son blok yüksekliğini al
      discoverLatestBlockHeight()
      
      // 3. Eksik blokları belirle
      identifyMissingBlocks()
      
      // 4. Eksik blokları indir
      downloadMissingBlocks()
      
      // 5. İndirilen blokları işle
      processDownloadedBlocks()
      
      // 6. İstatistikleri güncelle
      updateSyncStats()
      
      // 7. Senkronizasyon durumunu kontrol et
      checkSyncStatus()
    } catch {
      case e: Exception => 
        println(s"[BlockSynchronizer] Senkronizasyon hatası: ${e.getMessage}")
        syncStats = syncStats.copy(
          status = Error,
          lastError = Some(e.getMessage)
        )
    }
  }
  
  /**
   * Aktif eşleri günceller
   */
  private def updateActivePeers(): Unit = {
    val currentTime = Instant.now.getEpochSecond
    
    // Zaman aşımına uğramış eşleri temizle (300 saniye = 5 dakika)
    activePeers.filterInPlace((_, lastSeen) => currentTime - lastSeen < 300)
    
    // Yeni eşleri ekle
    networkNode.peers.foreach { peer =>
      activePeers(peer) = currentTime
    }
    
    // Eş yoksa uyarı ver
    if (activePeers.isEmpty) {
      println("[BlockSynchronizer] Uyarı: Aktif eş bulunamadı")
    }
  }
  
  /**
   * En son blok yüksekliğini keşfeder
   */
  private def discoverLatestBlockHeight(): Unit = {
    if (activePeers.isEmpty) return
    
    // Eşlerden en son yükseklik bilgisini iste
    val heightRequests = activePeers.keys.map { peer =>
      networkNode.requestLatestHeight(peer)
    }
    
    // En yüksek değeri bul
    val heights = heightRequests.flatten
    if (heights.nonEmpty) {
      val maxHeight = heights.max
      if (maxHeight > highestSeenHeight) {
        highestSeenHeight = maxHeight
        syncStats = syncStats.copy(targetHeight = maxHeight)
        println(s"[BlockSynchronizer] Yeni hedef yükseklik: $maxHeight")
      }
    }
  }
  
  /**
   * Eksik blokları belirler
   */
  private def identifyMissingBlocks(): Unit = {
    if (highestSeenHeight <= syncStats.currentHeight) return
    
    // Mevcut yükseklik ile hedef yükseklik arasındaki eksik blokları belirle
    val currentNodes = dagConsensus.getAllNodes
    val currentHeightMap = currentNodes.groupBy(_.height)
    
    // Sadece eksik yükseklikleri kontrol et (bellek tasarrufu için)
    val startHeight = syncStats.currentHeight.toInt + 1
    val endHeight = if (fastSync && highestSeenHeight > 1000) {
      // Hızlı senkronizasyon modunda, son 1000 bloğu tam olarak senkronize et
      highestSeenHeight - 1000 
    } else {
      // Tam senkronizasyon modunda, tüm blokları senkronize et
      1
    }
    
    // Eksik blokları kuyruğa ekle
    for (height <- startHeight to highestSeenHeight) {
      // Bu yükseklikteki mevcut düğümleri al
      val nodesAtHeight = currentHeightMap.getOrElse(height, Seq.empty)
      
      if (nodesAtHeight.isEmpty) {
        // Bu yükseklikte hiç düğüm yoksa, eşlerden düğümleri iste
        val discoveredNodes = requestNodesAtHeight(height)
        
        // Bulunan düğümleri kuyruğa ekle
        discoveredNodes.foreach { nodeId =>
          if (!processedNodeIds.contains(nodeId) && !downloadQueue.exists(_._1 == nodeId)) {
            downloadQueue.enqueue((nodeId, height))
          }
        }
      } else if (height % 100 == 0) {
        // Her 100 yükseklikte bir, eksik düğümleri kontrol et (eksik düğüm olabilir)
        val discoveredNodes = requestNodesAtHeight(height)
        val missingNodes = discoveredNodes.filterNot(id => 
          nodesAtHeight.exists(_.id == id) || 
          processedNodeIds.contains(id) || 
          downloadQueue.exists(_._1 == id)
        )
        
        // Eksik düğümleri kuyruğa ekle
        missingNodes.foreach { nodeId =>
          downloadQueue.enqueue((nodeId, height))
        }
      }
    }
    
    // Toplamda kaç düğüm kuyruğa alındı
    println(s"[BlockSynchronizer] İndirme kuyruğuna ${downloadQueue.size} düğüm eklendi")
  }
  
  /**
   * Belirli bir yükseklikteki düğümleri eşlerden ister
   * @param height Düğüm yüksekliği
   * @return Düğüm kimlikleri listesi
   */
  private def requestNodesAtHeight(height: Int): Seq[String] = {
    if (activePeers.isEmpty) return Seq.empty
    
    // Rastgele bir eşten düğümleri iste
    val randomPeers = scala.util.Random.shuffle(activePeers.keys.toSeq).take(3)
    
    randomPeers.flatMap { peer =>
      networkNode.requestNodeIdsAtHeight(peer, height)
    }.distinct
  }
  
  /**
   * Eksik blokları indirir
   */
  private def downloadMissingBlocks(): Unit = {
    if (downloadQueue.isEmpty) return
    
    // Maksimum indirme sayısı (paralel indirme)
    val maxDownloads = 50
    val nodesToDownload = downloadQueue.dequeueAll.take(maxDownloads)
    
    if (nodesToDownload.nonEmpty) {
      println(s"[BlockSynchronizer] ${nodesToDownload.size} düğüm indiriliyor...")
      
      // Tüm düğümleri indir
      nodesToDownload.foreach { case (nodeId, height) =>
        if (!downloadedNodes.contains(nodeId) && !processedNodeIds.contains(nodeId)) {
          downloadNode(nodeId, height)
        }
      }
    }
  }
  
  /**
   * Belirli bir düğümü indirir
   * @param nodeId Düğüm kimliği
   * @param height Düğüm yüksekliği
   */
  private def downloadNode(nodeId: String, height: Int): Unit = {
    if (activePeers.isEmpty) return
    
    // Rastgele bir eşten düğümü iste
    val randomPeers = scala.util.Random.shuffle(activePeers.keys.toSeq).take(3)
    
    randomPeers.find { peer =>
      networkNode.requestNode(peer, nodeId) match {
        case Some(node) =>
          // Düğümü doğrula ve kaydet
          if (validateNode(node)) {
            downloadedNodes(nodeId) = node
            syncStats = syncStats.copy(downloadedNodes = syncStats.downloadedNodes + 1)
            true
          } else {
            false
          }
        case None => false
      }
    }
  }
  
  /**
   * İndirilen düğümleri işler
   */
  private def processDownloadedBlocks(): Unit = {
    if (downloadedNodes.isEmpty) return
    
    // İşlenecek düğümleri yüksekliğe göre sırala
    val nodesToProcess = downloadedNodes.values.toSeq.sortBy(_.height)
    
    // Düğümleri işle
    var processedCount = 0
    var txCount = 0
    
    nodesToProcess.foreach { node =>
      if (!processedNodeIds.contains(node.id)) {
        // Düğümü DAG'a ekle
        if (dagConsensus.addNode(node)) {
          processedNodeIds.add(node.id)
          processedCount += 1
          txCount += node.transactions.size
          
          // En yüksek işlenen yüksekliği güncelle
          if (node.height > syncStats.currentHeight) {
            syncStats = syncStats.copy(currentHeight = node.height)
          }
          
          // İşlenen işlem sayısını güncelle
          syncStats = syncStats.copy(
            validatedNodes = syncStats.validatedNodes + 1,
            processedTransactions = syncStats.processedTransactions + node.transactions.size
          )
        }
      }
    }
    
    // İşlenen düğümleri temizle
    processedNodeIds.foreach(id => downloadedNodes.remove(id))
    
    if (processedCount > 0) {
      println(s"[BlockSynchronizer] $processedCount düğüm işlendi (${txCount} işlem), " +
        s"ilerleme: ${syncStats.progressPercentage.toInt}%")
    }
  }
  
  /**
   * Senkronizasyon istatistiklerini günceller
   */
  private def updateSyncStats(): Unit = {
    // Geçen süre (saniye)
    val elapsedTime = syncStats.elapsedTime
    if (elapsedTime == 0) return
    
    // Senkronizasyon hızı (düğüm/saniye)
    val syncSpeed = syncStats.validatedNodes.toDouble / elapsedTime.toDouble
    
    // Kalan düğüm sayısı
    val remainingNodes = highestSeenHeight - syncStats.currentHeight
    
    // Tahmini kalan süre
    val estimatedTimeRemaining = 
      if (syncSpeed <= 0 || remainingNodes <= 0) 0L
      else (remainingNodes.toDouble / syncSpeed).toLong
    
    // İstatistikleri güncelle
    syncStats = syncStats.copy(
      syncSpeed = syncSpeed,
      estimatedTimeRemaining = estimatedTimeRemaining
    )
  }
  
  /**
   * Senkronizasyon durumunu kontrol eder
   */
  private def checkSyncStatus(): Unit = {
    if (!isRunning) return
    
    // Senkronizasyon tamamlandı mı?
    if (syncStats.currentHeight >= highestSeenHeight && highestSeenHeight > 0) {
      val newStatus = if (fastSync) FastSync else FullSync
      
      syncStats = syncStats.copy(status = newStatus)
      
      println(s"[BlockSynchronizer] Senkronizasyon tamamlandı (${syncStats.status})")
      println(s"[BlockSynchronizer] İndirilen düğümler: ${syncStats.downloadedNodes}")
      println(s"[BlockSynchronizer] İşlenen düğümler: ${syncStats.validatedNodes}")
      println(s"[BlockSynchronizer] İşlenen işlemler: ${syncStats.processedTransactions}")
      println(s"[BlockSynchronizer] Toplam süre: ${syncStats.elapsedTime} saniye")
      
      // Hızlı senkronizasyondan tam senkronizasyona geçiş
      if (fastSync && newStatus == FastSync) {
        // Burası sadece hızlı senkronizasyon tamamlandığında çalışır
        // İsteğe bağlı olarak tam senkronizasyona geçilebilir
        // fastSync = false
        // syncStats = syncStats.copy(status = Syncing)
        // println("[BlockSynchronizer] Tam senkronizasyon moduna geçiliyor...")
      } else {
        // Senkronizasyonu durdur
        // stop()
      }
    }
  }
  
  /**
   * Düğümü doğrular
   * @param node Doğrulanacak düğüm
   * @return Düğüm geçerliyse true
   */
  private def validateNode(node: DAGNode): Boolean = {
    // Temel doğrulama kontrolleri
    
    // 1. Düğüm kimliği kontrolü
    if (node.id.isEmpty) return false
    
    // 2. Ebeveyn kontrolü
    if (node.parentIds.isEmpty && node.height > 0) return false
    
    // 3. İmza kontrolü (gerçekte daha karmaşık)
    if (node.signature.isEmpty) return false
    
    // 4. Düğüm yüksekliği kontrolü
    if (node.height < 0) return false
    
    // 5. İşlem kontrolü
    if (node.transactions.exists(tx => tx.id.isEmpty || tx.creator.isEmpty)) return false
    
    // Diğer validasyonlar burada yapılabilir
    true
  }
  
  /**
   * Senkronizasyon istatistiklerini alır
   * @return Senkronizasyon istatistikleri
   */
  def getSyncStats: SyncStats = syncStats
  
  /**
   * Senkronizasyon durumunu alır
   * @return Senkronizasyon durumu
   */
  def isSynced: Boolean = 
    syncStats.status == FastSync || syncStats.status == FullSync
}

/**
 * Blok senkronizasyon yardımcı nesnesi
 */
object BlockSynchronizer {
  /**
   * Creates a new BlockSynchronizer
   *
   * @param nodeId Node ID
   * @param dagConsensus DAG consensus mechanism
   * @param networkNode Network node
   * @param executionContext Execution context
   * @return A new BlockSynchronizer instance
   */
  def create(
    nodeId: String,
    dagConsensus: DAGConsensus,
    networkNode: NetworkNode,
    executionContext: ExecutionContext
  ): BlockSynchronizer = {
    new BlockSynchronizer(nodeId, dagConsensus, networkNode, executionContext)
  }
} 