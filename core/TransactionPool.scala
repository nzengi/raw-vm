package core

import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import java.time.Instant
import java.util.concurrent.{PriorityBlockingQueue, Executors, TimeUnit}
import java.util.Comparator
import scala.concurrent.duration._

/**
 * İşlem durumu
 */
object TxStatus extends Enumeration {
  type TxStatus = Value
  
  // İşlem havuza eklendi ama işlenmedi
  val Pending = Value(1, "Pending")
  
  // İşlem belirli blokta işlendi
  val Confirmed = Value(2, "Confirmed")
  
  // İşlem işlenme sırasında hata aldı
  val Failed = Value(3, "Failed")
  
  // İşlem havuzdan düştü (timeout veya başka nedenle)
  val Dropped = Value(4, "Dropped")
}

import TxStatus._

/**
 * İşlem grup türleri
 */
object TxGroupType extends Enumeration {
  type TxGroupType = Value
  
  // Yüksek öncelikli
  val HighPriority = Value(0, "HighPriority")
  
  // Normal
  val Normal = Value(1, "Normal")
  
  // Düşük öncelikli
  val LowPriority = Value(2, "LowPriority")
}

import TxGroupType._

/**
 * İşlem havuzu girişi
 * @param transaction İşlem
 * @param addedTime   Havuza eklenme zamanı
 * @param status      İşlem durumu
 * @param groupType   İşlem grubu
 * @param attempts    İşlem deneme sayısı
 */
case class PoolEntry(
  transaction: Transaction,
  addedTime: Long = Instant.now.getEpochSecond,
  var status: TxStatus = Pending,
  var groupType: TxGroupType = Normal,
  var attempts: Int = 0
) {
  // Eklendikten beri geçen süre (saniye)
  def waitTime(currentTime: Long = Instant.now.getEpochSecond): Long = currentTime - addedTime
}

/**
 * İşlem öncelik stratejisi
 */
trait TxPriorityStrategy {
  /**
   * İki işlem arasındaki öncelik sıralamasını belirle
   * @param tx1 Birinci işlem
   * @param tx2 İkinci işlem
   * @return -1: tx1 öncelikli, 0: eşit, 1: tx2 öncelikli
   */
  def compare(tx1: PoolEntry, tx2: PoolEntry): Int
}

/**
 * Ücret bazlı sıralama
 */
class FeeBasedPriority extends TxPriorityStrategy {
  override def compare(tx1: PoolEntry, tx2: PoolEntry): Int = {
    // Önce grup tipine göre sırala
    val groupComparison = tx1.groupType.id.compareTo(tx2.groupType.id)
    if (groupComparison != 0) return groupComparison
    
    // Grup aynıysa, gas fiyatına göre sırala (yüksek fiyat önce)
    val gasPriceComparison = tx2.transaction.fee.gasPrice.total.compareTo(tx1.transaction.fee.gasPrice.total)
    if (gasPriceComparison != 0) return gasPriceComparison
    
    // Gas fiyatı aynıysa, bekleme süresine göre sırala (uzun bekleyen önce)
    tx2.waitTime().compareTo(tx1.waitTime())
  }
}

/**
 * Bekleme süresine göre sıralama
 */
class WaitTimeBasedPriority extends TxPriorityStrategy {
  override def compare(tx1: PoolEntry, tx2: PoolEntry): Int = {
    // Önce grup tipine göre sırala
    val groupComparison = tx1.groupType.id.compareTo(tx2.groupType.id)
    if (groupComparison != 0) return groupComparison
    
    // Grup aynıysa, bekleme süresine göre sırala (uzun bekleyen önce)
    val waitTimeComparison = tx2.waitTime().compareTo(tx1.waitTime())
    if (waitTimeComparison != 0) return waitTimeComparison
    
    // Bekleme süresi aynıysa, gas fiyatına göre sırala (yüksek fiyat önce)
    tx2.transaction.fee.gasPrice.total.compareTo(tx1.transaction.fee.gasPrice.total)
  }
}

/**
 * Hibrit öncelik stratejisi
 */
class HybridPriorityStrategy extends TxPriorityStrategy {
  // Gas fiyatı ağırlığı (0-1 arası)
  private val gasPriceWeight = 0.7
  
  // Bekleme süresi ağırlığı (0-1 arası)
  private val waitTimeWeight = 0.3
  
  // Maksimum bekleme süresi (saniye) - normalizasyon için
  private val maxWaitTime = 3600L // 1 saat
  
  override def compare(tx1: PoolEntry, tx2: PoolEntry): Int = {
    // Önce grup tipine göre sırala
    val groupComparison = tx1.groupType.id.compareTo(tx2.groupType.id)
    if (groupComparison != 0) return groupComparison
    
    // Grup aynıysa, hibrit skora göre sırala
    val score1 = calculateScore(tx1)
    val score2 = calculateScore(tx2)
    
    score2.compareTo(score1) // Yüksek skor önce
  }
  
  // İşlem için hibrit skor hesapla
  private def calculateScore(entry: PoolEntry): Double = {
    // Gas fiyatı skoru (normalizasyon için bir üst limit kullan)
    val maxGasPrice = 1000.0 // Normalizasyon için maksimum gas fiyatı
    val gasPriceScore = Math.min(entry.transaction.fee.gasPrice.total / maxGasPrice, 1.0) * gasPriceWeight
    
    // Bekleme süresi skoru (maksimum 1 saat)
    val waitTimeScore = Math.min(entry.waitTime().toDouble / maxWaitTime, 1.0) * waitTimeWeight
    
    // Toplam skor
    gasPriceScore + waitTimeScore
  }
}

/**
 * İşlem havuzu grup yöneticisi
 * @param maxSize Maksimum havuz boyutu
 */
class TransactionPool(val maxSize: Int = 10000) {
  // Öncelik stratejisi
  private var priorityStrategy: TxPriorityStrategy = new HybridPriorityStrategy()
  
  // Öncelik sıralaması için kıyaslayıcı
  private val entryComparator = new Comparator[PoolEntry] {
    override def compare(e1: PoolEntry, e2: PoolEntry): Int = {
      priorityStrategy.compare(e1, e2)
    }
  }
  
  // İşlem havuzu (öncelik sıralı)
  private val txQueue = new PriorityBlockingQueue[PoolEntry](maxSize, entryComparator)
  
  // İşlem kimliği -> İşlem havuzu girişi eşlemesi
  private val txMap = mutable.Map[String, PoolEntry]()
  
  // İşlem gönderen -> İşlem sayısı (aynı adresten gelen çok fazla işlemi sınırlamak için)
  private val senderCounts = mutable.Map[String, Int]()
  
  // Zamanlayıcı
  private val scheduler = Executors.newSingleThreadScheduledExecutor()
  
  // İşlem havuz yaşlandırma ayarları
  private val txStaleTimeSeconds = 3600 // 1 saat
  private val maxAttemptsBeforeDrop = 5
  
  // İstatistikler
  private var totalProcessedTx = 0L
  private var totalDroppedTx = 0L
  private var totalConfirmedTx = 0L
  private var totalRejectedTx = 0L
  
  // Grup boyutları
  private var maxGroupSize = maxSize / 3
  private var currentLoad = 0.0
  
  // İşlem onaylama callback fonksiyonu
  private var confirmCallback: Option[(Transaction, Long) => Unit] = None
  
  // Havuzu başlat
  def start(): Unit = {
    // Periyodik temizlik
    scheduler.scheduleWithFixedDelay(
      new Runnable { def run(): Unit = cleanupStaleTransactions() },
      60, 60, TimeUnit.SECONDS
    )
    
    // Periyodik yeniden gruplandırma
    scheduler.scheduleWithFixedDelay(
      new Runnable { def run(): Unit = regroupTransactions() },
      300, 300, TimeUnit.SECONDS
    )
    
    println(s"Transaction pool started with capacity: $maxSize")
  }
  
  // Havuzu durdur
  def stop(): Unit = {
    scheduler.shutdown()
    println("Transaction pool stopped")
  }
  
  /**
   * İşlem onaylama geri çağrı fonksiyonunu ayarla
   * @param callback Callback fonksiyonu
   */
  def setConfirmCallback(callback: (Transaction, Long) => Unit): Unit = {
    confirmCallback = Some(callback)
  }
  
  /**
   * Öncelik stratejisini ayarla
   * @param strategy Öncelik stratejisi
   */
  def setPriorityStrategy(strategy: TxPriorityStrategy): Unit = {
    priorityStrategy = strategy
    
    // Sıralamayı yenile
    refreshPriorities()
  }
  
  /**
   * Havuz yük durumunu güncelle (0.0-1.0)
   * @param loadFactor Yük faktörü
   */
  def updateLoadFactor(loadFactor: Double): Unit = {
    currentLoad = Math.min(1.0, Math.max(0.0, loadFactor))
    
    // Grup boyutlarını yük durumuna göre düzenle
    if (currentLoad < 0.5) {
      // Hafif yük - daha dengeli dağıtım
      maxGroupSize = maxSize / 3
    } else if (currentLoad < 0.8) {
      // Orta yük - yüksek öncelikli işlemlere daha fazla alan
      maxGroupSize = maxSize / 4
    } else {
      // Ağır yük - sadece yüksek öncelikli işlemlere odaklan
      maxGroupSize = maxSize / 5
    }
  }
  
  /**
   * İşlemi havuza ekle
   * @param tx İşlem
   * @return Başarılı mı?
   */
  def addTransaction(tx: Transaction): Boolean = {
    if (txMap.contains(tx.id)) {
      return false // Zaten mevcut
    }
    
    // Havuz dolu mu kontrol et
    if (txMap.size >= maxSize) {
      // Düşük öncelikli işlemleri düşürmeyi dene
      if (!dropLowPriorityTransactions()) {
        return false // Yer açılamadı
      }
    }
    
    // Göndericiden gelen işlem sayısını kontrol et
    val senderCount = senderCounts.getOrElse(tx.sender, 0)
    val maxTxPerSender = if (currentLoad < 0.5) 50 else if (currentLoad < 0.8) 30 else 20
    
    if (senderCount >= maxTxPerSender) {
      // Aynı göndericiden çok fazla işlem var, eski işlemleri düşür
      dropOldTransactionsFromSender(tx.sender)
    }
    
    // İşlem grubunu belirle
    val groupType = determineGroupType(tx)
    
    // İşlem havuz girişi oluştur
    val entry = PoolEntry(
      transaction = tx,
      addedTime = Instant.now.getEpochSecond,
      status = Pending,
      groupType = groupType
    )
    
    // Havuza ekle
    txMap(tx.id) = entry
    txQueue.add(entry)
    
    // Gönderici sayısını güncelle
    senderCounts(tx.sender) = senderCount + 1
    
    true
  }
  
  /**
   * Belirli sayıda işlem al
   * @param count İstenilen işlem sayısı
   * @return İşlemler listesi
   */
  def getTransactions(count: Int): Seq[Transaction] = {
    if (count <= 0 || txQueue.isEmpty) {
      return Seq.empty
    }
    
    val result = mutable.ListBuffer[Transaction]()
    val entries = mutable.ListBuffer[PoolEntry]()
    
    // En öncelikli işlemleri al
    var i = 0
    while (i < count && !txQueue.isEmpty) {
      val entry = txQueue.poll()
      if (entry != null && entry.status == Pending) {
        result += entry.transaction
        entries += entry
        
        // İşlem deneme sayısını artır
        entry.attempts += 1
      }
      i += 1
    }
    
    // İşlemleri kuyruğa geri ekle
    entries.foreach(txQueue.add)
    
    result.toSeq
  }
  
  /**
   * İşlem durumunu güncelle
   * @param txId      İşlem kimliği
   * @param newStatus Yeni durum
   * @param blockHeight Opsiyonel blok yüksekliği
   * @return Başarılı mı?
   */
  def updateTransactionStatus(txId: String, newStatus: TxStatus, blockHeight: Option[Long] = None): Boolean = {
    txMap.get(txId) match {
      case Some(entry) =>
        val oldStatus = entry.status
        entry.status = newStatus
        
        // İşlem onaylandıysa
        if (newStatus == Confirmed && oldStatus != Confirmed) {
          // Gönderici sayısını azalt
          val sender = entry.transaction.sender
          val count = senderCounts.getOrElse(sender, 1)
          if (count > 1) {
            senderCounts(sender) = count - 1
          } else {
            senderCounts.remove(sender)
          }
          
          // İstatistikleri güncelle
          totalConfirmedTx += 1
          
          // İşlemi havuzdan kaldır
          txMap.remove(txId)
          
          // Callback'i çağır
          blockHeight.foreach { height =>
            confirmCallback.foreach(_(entry.transaction, height))
          }
        } else if (newStatus == Failed || newStatus == Dropped) {
          // İşlem hatalı veya düşürüldüyse
          // Gönderici sayısını azalt
          val sender = entry.transaction.sender
          val count = senderCounts.getOrElse(sender, 1)
          if (count > 1) {
            senderCounts(sender) = count - 1
          } else {
            senderCounts.remove(sender)
          }
          
          // İstatistikleri güncelle
          if (newStatus == Failed) totalRejectedTx += 1
          else totalDroppedTx += 1
          
          // İşlemi havuzdan kaldır
          txMap.remove(txId)
        }
        
        true
        
      case None =>
        false
    }
  }
  
  /**
   * İşlem getir
   * @param txId İşlem kimliği
   * @return İşlem (yoksa None)
   */
  def getTransaction(txId: String): Option[Transaction] = {
    txMap.get(txId).map(_.transaction)
  }
  
  /**
   * İşlemin durumunu kontrol et
   * @param txId İşlem kimliği
   * @return İşlem durumu (yoksa None)
   */
  def getTransactionStatus(txId: String): Option[TxStatus] = {
    txMap.get(txId).map(_.status)
  }
  
  /**
   * Gönderici için bekleyen işlemleri al
   * @param sender Gönderici adresi
   * @return Bekleyen işlemler
   */
  def getPendingTransactionsForSender(sender: String): Seq[Transaction] = {
    txMap.values
      .filter(entry => entry.transaction.sender == sender && entry.status == Pending)
      .map(_.transaction)
      .toSeq
  }
  
  /**
   * Tüm bekleyen işlemleri al
   * @return Bekleyen işlemler
   */
  def getAllPendingTransactions: Seq[Transaction] = {
    txMap.values
      .filter(_.status == Pending)
      .map(_.transaction)
      .toSeq
  }
  
  /**
   * Havuzdaki işlem sayısını al
   * @return İşlem sayısı
   */
  def size: Int = txMap.size
  
  /**
   * Havuz dolu mu?
   * @return Doluluk durumu
   */
  def isFull: Boolean = txMap.size >= maxSize
  
  /**
   * Havuzun doluluk oranını al (0.0-1.0)
   * @return Doluluk oranı
   */
  def getLoadFactor: Double = txMap.size.toDouble / maxSize
  
  /**
   * İstatistikleri al
   * @return İstatistikler
   */
  def getStatistics: Map[String, Long] = Map(
    "totalTransactions" -> txMap.size,
    "totalProcessed" -> totalProcessedTx,
    "totalConfirmed" -> totalConfirmedTx,
    "totalDropped" -> totalDroppedTx,
    "totalRejected" -> totalRejectedTx,
    "highPriorityCount" -> countByGroup(TxGroupType.HighPriority),
    "normalPriorityCount" -> countByGroup(TxGroupType.Normal),
    "lowPriorityCount" -> countByGroup(TxGroupType.LowPriority)
  )
  
  /**
   * Belirli bir grupta kaç işlem olduğunu say
   * @param groupType Grup türü
   * @return İşlem sayısı
   */
  private def countByGroup(groupType: TxGroupType): Long = {
    txMap.values.count(_.groupType == groupType)
  }
  
  /**
   * İşlemin hangi gruba ait olduğunu belirle
   * @param tx İşlem
   * @return Grup türü
   */
  private def determineGroupType(tx: Transaction): TxGroupType = {
    // Gas fiyatına göre grup belirle
    val gasPrice = tx.fee.gasPrice.total
    
    // Bu değerler sistem durumuna göre dinamik olarak ayarlanabilir
    val highPriorityThreshold = 10L // Örnek: 10 birim ve üzeri yüksek öncelikli
    val normalPriorityThreshold = 5L // Örnek: 5-10 arası normal öncelikli
    
    if (gasPrice >= highPriorityThreshold) {
      TxGroupType.HighPriority
    } else if (gasPrice >= normalPriorityThreshold) {
      TxGroupType.Normal
    } else {
      TxGroupType.LowPriority
    }
  }
  
  /**
   * Düşük öncelikli işlemleri düşür (yer açmak için)
   * @return İşlem başarılı mı?
   */
  private def dropLowPriorityTransactions(): Boolean = {
    // Öncelikle düşük öncelikli işlemleri düşür
    val lowPriorityEntries = txMap.values.filter(_.groupType == TxGroupType.LowPriority).toSeq
    
    if (lowPriorityEntries.nonEmpty) {
      // En düşük gas fiyatlı işlemleri bul
      val entriesToDrop = lowPriorityEntries
        .sortBy(_.transaction.fee.gasPrice.total)
        .take(maxSize / 20) // Toplam boyutun %5'ini düşür
      
      // İşlemleri düşür
      entriesToDrop.foreach { entry =>
        txMap.remove(entry.transaction.id)
        updateTransactionStatus(entry.transaction.id, Dropped)
      }
      
      // Sıralamayı yenile
      refreshPriorities()
      
      true
    } else {
      // Düşük öncelikli işlem yoksa, normal öncelikli işlemleri dene
      val normalPriorityEntries = txMap.values.filter(_.groupType == TxGroupType.Normal).toSeq
      
      if (normalPriorityEntries.nonEmpty) {
        // En düşük gas fiyatlı işlemleri bul
        val entriesToDrop = normalPriorityEntries
          .sortBy(_.transaction.fee.gasPrice.total)
          .take(maxSize / 50) // Toplam boyutun %2'sini düşür
        
        // İşlemleri düşür
        entriesToDrop.foreach { entry =>
          txMap.remove(entry.transaction.id)
          updateTransactionStatus(entry.transaction.id, Dropped)
        }
        
        // Sıralamayı yenile
        refreshPriorities()
        
        true
      } else {
        false // Düşürülecek işlem bulunamadı
      }
    }
  }
  
  /**
   * Belirli bir göndericiden gelen eski işlemleri düşür
   * @param sender Gönderici adresi
   */
  private def dropOldTransactionsFromSender(sender: String): Unit = {
    // Göndericiden gelen tüm işlemleri bul
    val senderEntries = txMap.values
      .filter(entry => entry.transaction.sender == sender && entry.status == Pending)
      .toSeq
    
    // En eski işlemleri bul
    val entriesToDrop = senderEntries
      .sortBy(_.addedTime) // Eski olan önce
      .take(senderEntries.size / 4) // %25'ini düşür
    
    // İşlemleri düşür
    entriesToDrop.foreach { entry =>
      txMap.remove(entry.transaction.id)
      updateTransactionStatus(entry.transaction.id, Dropped)
    }
  }
  
  /**
   * Eski işlemleri temizle
   */
  private def cleanupStaleTransactions(): Unit = {
    val currentTime = Instant.now.getEpochSecond
    
    // 1. Çok uzun süredir bekleyen işlemleri düşür
    val oldEntries = txMap.values
      .filter(entry => entry.status == Pending && 
               (entry.waitTime(currentTime) > txStaleTimeSeconds || 
                entry.attempts >= maxAttemptsBeforeDrop))
      .toSeq
    
    oldEntries.foreach { entry =>
      txMap.remove(entry.transaction.id)
      updateTransactionStatus(entry.transaction.id, Dropped)
    }
    
    // 2. İstatistikleri güncelle
    if (oldEntries.nonEmpty) {
      refreshPriorities()
    }
  }
  
  /**
   * İşlemleri yeniden gruplandır
   */
  private def regroupTransactions(): Unit = {
    val currentTime = Instant.now.getEpochSecond
    
    // Belirli bir bekleme süresi eşiğinden sonra işlemleri yükseltme
    val longWaitTimeThreshold = 1800L // 30 dakika
    val mediumWaitTimeThreshold = 900L // 15 dakika
    
    txMap.values.foreach { entry =>
      if (entry.status == Pending) {
        val waitTime = entry.waitTime(currentTime)
        
        // Uzun süre bekleyen düşük öncelikli işlemleri normal önceliğe yükselt
        if (entry.groupType == TxGroupType.LowPriority && waitTime > mediumWaitTimeThreshold) {
          entry.groupType = TxGroupType.Normal
        }
        // Uzun süre bekleyen normal öncelikli işlemleri yüksek önceliğe yükselt
        else if (entry.groupType == TxGroupType.Normal && waitTime > longWaitTimeThreshold) {
          entry.groupType = TxGroupType.HighPriority
        }
      }
    }
    
    // Grup boyutlarını dengele
    balanceGroupSizes()
    
    // Sıralamayı yenile
    refreshPriorities()
  }
  
  /**
   * Grup boyutlarını dengele
   */
  private def balanceGroupSizes(): Unit = {
    // Grup boyutlarını al
    val highPriorityCount = countByGroup(TxGroupType.HighPriority)
    val normalPriorityCount = countByGroup(TxGroupType.Normal)
    val lowPriorityCount = countByGroup(TxGroupType.LowPriority)
    
    // Yüksek öncelikli grup çok büyükse, aşağı düşür
    if (highPriorityCount > maxGroupSize) {
      val excessCount = (highPriorityCount - maxGroupSize).toInt
      val entries = txMap.values
        .filter(_.groupType == TxGroupType.HighPriority)
        .toSeq
        .sortBy(e => (e.transaction.fee.gasPrice.total, -e.waitTime())) // En düşük gas fiyatlı ve yeni eklenenler
        .take(excessCount)
      
      entries.foreach(_.groupType = TxGroupType.Normal)
    }
    
    // Normal öncelikli grup çok büyükse, aşağı düşür
    if (normalPriorityCount > maxGroupSize) {
      val excessCount = (normalPriorityCount - maxGroupSize).toInt
      val entries = txMap.values
        .filter(_.groupType == TxGroupType.Normal)
        .toSeq
        .sortBy(e => (e.transaction.fee.gasPrice.total, -e.waitTime())) // En düşük gas fiyatlı ve yeni eklenenler
        .take(excessCount)
      
      entries.foreach(_.groupType = TxGroupType.LowPriority)
    }
  }
  
  /**
   * Sıralamayı yenile (öncelik değişiminden sonra)
   */
  private def refreshPriorities(): Unit = {
    // Tüm işlemleri yeniden sırala
    val entries = new java.util.ArrayList[PoolEntry](txMap.values.toSeq.asJava)
    txQueue.clear()
    txQueue.addAll(entries)
  }
}

/**
 * İşlem havuzu yardımcı nesnesi
 */
object TransactionPool {
  /**
   * Yeni bir işlem havuzu oluştur
   * @param maxSize Maksimum havuz boyutu
   * @return İşlem havuzu
   */
  def create(maxSize: Int = 10000): TransactionPool = {
    val pool = new TransactionPool(maxSize)
    pool.start()
    pool
  }
} 