package cli

import core._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success, Failure}
import java.time.{Instant, Duration, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable

/**
 * İşlem İzleme Aracı
 * Blockchain ağındaki işlemleri izlemek ve raporlamak için kullanılır
 */
class TransactionMonitor(
  val blockchain: Blockchain,
  val updateIntervalMs: Int = 5000,
  implicit val executionContext: ExecutionContext
) {
  // İzleme durumu
  private val isRunning = new AtomicBoolean(false)
  
  // İzlenen İşlemler
  private val monitoredTransactions = mutable.Map[String, TransactionStatus]()
  
  // İşlem filtreleri
  private var addressFilter: Option[String] = None
  private var minimumValueFilter: Option[Double] = None
  private var maximumValueFilter: Option[Double] = None
  private var statusFilter: Option[String] = None
  
  // İstatistikler
  private val transactionStats = mutable.Map[String, Int]()
  private var startTime: Long = 0
  private var lastBlockHeight: Long = 0
  private var transactionCount: Int = 0
  private var confirmedTransactions: Int = 0
  private var failedTransactions: Int = 0
  private var totalValue: Double = 0.0
  private var avgConfirmationTime: Double = 0.0
  private var peakTPS: Double = 0.0
  private var currentTPS: Double = 0.0
  
  // İşlem geçmişi (saniyede işlem sayısı istatistiği için)
  private val transactionHistory = mutable.Queue[(Long, Int)]() // (timestamp, count)
  
  /**
   * İzleyiciyi başlat
   */
  def start(): Future[Unit] = Future {
    if (isRunning.compareAndSet(false, true)) {
      println(s"${Colors.GREEN}İşlem izleyici başlatılıyor...${Colors.RESET}")
      startTime = System.currentTimeMillis()
      
      // Ana izleme döngüsü
      while (isRunning.get()) {
        try {
          updateTransactionInfo()
          calculateStatistics()
          Thread.sleep(updateIntervalMs)
        } catch {
          case e: InterruptedException => isRunning.set(false)
          case e: Exception => println(s"${Colors.RED}İzleyici hatası: ${e.getMessage}${Colors.RESET}")
        }
      }
    }
  }
  
  /**
   * İzleyiciyi durdur
   */
  def stop(): Unit = {
    if (isRunning.compareAndSet(true, false)) {
      println(s"${Colors.YELLOW}İşlem izleyici durduruluyor...${Colors.RESET}")
    }
  }
  
  /**
   * İşlem bilgilerini güncelle
   */
  private def updateTransactionInfo(): Unit = {
    // Blockchain'den son işlemleri ve işlem havuzunu al
    val pendingTransactions = blockchain.getPendingTransactions
    val transactionPool = blockchain.getTransactionPoolStatistics
    
    // İşlemci bilgilerini güncelle
    lastBlockHeight = blockchain.getChainInfo("currentHeight").toString.toLong
    
    // İşlem havuzundaki yeni işlemleri ekle
    pendingTransactions.foreach { tx =>
      if (!monitoredTransactions.contains(tx.id) && matchesFilter(tx)) {
        monitoredTransactions.put(tx.id, TransactionStatus(
          id = tx.id,
          sender = tx.sender,
          recipient = tx.recipient,
          value = tx.value,
          gasPrice = tx.gasPrice,
          gasLimit = tx.gasLimit,
          data = tx.data,
          timeAdded = System.currentTimeMillis(),
          status = "pending",
          confirmations = 0,
          blockHeight = 0,
          confirmationTime = 0
        ))
        transactionCount += 1
      }
    }
    
    // Mevcut izlenen işlemlerin durumunu güncelle
    monitoredTransactions.values.filter(_.status == "pending").foreach { txStatus =>
      // İşlemin durumunu kontrol et
      blockchain.getTransactionById(txStatus.id) match {
        case Some(tx) if tx.status.isConfirmed =>
          // İşlem onaylandıysa
          val updatedStatus = txStatus.copy(
            status = "confirmed",
            confirmations = 1, // Başlangıç değeri
            blockHeight = tx.result.map(_.blockHeight).getOrElse(0),
            confirmationTime = System.currentTimeMillis() - txStatus.timeAdded
          )
          monitoredTransactions.update(txStatus.id, updatedStatus)
          confirmedTransactions += 1
          
        case Some(tx) if tx.status.isFailed =>
          // İşlem başarısız olduysa
          val updatedStatus = txStatus.copy(
            status = "failed",
            confirmations = 0,
            blockHeight = 0,
            confirmationTime = System.currentTimeMillis() - txStatus.timeAdded
          )
          monitoredTransactions.update(txStatus.id, updatedStatus)
          failedTransactions += 1
          
        case _ => // İşlem hala beklemede
      }
    }
    
    // Onaylanan işlemlerin onay sayısını güncelle
    monitoredTransactions.values.filter(_.status == "confirmed").foreach { txStatus =>
      val currentConfirmations = lastBlockHeight - txStatus.blockHeight + 1
      if (currentConfirmations > txStatus.confirmations) {
        val updatedStatus = txStatus.copy(confirmations = currentConfirmations.toInt)
        monitoredTransactions.update(txStatus.id, updatedStatus)
      }
    }
  }
  
  /**
   * İstatistikleri hesapla
   */
  private def calculateStatistics(): Unit = {
    val now = System.currentTimeMillis()
    
    // Toplam değeri hesapla
    totalValue = monitoredTransactions.values.map(_.value).sum
    
    // Ortalama onaylama süresini hesapla
    val confirmedTxs = monitoredTransactions.values.filter(_.status == "confirmed")
    if (confirmedTxs.nonEmpty) {
      avgConfirmationTime = confirmedTxs.map(_.confirmationTime).sum / confirmedTxs.size.toDouble
    }
    
    // Saniyede işlem sayısını hesapla
    val last10Seconds = now - 10000
    
    // Geçmiş kayıtlardan süresi dolanları temizle
    while (transactionHistory.nonEmpty && transactionHistory.head._1 < last10Seconds) {
      transactionHistory.dequeue()
    }
    
    // Son 1 saniyede eklenen işlem sayısını ekle
    val newTransactions = monitoredTransactions.values.count(tx => tx.timeAdded > now - 1000)
    transactionHistory.enqueue((now, newTransactions))
    
    // Son 10 saniyelik TPS ortalamasını hesapla
    if (transactionHistory.nonEmpty) {
      val totalTxInWindow = transactionHistory.map(_._2).sum
      val oldestTimestamp = if (transactionHistory.nonEmpty) transactionHistory.head._1 else now
      val timeWindow = (now - oldestTimestamp) / 1000.0
      currentTPS = if (timeWindow > 0) totalTxInWindow / timeWindow else 0
      
      // Zirve TPS güncelle
      if (currentTPS > peakTPS) {
        peakTPS = currentTPS
      }
    }
  }
  
  /**
   * İşlem bir filtreye uyuyor mu kontrol et
   * @param tx Kontrol edilecek işlem
   * @return Eşleşiyor mu
   */
  private def matchesFilter(tx: Transaction): Boolean = {
    var matches = true
    
    // Adres filtresi kontrol et
    if (addressFilter.isDefined) {
      matches = matches && (tx.sender == addressFilter.get || tx.recipient == addressFilter.get)
    }
    
    // Minimum değer filtresi kontrol et
    if (minimumValueFilter.isDefined) {
      matches = matches && tx.value >= minimumValueFilter.get
    }
    
    // Maksimum değer filtresi kontrol et
    if (maximumValueFilter.isDefined) {
      matches = matches && tx.value <= maximumValueFilter.get
    }
    
    matches
  }
  
  /**
   * Filtre ayarla
   * @param address Adres filtresi
   * @param minValue Minimum değer filtresi
   * @param maxValue Maksimum değer filtresi
   * @param status Durum filtresi
   */
  def setFilter(
    address: Option[String] = None,
    minValue: Option[Double] = None,
    maxValue: Option[Double] = None,
    status: Option[String] = None
  ): Unit = {
    addressFilter = address
    minimumValueFilter = minValue
    maximumValueFilter = maxValue
    statusFilter = status
    
    println(s"${Colors.GREEN}Filtreler güncellendi.${Colors.RESET}")
  }
  
  /**
   * İstatistikleri yazdır
   */
  def printStatistics(): Unit = {
    val runningTime = System.currentTimeMillis() - startTime
    val runningTimeStr = formatDuration(runningTime)
    
    println(s"${Colors.CYAN}${Colors.BOLD}İşlem İzleme İstatistikleri:${Colors.RESET}")
    println(s"Çalışma Süresi:      ${Colors.YELLOW}$runningTimeStr${Colors.RESET}")
    println(s"Toplam İşlem:         ${transactionCount}")
    println(s"Onaylanan İşlemler:   ${confirmedTransactions}")
    println(s"Başarısız İşlemler:   ${failedTransactions}")
    println(s"Bekleyen İşlemler:    ${transactionCount - confirmedTransactions - failedTransactions}")
    println(s"Toplam Değer:         ${totalValue.formatted("%.6f")}")
    println(s"Ort. Onaylama Süresi: ${formatDuration(avgConfirmationTime.toLong)}")
    println(s"Mevcut TPS:           ${currentTPS.formatted("%.2f")} tx/s")
    println(s"Zirve TPS:            ${peakTPS.formatted("%.2f")} tx/s")
    println(s"Son Blok Yüksekliği:  ${lastBlockHeight}")
  }
  
  /**
   * İzlenen işlemleri göster
   * @param limit Gösterilecek işlem sayısı
   * @param filter Durum filtresi
   * @param sortBy Sıralama kriteri
   */
  def showTransactions(
    limit: Int = 10,
    filter: Option[String] = None,
    sortBy: String = "time"
  ): Unit = {
    // Filtreleme
    var filtered = monitoredTransactions.values.toSeq
    
    filter match {
      case Some("pending") => filtered = filtered.filter(_.status == "pending")
      case Some("confirmed") => filtered = filtered.filter(_.status == "confirmed")
      case Some("failed") => filtered = filtered.filter(_.status == "failed")
      case _ => // Tüm işlemler
    }
    
    // Sıralama
    val sorted = sortBy match {
      case "time" => filtered.sortBy(-_.timeAdded)
      case "value" => filtered.sortBy(-_.value)
      case "confirmations" => filtered.sortBy(-_.confirmations)
      case "gasPrice" => filtered.sortBy(-_.gasPrice)
      case _ => filtered.sortBy(-_.timeAdded)
    }
    
    // Limitli gösterme
    val limited = sorted.take(limit)
    
    // Tablo başlığı
    println(f"${Colors.CYAN}%-20s %-10s %-10s %-10s %-8s %-14s %-6s${Colors.RESET}" format 
      ("İşlem ID", "Değer", "Gas", "G.Fiyatı", "Durum", "Süre", "Onay"))
    println("-" * 80)
    
    // Tabloyu yazdır
    limited.foreach { tx =>
      val idShort = tx.id.take(8) + "..." + tx.id.takeRight(8)
      val value = tx.value.formatted("%.6f")
      val gas = tx.gasLimit.toString
      val gasPrice = tx.gasPrice.formatted("%.2f")
      val status = tx.status match {
        case "pending" => s"${Colors.YELLOW}bekliyor${Colors.RESET}"
        case "confirmed" => s"${Colors.GREEN}onaylandı${Colors.RESET}"
        case "failed" => s"${Colors.RED}başarısız${Colors.RESET}"
        case _ => tx.status
      }
      val time = formatDuration(System.currentTimeMillis() - tx.timeAdded)
      val confirmations = if (tx.status == "confirmed") tx.confirmations.toString else "-"
      
      println(f"%-20s %-10s %-10s %-10s %-8s %-14s %-6s" format 
        (idShort, value, gas, gasPrice, status, time, confirmations))
    }
    
    println("-" * 80)
    println(s"${Colors.CYAN}Toplam ${filtered.size} işlem${Colors.RESET}")
  }
  
  /**
   * İşlem havuzu durumunu göster
   */
  def showTransactionPoolStatus(): Unit = {
    val stats = blockchain.getTransactionPoolStatistics
    
    println(s"${Colors.CYAN}${Colors.BOLD}İşlem Havuzu Durumu:${Colors.RESET}")
    println(s"Toplam İşlem:       ${stats("totalTransactions")}")
    println(s"Bekleyen İşlemler:  ${stats("pendingTransactions")}")
    println(s"Yüksek Öncelikli:   ${stats("highPriorityTransactions")}")
    println(s"Normal Öncelikli:   ${stats("normalPriorityTransactions")}")
    println(s"Düşük Öncelikli:    ${stats("lowPriorityTransactions")}")
    println(s"Son Saat Onaylama: ${stats("confirmedLastHour")}")
    println(s"Son Saat Başarısız: ${stats("droppedLastHour")}")
    println(s"Ortalama Gas Fiyatı: ${stats("avgGasPrice")}")
    println(s"Tavsiye Edilen Gas:  ${stats("suggestedGasPrice")}")
    println(s"Havuz Doluluk:       ${stats("poolFullness").toString.toDouble.formatted("%.2f")}%")
  }
  
  /**
   * Belirli bir işlemi detaylı göster
   * @param txId İşlem ID
   */
  def showTransactionDetails(txId: String): Unit = {
    // Yerel izleme bilgisinden kontrol et
    monitoredTransactions.get(txId) match {
      case Some(tx) =>
        printTransactionDetails(tx)
        
      case None =>
        // Blockchain'den kontrol et
        blockchain.getTransactionById(txId) match {
          case Some(tx) =>
            val txStatus = TransactionStatus(
              id = tx.id,
              sender = tx.sender,
              recipient = tx.recipient,
              value = tx.value,
              gasPrice = tx.gasPrice,
              gasLimit = tx.gasLimit,
              data = tx.data,
              timeAdded = tx.timestamp,
              status = if (tx.status.isConfirmed) "confirmed" 
                       else if (tx.status.isFailed) "failed" 
                       else "pending",
              confirmations = 0, // Bunu daha sonra hesaplayacağız
              blockHeight = tx.result.map(_.blockHeight).getOrElse(0),
              confirmationTime = tx.result.map(r => r.timestamp - tx.timestamp).getOrElse(0)
            )
            
            // Onay sayısını hesapla
            val confirmations = if (txStatus.blockHeight > 0) {
              (lastBlockHeight - txStatus.blockHeight + 1).toInt
            } else 0
            
            printTransactionDetails(txStatus.copy(confirmations = confirmations))
            
          case None =>
            println(s"${Colors.RED}İşlem bulunamadı: $txId${Colors.RESET}")
        }
    }
  }
  
  /**
   * İşlem detaylarını yazdır
   * @param tx İşlem durumu
   */
  private def printTransactionDetails(tx: TransactionStatus): Unit = {
    println(s"${Colors.CYAN}${Colors.BOLD}İşlem Detayları:${Colors.RESET}")
    println(s"İşlem ID:          ${tx.id}")
    println(s"Gönderen:          ${tx.sender}")
    println(s"Alıcı:             ${tx.recipient}")
    println(s"Değer:             ${tx.value.formatted("%.6f")}")
    println(s"Gas Limiti:        ${tx.gasLimit}")
    println(s"Gas Fiyatı:        ${tx.gasPrice.formatted("%.2f")}")
    println(s"Toplam Gas Maliyeti: ${(tx.gasLimit * tx.gasPrice).formatted("%.6f")}")
    println(s"Veri:              ${if (tx.data.isEmpty) "(boş)" else tx.data}")
    
    val status = tx.status match {
      case "pending" => s"${Colors.YELLOW}Bekliyor${Colors.RESET}"
      case "confirmed" => s"${Colors.GREEN}Onaylandı${Colors.RESET}"
      case "failed" => s"${Colors.RED}Başarısız${Colors.RESET}"
      case _ => tx.status
    }
    
    println(s"Durum:             $status")
    println(s"Eklenme Zamanı:    ${formatTimestamp(tx.timeAdded)}")
    
    if (tx.status == "confirmed") {
      println(s"Blok Yüksekliği:   ${tx.blockHeight}")
      println(s"Onay Sayısı:       ${tx.confirmations}")
      println(s"Onaylama Süresi:   ${formatDuration(tx.confirmationTime)}")
    } else if (tx.status == "failed") {
      println(s"Başarısız Olma Süresi: ${formatDuration(tx.confirmationTime)}")
    } else {
      println(s"Bekleme Süresi:    ${formatDuration(System.currentTimeMillis() - tx.timeAdded)}")
    }
  }
  
  /**
   * Süreyi formatlı şekilde göster
   * @param durationMs Süre (milisaniye)
   * @return Formatlı süre
   */
  private def formatDuration(durationMs: Long): String = {
    if (durationMs < 1000) {
      s"${durationMs}ms"
    } else if (durationMs < 60000) {
      f"${durationMs / 1000.0}%.2fs"
    } else {
      val minutes = durationMs / 60000
      val seconds = (durationMs % 60000) / 1000.0
      f"${minutes}m ${seconds}%.2fs"
    }
  }
  
  /**
   * Zaman damgasını formatlı şekilde göster
   * @param timestamp Zaman damgası (milisaniye)
   * @return Formatlı zaman
   */
  private def formatTimestamp(timestamp: Long): String = {
    val instant = Instant.ofEpochMilli(timestamp)
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
      .withZone(ZoneId.systemDefault())
    formatter.format(instant)
  }
}

/**
 * İşlem durumu
 */
case class TransactionStatus(
  id: String,
  sender: String,
  recipient: String,
  value: Double,
  gasPrice: Double,
  gasLimit: Long,
  data: String,
  timeAdded: Long,
  status: String,
  confirmations: Int,
  blockHeight: Long,
  confirmationTime: Long
)

/**
 * TransactionMonitor yardımcı sınıfı
 */
object TransactionMonitor {
  /**
   * İzleyici oluştur
   * @param blockchain Blockchain nesnesi
   * @param updateIntervalMs Güncelleme aralığı (ms)
   * @param executionContext Yürütme bağlamı
   * @return İzleyici nesnesi
   */
  def create(
    blockchain: Blockchain,
    updateIntervalMs: Int = 5000
  )(implicit executionContext: ExecutionContext): TransactionMonitor = {
    new TransactionMonitor(blockchain, updateIntervalMs, executionContext)
  }
} 