package cli

import core._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success, Failure}
import scala.io.StdIn
import scala.collection.mutable
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.UUID
import java.io.{File, FileWriter, PrintWriter}

/**
 * Komut satırı arayüzü renkleri
 */
object Colors {
  val RESET = "\u001B[0m"
  val BLACK = "\u001B[30m"
  val RED = "\u001B[31m"
  val GREEN = "\u001B[32m"
  val YELLOW = "\u001B[33m"
  val BLUE = "\u001B[34m"
  val PURPLE = "\u001B[35m"
  val CYAN = "\u001B[36m"
  val WHITE = "\u001B[37m"
  val BOLD = "\u001B[1m"
  val UNDERLINE = "\u001B[4m"
}

/**
 * Blockchain CLI yardımcı sınıfı
 */
object BlockchainCLI {
  // Aktif düğüm
  private var activeNode: Option[Blockchain] = None
  
  // Çıkış isteği
  private var exitRequested = false
  
  // Komut geçmişi
  private val commandHistory = mutable.ListBuffer[String]()
  
  // Komut satırı promptu
  private var prompt = "blockchain> "
  
  // Son komutun çalışma süresi
  private var lastCommandDuration = 0L
  
  /**
   * CLI başlatma
   * @param args Komut satırı argümanları
   */
  def main(args: Array[String]): Unit = {
    println(s"${Colors.CYAN}${Colors.BOLD}Blockchain CLI v1.0${Colors.RESET}")
    println("Komutlar hakkında bilgi için 'help' yazın.")
    
    // Düğüm başlatma argümanlarını kontrol et
    if (args.contains("--start-node")) {
      val nodeId = args.dropWhile(_ != "--node-id").drop(1).headOption.getOrElse(s"node-${UUID.randomUUID().toString.take(8)}")
      startNode(nodeId)
    }
    
    // Ana döngü
    while (!exitRequested) {
      print(s"${Colors.GREEN}$prompt${Colors.RESET}")
      val input = StdIn.readLine()
      
      if (input != null && input.trim.nonEmpty) {
        processCommand(input.trim)
      }
    }
    
    // Kaynaklarını serbest bırak
    cleanup()
    
    println(s"${Colors.YELLOW}Blockchain CLI kapatılıyor. Hoşçakalın!${Colors.RESET}")
  }
  
  /**
   * Komut işleme
   * @param input Kullanıcı girişi
   */
  private def processCommand(input: String): Unit = {
    val startTime = System.currentTimeMillis()
    
    // Komutu geçmişe ekle
    commandHistory += input
    
    // Komutu parçalara ayır
    val parts = input.split("\\s+").filter(_.nonEmpty)
    if (parts.isEmpty) return
    
    // Komutu işle
    val cmd = parts(0).toLowerCase
    val args = parts.drop(1)
    
    try {
      cmd match {
        case "help" => showHelp(args)
        case "exit" | "quit" => exitRequested = true
        case "history" => showHistory()
        case "clear" => clearScreen()
        
        // Düğüm yönetimi
        case "start-node" => startNodeCommand(args)
        case "stop-node" => stopNode()
        case "status" => showNodeStatus()
        case "info" => showNodeInfo()
        
        // İşlem komutları
        case "tx" => handleTransactionCommands(args)
        case "create-tx" => createTransaction(args)
        case "send-tx" => sendTransaction(args)
        case "get-tx" => getTransaction(args)
        case "list-txs" => listTransactions(args)
        case "pool" => showTransactionPool(args)
        
        // Ağ komutları
        case "network" => handleNetworkCommands(args)
        case "peers" => listPeers()
        case "connect" => connectToPeer(args)
        
        // Blok komutları
        case "block" => handleBlockCommands(args)
        case "get-block" => getBlock(args)
        case "create-block" => createBlock()
        
        // Stake komutları
        case "stake" => handleStakeCommands(args)
        case "validators" => listValidators()
        case "register-validator" => registerValidator(args)
        case "add-stake" => addStake(args)
        case "unstake" => unstake(args)
        
        // Ödül komutları
        case "rewards" => handleRewardCommands(args)
        case "get-rewards" => getRewards(args)
        
        // İzleme komutları
        case "watch" => watchBlockchain(args)
        case "stats" => showStatistics(args)
        case "dashboard" => showDashboard()
        
        // Veri ve yapılandırma
        case "export" => exportData(args)
        case "import" => importData(args)
        case "config" => handleConfigCommands(args)
        
        case unknown => println(s"${Colors.RED}Bilinmeyen komut: $unknown. Yardım için 'help' yazın.${Colors.RESET}")
      }
    } catch {
      case e: Exception => 
        println(s"${Colors.RED}Hata: ${e.getMessage}${Colors.RESET}")
        e.printStackTrace()
    }
    
    lastCommandDuration = System.currentTimeMillis() - startTime
    
    // Komut süresini göster (100ms'den uzun sürdüyse)
    if (lastCommandDuration > 100) {
      println(s"${Colors.YELLOW}Komut çalışma süresi: ${formatDuration(lastCommandDuration)}${Colors.RESET}")
    }
  }
  
  /**
   * Yardım gösterme
   * @param args Argümanlar
   */
  private def showHelp(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println(s"${Colors.CYAN}${Colors.BOLD}Mevcut Komutlar:${Colors.RESET}")
      println(s"${Colors.CYAN}Genel:${Colors.RESET}")
      println("  help [komut]      - Tüm komutlar veya belirli bir komut için yardım gösterir")
      println("  exit, quit        - CLI'dan çıkış yapar")
      println("  history           - Komut geçmişini gösterir")
      println("  clear             - Ekranı temizler")
      
      println(s"\n${Colors.CYAN}Düğüm Yönetimi:${Colors.RESET}")
      println("  start-node        - Yeni bir düğüm başlatır")
      println("  stop-node         - Aktif düğümü durdurur")
      println("  status            - Düğüm durumunu gösterir")
      println("  info              - Düğüm bilgilerini gösterir")
      
      println(s"\n${Colors.CYAN}İşlem:${Colors.RESET}")
      println("  tx                - İşlem komutları (daha fazla bilgi için 'help tx')")
      println("  create-tx         - Yeni bir işlem oluşturur")
      println("  send-tx           - İşlem gönderir")
      println("  get-tx            - İşlem detaylarını gösterir")
      println("  list-txs          - İşlemleri listeler")
      println("  pool              - İşlem havuzu bilgisi")
      
      println(s"\n${Colors.CYAN}Ağ:${Colors.RESET}")
      println("  network           - Ağ komutları (daha fazla bilgi için 'help network')")
      println("  peers             - Bağlı eşleri listeler")
      println("  connect           - Bir eşe bağlanır")
      
      println(s"\n${Colors.CYAN}Blok Komutları:${Colors.RESET}")
      println("  block             - Blok komutları (daha fazla bilgi için 'help block')")
      println("  get-block         - Blok detaylarını gösterir")
      
      println(s"\n${Colors.CYAN}Stake İşlemleri:${Colors.RESET}")
      println("  stake             - Stake komutları (daha fazla bilgi için 'help stake')")
      println("  validators        - Doğrulayıcıları listeler")
      println("  register-validator - Doğrulayıcı olarak kaydolur")
      println("  add-stake         - Stake ekler")
      println("  unstake           - Stake çeker")
      
      println(s"\n${Colors.CYAN}Ödüller:${Colors.RESET}")
      println("  rewards           - Ödül komutları (daha fazla bilgi için 'help rewards')")
      println("  get-rewards       - Ödül geçmişini gösterir")
      
      println(s"\n${Colors.CYAN}İzleme:${Colors.RESET}")
      println("  watch             - Blockchain'i izler")
      println("  stats             - İstatistikleri gösterir")
      println("  dashboard         - Canlı dashboard'u gösterir")
      
      println(s"\n${Colors.CYAN}Veri ve Yapılandırma:${Colors.RESET}")
      println("  export            - Veri dışa aktarır")
      println("  import            - Veri içe aktarır")
      println("  config            - Yapılandırma ayarları")
    } else {
      val command = args(0).toLowerCase
      
      command match {
        case "tx" | "transaction" =>
          println(s"${Colors.CYAN}${Colors.BOLD}İşlem Komutları:${Colors.RESET}")
          println("  tx create [alıcı] [miktar] [data] - Yeni işlem oluşturur")
          println("  tx send [txId]                    - İşlemi gönderir")
          println("  tx get [txId]                     - İşlem detaylarını gösterir")
          println("  tx list [limit=10]                - İşlemleri listeler")
          println("  tx pending                        - Bekleyen işlemleri gösterir")
          println("  tx fee [priority=false]           - Tavsiye edilen ücreti gösterir")
        
        case "network" =>
          println(s"${Colors.CYAN}${Colors.BOLD}Ağ Komutları:${Colors.RESET}")
          println("  network status                - Ağ durumunu gösterir")
          println("  network peers                 - Bağlı eşleri listeler")
          println("  network connect [ip:port]     - Bir eşe bağlanır")
          println("  network disconnect [peerId]   - Bir eşle bağlantıyı keser")
          println("  network sync-status           - Senkronizasyon durumunu gösterir")
          
        case "block" =>
          println(s"${Colors.CYAN}${Colors.BOLD}Blok Komutları:${Colors.RESET}")
          println("  block get [height]            - Belirli yükseklikteki bloğu gösterir")
          println("  block latest                  - En son bloğu gösterir")
          println("  block list [limit=10]         - Blokları listeler")
          println("  block tips                    - DAG uç bloklarını gösterir")
          println("  block export [height] [file]  - Bloğu dışa aktarır")
          
        case "stake" =>
          println(s"${Colors.CYAN}${Colors.BOLD}Stake Komutları:${Colors.RESET}")
          println("  stake info                    - Stake bilgilerini gösterir")
          println("  stake validators              - Doğrulayıcıları listeler")
          println("  stake register [isim] [miktar] [komisyon] - Doğrulayıcı olarak kaydolur")
          println("  stake add [validatorId] [miktar] - Stake ekler")
          println("  stake withdraw [stakeId]      - Stake çeker")
          println("  stake balance                 - Stake bakiyesini gösterir")
          
        case "rewards" =>
          println(s"${Colors.CYAN}${Colors.BOLD}Ödül Komutları:${Colors.RESET}")
          println("  rewards info                  - Ödül bilgilerini gösterir")
          println("  rewards history               - Ödül geçmişini gösterir")
          println("  rewards calculate [height]    - Belirli blok için ödülleri hesaplar")
          println("  rewards stats                 - Ödül istatistiklerini gösterir")
          
        case "config" =>
          println(s"${Colors.CYAN}${Colors.BOLD}Yapılandırma Komutları:${Colors.RESET}")
          println("  config get [anahtar]          - Yapılandırma değerini gösterir")
          println("  config set [anahtar] [değer]  - Yapılandırma değerini ayarlar")
          println("  config list                   - Tüm yapılandırma değerlerini gösterir")
          println("  config export [dosya]         - Yapılandırmayı dışa aktarır")
          println("  config import [dosya]         - Yapılandırmayı içe aktarır")
          
        case _ =>
          println(s"${Colors.RED}Bilinmeyen komut: $command. Yardım için 'help' yazın.${Colors.RESET}")
      }
    }
  }
  
  /**
   * Komut geçmişini göster
   */
  private def showHistory(): Unit = {
    println(s"${Colors.CYAN}${Colors.BOLD}Komut Geçmişi:${Colors.RESET}")
    commandHistory.zipWithIndex.foreach { case (cmd, idx) =>
      println(f"${idx + 1}%3d: $cmd")
    }
  }
  
  /**
   * Ekranı temizle
   */
  private def clearScreen(): Unit = {
    // ANSI escape sequence for clearing the screen
    print("\u001b[2J")
    // ANSI escape sequence for moving cursor to top-left corner
    print("\u001b[H")
    // Flush
    Console.flush()
  }
  
  /**
   * Düğüm başlatma komutu işleme
   * @param args Argümanlar
   */
  private def startNodeCommand(args: Array[String]): Unit = {
    if (activeNode.isDefined) {
      println(s"${Colors.YELLOW}Zaten aktif bir düğüm var. Önce 'stop-node' komutunu kullanın.${Colors.RESET}")
      return
    }
    
    val nodeId = args.headOption.getOrElse(s"node-${UUID.randomUUID().toString.take(8)}")
    startNode(nodeId)
  }
  
  /**
   * Düğüm başlatma
   * @param nodeId Düğüm kimliği
   */
  private def startNode(nodeId: String): Unit = {
    println(s"${Colors.YELLOW}Düğüm başlatılıyor: $nodeId...${Colors.RESET}")
    
    implicit val ec = ExecutionContext.global
    
    // KeyPair oluştur
    val keyPair = KeyPairUtils.generateKeyPair()
    
    // Bootstrap düğümleri
    val bootstrapNodes = Seq("127.0.0.1:9000") // Varsayılan bootstrap düğümü
    
    // Blockchain düğümünü başlat
    val blockchain = new Blockchain(
      nodeId = nodeId,
      keyPair = keyPair,
      endpoint = s"127.0.0.1:${9000 + commandHistory.size % 1000}", // Basit port atama
      bootstrapNodes = if (nodeId == "node-1") Seq.empty else bootstrapNodes,
      executionContext = ec
    )
    
    blockchain.start()
    activeNode = Some(blockchain)
    
    // Promptu güncelle
    prompt = s"$nodeId> "
    
    // Durum bilgisi
    println(s"${Colors.GREEN}Düğüm başlatıldı:${Colors.RESET}")
    showNodeInfo()
  }
  
  /**
   * Düğümü durdur
   */
  private def stopNode(): Unit = {
    activeNode match {
      case Some(node) =>
        println(s"${Colors.YELLOW}Düğüm durduruluyor: ${node.nodeId}...${Colors.RESET}")
        // Düğümü durdur (gerçek uygulamada burada durdurma işlemleri yapılır)
        activeNode = None
        prompt = "blockchain> "
        println(s"${Colors.GREEN}Düğüm durduruldu.${Colors.RESET}")
        
      case None =>
        println(s"${Colors.RED}Aktif düğüm yok. Önce 'start-node' komutunu kullanın.${Colors.RESET}")
    }
  }
  
  /**
   * Düğüm durumunu göster
   */
  private def showNodeStatus(): Unit = {
    activeNode match {
      case Some(node) =>
        val info = node.getChainInfo
        
        println(s"${Colors.CYAN}${Colors.BOLD}Düğüm Durumu:${Colors.RESET}")
        println(s"ID:                 ${Colors.GREEN}${info("nodeId")}${Colors.RESET}")
        println(s"Mevcut Yükseklik:   ${Colors.YELLOW}${info("currentHeight")}${Colors.RESET}")
        println(s"Mevcut Tur:         ${info("currentRound")}")
        println(s"Son Blok Zamanı:    ${formatTimestamp(info("lastBlockTime").toString.toLong)}")
        println(s"Aktif Doğrulayıcılar: ${info("activeValidators")}")
        println(s"Bağlı Eşler:        ${info("connectedPeers")}")
        println(s"Bekleyen İşlemler:  ${info("pendingTransactions")}")
        println(s"Gas Fiyatı:         ${info("currentGasPrice")}")
        println(s"Bakiye:             ${info("balance")}")
        println(s"Toplam Bakiye:      ${info("totalBalance")}")
        println(s"Toplam Ödüller:     ${info("totalRewards")}")
        println(s"Toplam Stake:       ${info("totalStake")}")
        println(s"Senkronizasyon:     ${info("syncStatus")} (${info("syncProgress").toString.toDouble.formatted("%.2f")}%)")
        
      case None =>
        println(s"${Colors.RED}Aktif düğüm yok. Önce 'start-node' komutunu kullanın.${Colors.RESET}")
    }
  }
  
  /**
   * Düğüm bilgilerini göster
   */
  private def showNodeInfo(): Unit = {
    activeNode match {
      case Some(node) =>
        println(s"${Colors.CYAN}${Colors.BOLD}Düğüm Bilgisi:${Colors.RESET}")
        println(s"ID:           ${Colors.GREEN}${node.nodeId}${Colors.RESET}")
        println(s"Endpoint:     ${node.endpoint}")
        println(s"Başlangıç:    ${node.bootstrapNodes.mkString(", ")}")
        
      case None =>
        println(s"${Colors.RED}Aktif düğüm yok. Önce 'start-node' komutunu kullanın.${Colors.RESET}")
    }
  }
  
  /**
   * İşlem komutlarını işle
   * @param args Argümanlar
   */
  private def handleTransactionCommands(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println(s"${Colors.RED}İşlem komutu eksik. 'help tx' komutunu kullanın.${Colors.RESET}")
      return
    }
    
    val subCommand = args(0).toLowerCase
    val subArgs = args.drop(1)
    
    subCommand match {
      case "create" => createTransaction(subArgs)
      case "send" => sendTransaction(subArgs)
      case "get" => getTransaction(subArgs)
      case "list" => listTransactions(subArgs)
      case "pending" => listPendingTransactions()
      case "fee" => showTransactionFee(subArgs)
      case _ => println(s"${Colors.RED}Bilinmeyen işlem komutu: $subCommand. 'help tx' komutunu kullanın.${Colors.RESET}")
    }
  }
  
  /**
   * İşlem oluştur
   * @param args Argümanlar
   */
  private def createTransaction(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("İşlem oluşturma işlevi henüz uygulanmadı.")
  }
  
  /**
   * İşlem gönder
   * @param args Argümanlar
   */
  private def sendTransaction(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("İşlem gönderme işlevi henüz uygulanmadı.")
  }
  
  /**
   * İşlem getir
   * @param args Argümanlar
   */
  private def getTransaction(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("İşlem getirme işlevi henüz uygulanmadı.")
  }
  
  /**
   * İşlemleri listele
   * @param args Argümanlar
   */
  private def listTransactions(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("İşlem listeleme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Bekleyen işlemleri listele
   */
  private def listPendingTransactions(): Unit = {
    // İşlevsellik burada uygulanacak
    println("Bekleyen işlemleri listeleme işlevi henüz uygulanmadı.")
  }
  
  /**
   * İşlem ücretini göster
   * @param args Argümanlar
   */
  private def showTransactionFee(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("İşlem ücreti gösterme işlevi henüz uygulanmadı.")
  }
  
  /**
   * İşlem havuzunu göster
   * @param args Argümanlar
   */
  private def showTransactionPool(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("İşlem havuzu gösterme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Ağ komutlarını işle
   * @param args Argümanlar
   */
  private def handleNetworkCommands(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Ağ komutları işlevi henüz uygulanmadı.")
  }
  
  /**
   * Eşleri listele
   */
  private def listPeers(): Unit = {
    // İşlevsellik burada uygulanacak
    println("Eş listeleme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Eşe bağlan
   * @param args Argümanlar
   */
  private def connectToPeer(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Eşe bağlanma işlevi henüz uygulanmadı.")
  }
  
  /**
   * Blok komutlarını işle
   * @param args Argümanlar
   */
  private def handleBlockCommands(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Blok komutları işlevi henüz uygulanmadı.")
  }
  
  /**
   * Blok getir
   * @param args Argümanlar
   */
  private def getBlock(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Blok getirme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Blok oluştur
   */
  private def createBlock(): Unit = {
    // İşlevsellik burada uygulanacak
    println("Blok oluşturma işlevi henüz uygulanmadı.")
  }
  
  /**
   * Stake komutlarını işle
   * @param args Argümanlar
   */
  private def handleStakeCommands(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Stake komutları işlevi henüz uygulanmadı.")
  }
  
  /**
   * Doğrulayıcıları listele
   */
  private def listValidators(): Unit = {
    // İşlevsellik burada uygulanacak
    println("Doğrulayıcı listeleme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Doğrulayıcı olarak kaydol
   * @param args Argümanlar
   */
  private def registerValidator(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Doğrulayıcı kayıt işlevi henüz uygulanmadı.")
  }
  
  /**
   * Stake ekle
   * @param args Argümanlar
   */
  private def addStake(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Stake ekleme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Stake çek
   * @param args Argümanlar
   */
  private def unstake(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Stake çekme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Ödül komutlarını işle
   * @param args Argümanlar
   */
  private def handleRewardCommands(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Ödül komutları işlevi henüz uygulanmadı.")
  }
  
  /**
   * Ödülleri göster
   * @param args Argümanlar
   */
  private def getRewards(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Ödül gösterme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Blockchain'i izle
   * @param args Argümanlar
   */
  private def watchBlockchain(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Blockchain izleme işlevi henüz uygulanmadı.")
  }
  
  /**
   * İstatistikleri göster
   * @param args Argümanlar
   */
  private def showStatistics(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("İstatistik gösterme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Dashboard'u göster
   */
  private def showDashboard(): Unit = {
    // İşlevsellik burada uygulanacak
    println("Dashboard gösterme işlevi henüz uygulanmadı.")
  }
  
  /**
   * Veriyi dışa aktar
   * @param args Argümanlar
   */
  private def exportData(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Veri dışa aktarma işlevi henüz uygulanmadı.")
  }
  
  /**
   * Veriyi içe aktar
   * @param args Argümanlar
   */
  private def importData(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Veri içe aktarma işlevi henüz uygulanmadı.")
  }
  
  /**
   * Yapılandırma komutlarını işle
   * @param args Argümanlar
   */
  private def handleConfigCommands(args: Array[String]): Unit = {
    // İşlevsellik burada uygulanacak
    println("Yapılandırma komutları işlevi henüz uygulanmadı.")
  }
  
  /**
   * Kaynakları temizle
   */
  private def cleanup(): Unit = {
    activeNode.foreach { node =>
      // Düğümü düzgün bir şekilde kapat
      // (Gerçekte burada network bağlantılarını kapatma, vb işlemler yapılır)
    }
  }
  
  // Yardımcı fonksiyonlar
  
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
   * Kelime kaydırma
   * @param text Metin
   * @param width Genişlik
   * @return Kaydırılmış metin
   */
  private def wordWrap(text: String, width: Int): Seq[String] = {
    if (text.length <= width) return Seq(text)
    
    val words = text.split("\\s+")
    val lines = mutable.ListBuffer[String]()
    var currentLine = new StringBuilder()
    
    words.foreach { word =>
      if (currentLine.length + word.length <= width) {
        if (currentLine.nonEmpty) currentLine.append(" ")
        currentLine.append(word)
      } else {
        lines += currentLine.toString
        currentLine = new StringBuilder(word)
      }
    }
    
    if (currentLine.nonEmpty) {
      lines += currentLine.toString
    }
    
    lines.toSeq
  }
} 