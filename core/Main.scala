package core

import scala.concurrent.ExecutionContext
import java.security.KeyPair
import scala.io.StdIn
import scala.util.{Try, Success, Failure}
import java.nio.file.{Files, Paths}
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, KeyPairGenerator, PrivateKey, PublicKey}

object Main {
  def main(args: Array[String]): Unit = {
    println("ZK-DAG-PoS Blockchain Node başlatılıyor...")
    
    // Komut satırı argümanlarını işle
    val (nodeId, endpoint, bootstrapNodes) = parseArgs(args)
    
    // Execution context
    implicit val ec = ExecutionContext.global
    
    // Anahtar çiftini oluştur veya yükle
    val keyPair = loadOrCreateKeyPair(nodeId)
    
    // Blockchain düğümünü oluştur
    val blockchain = new Blockchain(
      nodeId = nodeId,
      keyPair = keyPair,
      endpoint = endpoint,
      bootstrapNodes = bootstrapNodes,
      executionContext = ec
    )
    
    // Blockchain'i başlat
    blockchain.start()
    
    println(s"Düğüm başlatıldı: $nodeId @ $endpoint")
    if (bootstrapNodes.nonEmpty) {
      println(s"Bootstrap düğümleri: ${bootstrapNodes.mkString(", ")}")
    } else {
      println("Bu bir bootstrap düğümüdür (genesis)")
    }
    
    // Basit komut satırı arayüzü
    startCommandLineInterface(blockchain)
  }
  
  // Komut satırı argümanlarını ayrıştır
  private def parseArgs(args: Array[String]): (String, String, Seq[String]) = {
    // Varsayılan değerler
    var nodeId = s"node-${java.util.UUID.randomUUID().toString.substring(0, 8)}"
    var endpoint = "127.0.0.1:9000"
    var bootstrapNodes = Seq.empty[String]
    
    // Argümanları işle
    var i = 0
    while (i < args.length) {
      args(i) match {
        case "--node-id" if i + 1 < args.length =>
          nodeId = args(i + 1)
          i += 2
          
        case "--endpoint" if i + 1 < args.length =>
          endpoint = args(i + 1)
          i += 2
          
        case "--bootstrap" if i + 1 < args.length =>
          bootstrapNodes = args(i + 1).split(",").toSeq
          i += 2
          
        case other =>
          println(s"Bilinmeyen argüman: $other")
          printUsage()
          i += 1
      }
    }
    
    (nodeId, endpoint, bootstrapNodes)
  }
  
  // Kullanım bilgisi yazdır
  private def printUsage(): Unit = {
    println("""
      |Kullanım: 
      |  --node-id <id>      : Düğüm kimliği (varsayılan: rastgele UUID)
      |  --endpoint <adres>  : Bağlantı noktası (varsayılan: 127.0.0.1:9000)
      |  --bootstrap <liste> : Bootstrap düğümleri (virgülle ayrılmış)
    """.stripMargin)
  }
  
  // Anahtar çiftini yükle veya oluştur
  private def loadOrCreateKeyPair(nodeId: String): KeyPair = {
    val keyDir = Paths.get("keys")
    val privateKeyPath = keyDir.resolve(s"$nodeId.key")
    val publicKeyPath = keyDir.resolve(s"$nodeId.pub")
    
    // Dizini oluştur (yoksa)
    if (!Files.exists(keyDir)) {
      Files.createDirectories(keyDir)
    }
    
    // Anahtarları diskten yükleme veya yeni oluşturma
    if (Files.exists(privateKeyPath) && Files.exists(publicKeyPath)) {
      try {
        // Anahtarları dosyadan oku
        val privateKeyBytes = Files.readAllBytes(privateKeyPath)
        val publicKeyBytes = Files.readAllBytes(publicKeyPath)
        
        // KeyFactory ile anahtarları oluştur
        val keyFactory = KeyFactory.getInstance("RSA")
        val privateKey = keyFactory.generatePrivate(new PKCS8EncodedKeySpec(privateKeyBytes))
        val publicKey = keyFactory.generatePublic(new X509EncodedKeySpec(publicKeyBytes))
        
        // KeyPair oluştur
        val keyPair = new KeyPair(publicKey, privateKey)
        println(s"RSA anahtar çifti '$nodeId' için yüklendi")
        
        keyPair
      } catch {
        case e: Exception =>
          println(s"Anahtar yüklenirken hata oluştu: ${e.getMessage}")
          println("Yeni anahtar çifti oluşturuluyor...")
          createAndSaveKeyPair(nodeId, privateKeyPath, publicKeyPath)
      }
    } else {
      println(s"$nodeId için anahtar çifti bulunamadı. Yeni anahtar çifti oluşturuluyor...")
      createAndSaveKeyPair(nodeId, privateKeyPath, publicKeyPath)
    }
  }
  
  // Yeni bir key pair oluştur ve kaydet
  private def createAndSaveKeyPair(nodeId: String, privatePath: java.nio.file.Path, publicPath: java.nio.file.Path): KeyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA")
    keyGen.initialize(2048)
    val keyPair = keyGen.generateKeyPair()
    
    // Anahtarları diske kaydet
    Files.write(privatePath, keyPair.getPrivate.getEncoded)
    Files.write(publicPath, keyPair.getPublic.getEncoded)
    
    println(s"Yeni RSA anahtar çifti oluşturuldu ve kaydedildi: $nodeId")
    keyPair
  }
  
  // Basit komut satırı arayüzü
  private def startCommandLineInterface(blockchain: Blockchain): Unit = {
    var running = true
    
    while (running) {
      print("> ")
      val input = StdIn.readLine()
      
      input.split("\\s+").toList match {
        case "exit" :: _ =>
          running = false
          println("Düğüm kapatılıyor...")
          
        case "info" :: _ =>
          val info = blockchain.getChainInfo
          println("Blockchain Bilgisi:")
          info.foreach { case (key, value) =>
            println(s"  $key: $value")
          }
          
        case "validator" :: "register" :: name :: amount :: rate :: _ =>
          Try {
            val initialStake = amount.toLong
            val commissionRate = rate.toDouble
            
            blockchain.registerAsValidator(name, initialStake, commissionRate) match {
              case Right(validator) =>
                println(s"Doğrulayıcı başarıyla kaydedildi: ${validator.id}")
                println(s"  İsim: ${validator.name}")
                println(s"  İlk stake: ${validator.ownStake}")
                println(s"  Komisyon oranı: ${validator.commissionRate}")
                
              case Left(error) =>
                println(s"Doğrulayıcı kaydedilemedi: $error")
            }
          }.recover {
            case e: Exception => println(s"Geçersiz parametreler: ${e.getMessage}")
          }
          
        case "stake" :: validatorId :: amount :: _ =>
          Try {
            val stakeAmount = amount.toLong
            
            blockchain.stake(validatorId, stakeAmount) match {
              case Right(stake) =>
                println(s"Stake başarıyla eklendi: ${stake.amount}")
                println(s"  Validator: ${stake.validatorId}")
                println(s"  Kilit süresi: ${stake.lockPeriod / 86400} gün")
                
              case Left(error) =>
                println(s"Stake eklenemedi: $error")
            }
          }.recover {
            case e: Exception => println(s"Geçersiz parametreler: ${e.getMessage}")
          }
          
        case "object" :: "create" :: typeTag :: data :: _ =>
          val obj = blockchain.createObject(
            typeTag = typeTag,
            data = data.getBytes,
            isShared = false,
            isMutable = true
          )
          
          println(s"Nesne oluşturuldu: ${obj.ref.id}")
          println(s"  Tip: ${obj.metadata.typeTag}")
          println(s"  Sahibi: ${obj.metadata.owners.mkString(", ")}")
          
        case "object" :: "list" :: Nil =>
          // Nesneleri listele
          // Nesne deposundan bütün nesneleri al ve göster
          val objectsByType = blockchain.objectStore.getAllObjectsByType()
          
          if (objectsByType.isEmpty) {
            println("Henüz hiç nesne oluşturulmamış.")
          } else {
            println("Mevcut Nesneler:")
            objectsByType.foreach { case (typeTag, objects) =>
              println(s"\nTip: $typeTag (${objects.size} nesne)")
              objects.foreach { obj =>
                println(s"  ID: ${obj.ref.id} (v${obj.ref.version})")
                println(s"    Sahip: ${obj.metadata.owners.mkString(", ")}")
                println(s"    Oluşturan: ${obj.metadata.creator}")
                println(s"    Paylaşımlı: ${obj.metadata.isShared}")
                println(s"    Değiştirilebilir: ${obj.metadata.isMutable}")
                
                // Nesne boyutunu göster
                println(s"    Boyut: ${obj.data.length} byte")
              }
            }
          }
          
        case "object" :: "list" :: typeTag :: _ =>
          // Belirli tip için nesneleri listele
          val objects = blockchain.objectStore.getObjectsByType(typeTag)
          
          if (objects.isEmpty) {
            println(s"'$typeTag' tipinde nesne bulunamadı.")
          } else {
            println(s"\n'$typeTag' Tipindeki Nesneler (${objects.size} nesne):")
            objects.foreach { obj =>
              println(s"  ID: ${obj.ref.id} (v${obj.ref.version})")
              println(s"    Sahip: ${obj.metadata.owners.mkString(", ")}")
              println(s"    Oluşturan: ${obj.metadata.creator}")
              println(s"    Paylaşımlı: ${obj.metadata.isShared}")
              println(s"    Değiştirilebilir: ${obj.metadata.isMutable}")
              println(s"    Boyut: ${obj.data.length} byte")
            }
          }
          
        case "help" :: _ =>
          printHelp()
          
        case "" :: Nil =>
          // Boş komut
          
        case cmd :: _ =>
          println(s"Bilinmeyen komut: $cmd")
          println("Kullanılabilir komutlar için 'help' yazın.")
          
        case Nil =>
          // Boş satır
      }
    }
  }
  
  // Yardım bilgisi yazdır
  private def printHelp(): Unit = {
    println("""
      |Kullanılabilir Komutlar:
      |  info                               - Blockchain bilgisini göster
      |  validator register <isim> <miktar> <oran> - Doğrulayıcı olarak kaydol
      |  stake <validator-id> <miktar>      - Bir doğrulayıcıya stake ekle
      |  object create <tip> <veri>         - Yeni bir nesne oluştur
      |  object list                        - Tüm nesneleri listele
      |  object list <tip>                  - Belirli tipteki nesneleri listele
      |  help                               - Bu yardım mesajını göster
      |  exit                               - Programdan çık
    """.stripMargin)
  }
}