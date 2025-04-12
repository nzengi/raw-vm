package core

import scala.collection.mutable.{Map => MutableMap}
import scala.math._
import java.util.{Timer, TimerTask}
import java.time.Instant

/**
 * Gas fiyat bilgisi
 * @param basePrice Temel gas fiyatı
 * @param priorityFee Öncelik ücreti
 */
case class GasPrice(basePrice: Long, priorityFee: Long = 0) {
  /**
   * Toplam gas fiyatı
   * @return Temel fiyat + öncelik ücreti
   */
  def total: Long = basePrice + priorityFee

  /**
   * Gas fiyatını arttır
   * @param percentage Artış yüzdesi
   * @return Arttırılmış gas fiyatı
   */
  def increase(percentage: Double): GasPrice = {
    val newBasePrice = max(1L, (basePrice * (1 + percentage)).toLong)
    val newPriorityFee = max(0L, (priorityFee * (1 + percentage)).toLong)
    GasPrice(newBasePrice, newPriorityFee)
  }

  /**
   * Gas fiyatını azalt
   * @param percentage Azalış yüzdesi
   * @return Azaltılmış gas fiyatı
   */
  def decrease(percentage: Double): GasPrice = {
    val newBasePrice = max(1L, (basePrice * (1 - percentage)).toLong)
    val newPriorityFee = max(0L, (priorityFee * (1 - percentage)).toLong)
    GasPrice(newBasePrice, newPriorityFee)
  }
  
  /**
   * EIP-1559 uyumlu etkin gas fiyatını hesapla
   * @param networkBaseFee Ağ temel ücreti
   * @param maxFeePerGas Maksimum toplam fee/gas
   * @param maxPriorityFeePerGas Maksimum öncelik fee/gas
   * @return Etkin gas fiyatı
   */
  def getEffectivePrice(
    networkBaseFee: Long, 
    maxFeePerGas: Long = 0, 
    maxPriorityFeePerGas: Long = 0
  ): Long = {
    if (maxFeePerGas > 0) {
      // EIP-1559 tarzı fiyatlandırma
      val effectivePriorityFee = min(maxPriorityFeePerGas, maxFeePerGas - networkBaseFee)
      min(maxFeePerGas, networkBaseFee + max(0, effectivePriorityFee))
    } else {
      // Legacy fiyatlandırma
      total
    }
  }
  
  /**
   * Belirli bir ağ durumu için önerilen gas fiyatını oluştur
   * @param networkBaseFee Ağ temel ücreti
   * @param congestionLevel Sıkışıklık seviyesi (0.0-1.0)
   * @param isHighPriority Yüksek öncelikli mi?
   * @return Önerilen gas fiyatı
   */
  def withRecommendedPrices(
    networkBaseFee: Long,
    congestionLevel: Double,
    isHighPriority: Boolean = false
  ): GasPrice = {
    // Sıkışıklık seviyesine göre temel ücrete ekleme
    val baseFeeMultiplier = 1.0 + (congestionLevel * 0.5)
    val newBasePrice = (networkBaseFee * baseFeeMultiplier).toLong
    
    // Öncelik ücreti hesaplama
    val priorityMultiplier = if (isHighPriority) {
      2.0 + congestionLevel * 3.0 // Yüksek öncelik için daha büyük çarpan
    } else {
      1.0 + congestionLevel * 1.5
    }
    
    val newPriorityFee = (networkBaseFee * 0.1 * priorityMultiplier).toLong
    
    GasPrice(newBasePrice, newPriorityFee)
  }
  
  /**
   * Gas fiyatını verilen maksimum sınırlara göre ayarla
   * @param maxBasePrice Maksimum temel fiyat
   * @param maxPriorityFee Maksimum öncelik ücreti
   * @return Ayarlanmış gas fiyatı
   */
  def capPrices(maxBasePrice: Long, maxPriorityFee: Long): GasPrice = {
    GasPrice(
      min(basePrice, maxBasePrice),
      min(priorityFee, maxPriorityFee)
    )
  }
  
  /**
   * Bu fiyatın verilen diğer fiyattan yüksek olup olmadığını kontrol et
   * @param other Karşılaştırılacak diğer fiyat
   * @return Bu fiyat daha yüksekse true
   */
  def isHigherThan(other: GasPrice): Boolean = {
    total > other.total
  }
  
  /**
   * Bu fiyatın verilen diğer fiyata göre yüzde farkını hesapla
   * @param other Karşılaştırılacak diğer fiyat
   * @return Yüzde farkı
   */
  def percentageDifference(other: GasPrice): Double = {
    if (other.total == 0) 1.0
    else (total - other.total).toDouble / other.total
  }
}

/**
 * Gas kullanım sonucu
 * @param gasUsed Kullanılan gas miktarı
 * @param returnData Dönüş verisi (opsiyonel)
 * @param error Hata mesajı (opsiyonel)
 */
case class GasUsageResult(
  gasUsed: Long,
  returnData: Option[Array[Byte]] = None,
  error: Option[String] = None
) {
  /**
   * İşlemin başarısız olup olmadığını kontrol eder
   * @return İşlem başarısız ise true
   */
  def isFailure: Boolean = error.isDefined
  
  /**
   * İşlemin başarılı olup olmadığını kontrol eder
   * @return İşlem başarılı ise true
   */
  def isSuccess: Boolean = !isFailure
}

/**
 * Gas maliyeti hesaplayıcı
 */
object GasCalculator {
  // Sabit gas maliyetleri
  val BASE_TX_GAS = 21_000L // Temel işlem gas maliyeti
  val ZERO_BYTE_GAS = 4L // Sıfır bayt gas maliyeti
  val NON_ZERO_BYTE_GAS = 16L // Sıfır olmayan bayt gas maliyeti
  val NEW_ACCOUNT_GAS = 25_000L // Yeni hesap oluşturma gas maliyeti
  val CONTRACT_CREATION_GAS = 32_000L // Sözleşme oluşturma gas maliyeti
  val CALL_GAS = 700L // Çağrı gas maliyeti
  val VALUE_TRANSFER_GAS = 9_000L // Değer transfer gas maliyeti
  val MEMORY_GAS = 3L // Bellek gas maliyeti (bayt başına)
  val STORAGE_SLOT_SET_GAS = 20_000L // Depolama alanı ayarlama gas maliyeti
  val STORAGE_SLOT_RESET_GAS = 5_000L // Depolama alanı sıfırlama gas maliyeti
  val STORAGE_SLOT_CLEAR_REFUND = 15_000L // Depolama alanı temizleme iade miktarı

  // İşlem talimatları için gas maliyetleri
  val INSTRUCTION_GAS = Map(
    "ADD" -> 3L,
    "MUL" -> 5L,
    "SUB" -> 3L,
    "DIV" -> 5L,
    "SDIV" -> 5L,
    "MOD" -> 5L,
    "SMOD" -> 5L,
    "ADDMOD" -> 8L,
    "MULMOD" -> 8L,
    "EXP" -> 10L,
    "LT" -> 3L,
    "GT" -> 3L,
    "SLT" -> 3L,
    "SGT" -> 3L,
    "EQ" -> 3L,
    "ISZERO" -> 3L,
    "AND" -> 3L,
    "OR" -> 3L,
    "XOR" -> 3L,
    "NOT" -> 3L,
    "BYTE" -> 3L,
    "SHL" -> 3L,
    "SHR" -> 3L,
    "SAR" -> 3L,
    "SHA3" -> 30L,
    "ADDRESS" -> 2L,
    "BALANCE" -> 700L,
    "ORIGIN" -> 2L,
    "CALLER" -> 2L,
    "CALLVALUE" -> 2L,
    "CALLDATALOAD" -> 3L,
    "CALLDATASIZE" -> 2L,
    "CALLDATACOPY" -> 3L,
    "CODESIZE" -> 2L,
    "CODECOPY" -> 3L,
    "GASPRICE" -> 2L,
    "EXTCODESIZE" -> 700L,
    "EXTCODECOPY" -> 700L,
    "RETURNDATASIZE" -> 2L,
    "RETURNDATACOPY" -> 3L,
    "EXTCODEHASH" -> 700L,
    "BLOCKHASH" -> 20L,
    "COINBASE" -> 2L,
    "TIMESTAMP" -> 2L,
    "NUMBER" -> 2L,
    "DIFFICULTY" -> 2L,
    "GASLIMIT" -> 2L,
    "CHAINID" -> 2L,
    "SELFBALANCE" -> 5L,
    "POP" -> 2L,
    "MLOAD" -> 3L,
    "MSTORE" -> 3L,
    "MSTORE8" -> 3L,
    "SLOAD" -> 800L,
    "SSTORE" -> 20000L,
    "JUMP" -> 8L,
    "JUMPI" -> 10L,
    "PC" -> 2L,
    "MSIZE" -> 2L,
    "GAS" -> 2L,
    "JUMPDEST" -> 1L,
    "PUSH1" -> 3L,
    "PUSH32" -> 3L,
    "DUP1" -> 3L,
    "DUP16" -> 3L,
    "SWAP1" -> 3L,
    "SWAP16" -> 3L,
    "LOG0" -> 375L,
    "LOG1" -> 750L,
    "LOG2" -> 1125L,
    "LOG3" -> 1500L,
    "LOG4" -> 1875L,
    "CREATE" -> 32000L,
    "CALL" -> 700L,
    "CALLCODE" -> 700L,
    "RETURN" -> 0L,
    "DELEGATECALL" -> 700L,
    "CREATE2" -> 32000L,
    "STATICCALL" -> 700L,
    "REVERT" -> 0L,
    "INVALID" -> 0L,
    "SELFDESTRUCT" -> 5000L
  )

  /**
   * İşlem için toplam gas maliyetini hesaplar
   * @param tx İşlem
   * @return Gas maliyeti
   */
  def calculateTransactionGas(tx: Transaction): Long = {
    var gas = BASE_TX_GAS

    // İşlem verisi için gas hesaplama
    gas += calculateDataGas(tx.data.getOrElse(Array.emptyByteArray))

    // İşlem türüne göre ek gas hesaplama
    if (tx.isContractCreation) {
      gas += CONTRACT_CREATION_GAS
    } else if (tx.isContractCall) {
      gas += CALL_GAS
      if (tx.value > 0) {
        gas += VALUE_TRANSFER_GAS
      }
    } else if (tx.isDelegateCall) {
      gas += CALL_GAS
    }

    gas
  }

  /**
   * İşlem verisi için gas maliyetini hesaplar
   * @param data İşlem verisi
   * @return Gas maliyeti
   */
  def calculateDataGas(data: Array[Byte]): Long = {
    var gas = 0L
    for (b <- data) {
      gas += (if (b == 0) ZERO_BYTE_GAS else NON_ZERO_BYTE_GAS)
    }
    gas
  }

  /**
   * Sözleşme yürütme gas maliyetini hesaplar
   * @param instructions Yürütülecek talimatlar
   * @return Gas maliyeti
   */
  def calculateExecutionGas(instructions: Seq[String]): Long = {
    instructions.map(i => INSTRUCTION_GAS.getOrElse(i, 0L)).sum
  }

  /**
   * Bellek kullanımı için gas maliyetini hesaplar
   * @param memorySize Bellek boyutu (bayt cinsinden)
   * @return Gas maliyeti
   */
  def calculateMemoryGas(memorySize: Long): Long = {
    val memWords = (memorySize + 31) / 32 // 32 baytlık kelimeler halinde yuvarla
    val memWordsSquared = memWords * memWords
    val memCost = memWords * MEMORY_GAS + memWordsSquared / 512
    memCost
  }

  /**
   * Depolama alanı kullanımı için gas maliyetini hesaplar
   * @param key Depolama anahtarı
   * @param newValue Yeni değer
   * @param oldValue Eski değer
   * @return (Gas maliyeti, Gas iadesi)
   */
  def calculateStorageGas(key: Array[Byte], newValue: Array[Byte], oldValue: Array[Byte]): (Long, Long) = {
    val isNewValueZero = newValue.forall(_ == 0)
    val isOldValueZero = oldValue.forall(_ == 0)

    if (isNewValueZero && !isOldValueZero) {
      // Sıfırlama, gas iadesi var
      (STORAGE_SLOT_RESET_GAS, STORAGE_SLOT_CLEAR_REFUND)
    } else if (!isNewValueZero && isOldValueZero) {
      // Yeni değer ayarlama
      (STORAGE_SLOT_SET_GAS, 0)
    } else if (!isNewValueZero && !isOldValueZero) {
      // Değer güncelleme
      (STORAGE_SLOT_RESET_GAS, 0)
    } else {
      // Zaten sıfır olan bir değeri sıfırlama girişimi
      (0L, 0L)
    }
  }

  /**
   * Çağrı işlemi için gas maliyetini hesaplar
   * @param target Hedef adres
   * @param value Transfer edilecek değer
   * @param data Çağrı verisi
   * @param delegateCall Yetkilendirilmiş çağrı mı?
   * @return Gas maliyeti
   */
  def calculateCallGas(target: String, value: Long, data: Array[Byte], delegateCall: Boolean): Long = {
    var gas = CALL_GAS

    // Değer transferi varsa ek gas hesaplama
    if (value > 0 && !delegateCall) {
      gas += VALUE_TRANSFER_GAS
    }

    // Çağrı verisi için gas hesaplama
    gas += calculateDataGas(data)

    gas
  }
}

/**
 * Dinamik gas fiyatı ayarlayıcı sınıf
 * @param initialGasPrice Başlangıç gas fiyatı
 * @param initialBaseFee Başlangıç temel ücreti
 * @param targetBlockTime Hedef blok oluşturma süresi (saniye cinsinden)
 * @param targetBlockGasLimit Hedef blok gas limiti
 * @param minGasPrice Minimum gas fiyatı
 * @param maxGasPrice Maksimum gas fiyatı
 * @param adjustmentPeriod Ayarlama periyodu (saniye cinsinden)
 */
class GasPriceAdjuster(
  initialGasPrice: GasPrice,
  initialBaseFee: Long = 10,
  targetBlockTime: Long = 15,
  targetBlockGasLimit: Long = 15_000_000,
  minGasPrice: Long = 1,
  maxGasPrice: Long = 1000,
  adjustmentPeriod: Long = 10
) {
  // Mevcut gas fiyatı
  private var currentGasPrice = initialGasPrice
  
  // Mevcut temel ücret (EIP-1559 için)
  private var currentBaseFee = initialBaseFee
  
  // Blok zamanları ve işlem sayıları
  private var blockTimes = List.empty[Long]
  private var blockTransactionCounts = List.empty[Int]
  private var blockGasUsed = List.empty[Long]
  
  // Geçmiş fiyat bilgisi
  private var priceHistory = List.empty[(Long, GasPrice, Long)] // (timestamp, price, baseFee)
  
  // İstatistikler
  private var peakNetworkLoad = 0.0
  private var currentNetworkLoad = 0.0
  
  // Son analiz zamanı
  private var lastAdjustmentTime = System.currentTimeMillis()
  
  // En fazla örnek sayısı
  private val maxSamples = 10 // Son 10 bloğu dikkate al

  // Ayarlama periyodu için zamanlayıcı
  private val timer = new Timer("GasPriceAdjuster", true)

  // Başlangıçta zamanlayıcıyı başlat
  timer.scheduleAtFixedRate(new TimerTask {
    override def run(): Unit = adjustGasPrice()
  }, adjustmentPeriod * 1000, adjustmentPeriod * 1000)

  /**
   * Yeni bir blok ekler
   * @param blockTime Blok oluşturma süresi (saniye cinsinden)
   * @param transactionCount Blok içindeki işlem sayısı
   * @param gasUsed Kullanılan gas miktarı
   */
  def newBlock(blockTime: Long, transactionCount: Int, gasUsed: Long): Unit = {
    blockTimes = (blockTime :: blockTimes).take(maxSamples)
    blockTransactionCounts = (transactionCount :: blockTransactionCounts).take(maxSamples)
    blockGasUsed = (gasUsed :: blockGasUsed).take(maxSamples)
    
    // Blok doluluk oranını hesapla ve ağ yük değerini güncelle
    val blockFillRate = Math.min(1.0, gasUsed.toDouble / targetBlockGasLimit)
    updateNetworkLoad(blockFillRate)
    
    // Temel ücreti ayarla (EIP-1559 benzeri)
    adjustBaseFee(blockFillRate)
    
    // Gas fiyat geçmişini güncelle
    priceHistory = ((System.currentTimeMillis(), currentGasPrice, currentBaseFee) :: priceHistory).take(100)
  }

  /**
   * Mevcut gas fiyatını döndürür
   * @return Mevcut gas fiyatı
   */
  def getCurrentGasPrice: GasPrice = currentGasPrice
  
  /**
   * Mevcut temel ücreti döndürür
   * @return Mevcut temel ücret
   */
  def getCurrentBaseFee: Long = currentBaseFee
  
  /**
   * Ağ yükünü güncelle
   * @param blockFillRate Blok doluluk oranı (0.0-1.0)
   */
  def updateNetworkLoad(blockFillRate: Double): Unit = {
    // Mevcut yükü, yeni değer ile karışım yaparak güncelle (%20 yeni, %80 eski)
    currentNetworkLoad = currentNetworkLoad * 0.8 + blockFillRate * 0.2
    
    // Zirve yükü güncelle
    if (currentNetworkLoad > peakNetworkLoad) {
      peakNetworkLoad = currentNetworkLoad
    }
  }
  
  /**
   * EIP-1559 benzeri temel ücret ayarlama
   * @param blockFillRate Blok doluluk oranı (0.0-1.0)
   */
  private def adjustBaseFee(blockFillRate: Double): Unit = {
    // Hedef doluluk oranı %50
    val targetFillRate = 0.5
    
    if (blockFillRate > targetFillRate) {
      // Blok dolu, temel ücreti artır
      val increaseFactor = Math.min(1.125, 1.0 + (blockFillRate - targetFillRate))
      currentBaseFee = (currentBaseFee * increaseFactor).toLong
    } else if (blockFillRate < targetFillRate) {
      // Blok boş, temel ücreti azalt
      val decreaseFactor = Math.max(0.875, 1.0 - (targetFillRate - blockFillRate))
      currentBaseFee = (currentBaseFee * decreaseFactor).toLong
    }
    
    // Temel ücretin minimum 1 olmasını sağla
    currentBaseFee = Math.max(1L, currentBaseFee)
  }

  /**
   * Gas fiyatını ayarlar
   */
  private def adjustGasPrice(): Unit = {
    if (blockTimes.isEmpty) return

    val now = System.currentTimeMillis()
    lastAdjustmentTime = now
    
    // Ortalama blok süresini hesapla
    val avgBlockTime = blockTimes.sum.toDouble / blockTimes.size

    // Ortalama işlem sayısını hesapla
    val avgTxCount = blockTransactionCounts.sum.toDouble / blockTransactionCounts.size
    
    // Ortalama gas kullanımını hesapla
    val avgGasUsed = if (blockGasUsed.nonEmpty) blockGasUsed.sum.toDouble / blockGasUsed.size else 0

    // Hedef blok süresinden sapma oranı
    val timeDeviation = avgBlockTime / targetBlockTime
    
    // Gas hedef kullanım oranı
    val gasUsageRatio = avgGasUsed / targetBlockGasLimit

    // Yeni gas fiyatını hesapla
    val basePriceChange = 
      if (timeDeviation > 1.25) {
        // Bloklar çok yavaş, fiyatı artır
        Math.min(0.1, (timeDeviation - 1.0) * 0.2)
      } else if (timeDeviation < 0.75) {
        // Bloklar çok hızlı, fiyatı düşür
        Math.max(-0.05, (timeDeviation - 1.0) * 0.1)
      } else if (gasUsageRatio < 0.3 && currentGasPrice.basePrice > minGasPrice * 2) {
        // Gas kullanımı az, fiyatı düşür
        -0.02
      } else if (gasUsageRatio > 0.8) {
        // Gas kullanımı yüksek, fiyatı artır
        0.05
      } else {
        // İdeal aralıkta, küçük bir ayarlama yap
        (gasUsageRatio - 0.5) * 0.04
      }
    
    // Öncelik ücreti değişimi (ağ yoğunluğuna dayanarak)
    val priorityFeeChange = 
      if (currentNetworkLoad > 0.8) {
        // Ağ yoğun, öncelik ücretini artır
        0.1
      } else if (currentNetworkLoad < 0.3 && currentGasPrice.priorityFee > 1) {
        // Ağ boş, öncelik ücretini düşür
        -0.05
      } else {
        // Normal yoğunluk, küçük ayarlama
        (currentNetworkLoad - 0.5) * 0.06
      }

    // Fiyatı güncelle
    updateGasPrice(basePriceChange, priorityFeeChange)
  }
  
  /**
   * Gas fiyatını güncelle
   * @param basePriceChangePercent Temel fiyat değişim yüzdesi
   * @param priorityFeeChangePercent Öncelik ücreti değişim yüzdesi
   */
  private def updateGasPrice(basePriceChangePercent: Double, priorityFeeChangePercent: Double): Unit = {
    // Temel fiyatı güncelle
    val newBasePrice = if (basePriceChangePercent > 0) {
      Math.round(currentGasPrice.basePrice * (1 + basePriceChangePercent))
    } else if (basePriceChangePercent < 0) {
      Math.round(currentGasPrice.basePrice * (1 + basePriceChangePercent))
    } else {
      currentGasPrice.basePrice
    }
    
    // Öncelik ücretini güncelle
    val newPriorityFee = if (priorityFeeChangePercent > 0) {
      Math.round(currentGasPrice.priorityFee * (1 + priorityFeeChangePercent))
    } else if (priorityFeeChangePercent < 0) {
      Math.round(currentGasPrice.priorityFee * (1 + priorityFeeChangePercent))
    } else {
      currentGasPrice.priorityFee
    }
    
    // Yeni gas fiyatını oluştur
    currentGasPrice = GasPrice(newBasePrice, newPriorityFee)
    
    // Minimum ve maksimum değerleri kontrol et
    ensureGasPriceInRange()
  }

  /**
   * Gas fiyatının belirlenen sınırlar içinde olmasını sağlar
   */
  private def ensureGasPriceInRange(): Unit = {
    // Temel fiyat sınırları
    val newBasePrice = Math.max(minGasPrice, Math.min(maxGasPrice, currentGasPrice.basePrice))
    
    // Öncelik ücreti sınırları (temel fiyatın %50'sinden fazla olmasın)
    val maxPriorityFee = Math.max(1, newBasePrice / 2)
    val newPriorityFee = Math.min(maxPriorityFee, currentGasPrice.priorityFee)
    
    currentGasPrice = GasPrice(newBasePrice, newPriorityFee)
  }
  
  /**
   * Önerilen gas fiyatını döndürür
   * @param priority Öncelik seviyesi (high, normal, low)
   * @return Önerilen gas fiyatı
   */
  def getRecommendedGasPrice(priority: String = "normal"): GasPrice = {
    val baseFee = currentBaseFee
    
    priority match {
      case "high" =>
        // Yüksek öncelik: Temel ücret + %100 öncelik ücreti
        GasPrice(baseFee, baseFee)
      case "normal" =>
        // Normal öncelik: Temel ücret + %30 öncelik ücreti
        GasPrice(baseFee, Math.max(1, (baseFee * 0.3).toLong))
      case "low" =>
        // Düşük öncelik: Sadece temel ücret + minimum öncelik
        GasPrice(baseFee, 1)
      case _ =>
        // Varsayılan: normal
        GasPrice(baseFee, Math.max(1, (baseFee * 0.3).toLong))
    }
  }
  
  /**
   * EIP-1559 tarzı işlem için önerilen gas parametrelerini döndürür
   * @param priority Öncelik seviyesi (high, normal, low)
   * @return (maxFeePerGas, maxPriorityFeePerGas) - maksimum ücret ve maksimum öncelik ücreti
   */
  def getEIP1559Parameters(priority: String = "normal"): (Long, Long) = {
    val baseFee = currentBaseFee
    
    // Temel ücretin değişim ihtimaline karşı buffer ekle
    val baseBuffer = priority match {
      case "high" => 2.0   // %100 buffer
      case "normal" => 1.5 // %50 buffer
      case "low" => 1.2    // %20 buffer
      case _ => 1.5
    }
    
    // Maksimum toplam fee/gas
    val maxFeePerGas = (baseFee * baseBuffer).toLong
    
    // Maksimum öncelik ücreti
    val maxPriorityFeePerGas = priority match {
      case "high" => Math.max(1, (baseFee * 1.0).toLong)
      case "normal" => Math.max(1, (baseFee * 0.3).toLong)
      case "low" => 1L
      case _ => Math.max(1, (baseFee * 0.3).toLong)
    }
    
    (maxFeePerGas, maxPriorityFeePerGas)
  }

  /**
   * Zamanlayıcıyı durdurur
   */
  def stop(): Unit = {
    timer.cancel()
  }
  
  /**
   * Gas fiyat istatistiklerini al
   * @return İstatistikler
   */
  def getStatistics: Map[String, Any] = {
    Map(
      "currentBasePrice" -> currentGasPrice.basePrice,
      "currentPriorityFee" -> currentGasPrice.priorityFee,
      "currentBaseFee" -> currentBaseFee,
      "currentNetworkLoad" -> currentNetworkLoad,
      "peakNetworkLoad" -> peakNetworkLoad,
      "avgBlockTime" -> (if (blockTimes.nonEmpty) blockTimes.sum.toDouble / blockTimes.size else 0),
      "avgTxCount" -> (if (blockTransactionCounts.nonEmpty) blockTransactionCounts.sum.toDouble / blockTransactionCounts.size else 0),
      "avgGasUsed" -> (if (blockGasUsed.nonEmpty) blockGasUsed.sum.toDouble / blockGasUsed.size else 0),
      "lastAdjustmentTime" -> lastAdjustmentTime
    )
  }
}

/**
 * Ücret yöneticisi sınıfı
 */
class FeeManager {
  // Validatör adresine göre toplanan ücretler
  private val collectedFees = MutableMap.empty[String, Long]
  
  // Toplam yakılan ücretler (EIP-1559 temel ücretleri için)
  private var burnedFees: Long = 0
  
  // Ağ istatistikleri
  private var totalTransactions: Long = 0
  private var totalGasUsed: Long = 0
  private var totalFeesCollected: Long = 0
  private var totalBaseFeesBurned: Long = 0
  private var totalPriorityFeesCollected: Long = 0
  
  // Tarihsel gas fiyatları (zaman, gas fiyatı, temel ücret)
  private val gasPriceHistory = MutableMap.empty[Long, (GasPrice, Long)]
  
  /**
   * İşlem ücretini ayırır ve dağıtır
   * @param tx İşlem
   * @param gasUsed Kullanılan gas miktarı
   * @param baseFee Mevcut temel ücret
   * @param blockProducer Blok üreticisi
   * @return (Toplam ücret, Blok üreticisinin payı, Yakılan miktar)
   */
  def distributeFee(
    tx: Transaction,
    gasUsed: Long,
    baseFee: Long,
    blockProducer: String
  ): (Long, Long, Long) = {
    // Etkili gas fiyatını hesapla
    val effectiveGasPrice = tx.fee.getEffectiveGasPrice(baseFee)
    
    // Toplam ücret
    val totalFee = gasUsed * effectiveGasPrice
    
    // Temel ücret kısmı (yakılacak veya ağa gidecek)
    val baseFeeAmount = gasUsed * baseFee
    
    // Öncelik ücreti (doğrulayıcıya gidecek)
    val priorityFeeAmount = totalFee - baseFeeAmount
    
    // Doğrulayıcı payını topla
    if (priorityFeeAmount > 0) {
      collectFee(blockProducer, priorityFeeAmount)
      totalPriorityFeesCollected += priorityFeeAmount
    }
    
    // Yakılan miktarı güncelle
    if (baseFeeAmount > 0) {
      burnedFees += baseFeeAmount
      totalBaseFeesBurned += baseFeeAmount
    }
    
    // İstatistikleri güncelle
    totalTransactions += 1
    totalGasUsed += gasUsed
    totalFeesCollected += totalFee
    
    (totalFee, priorityFeeAmount, baseFeeAmount)
  }

  /**
   * İşlem ücreti toplar
   * @param validator Bloku oluşturan validatör adresi
   * @param fee Toplanan ücret miktarı
   */
  def collectFee(validator: String, fee: Long): Unit = {
    val currentFee = collectedFees.getOrElse(validator, 0L)
    collectedFees.put(validator, currentFee + fee)
  }

  /**
   * Belirli bir validatör için toplanan ücretleri döndürür
   * @param validator Validatör adresi
   * @return Toplanan ücret miktarı
   */
  def getCollectedFees(validator: String): Long = {
    collectedFees.getOrElse(validator, 0L)
  }

  /**
   * Tüm toplanan ücretleri döndürür
   * @return (Validatör adresi, Toplanan ücret) haritası
   */
  def getAllCollectedFees: Map[String, Long] = {
    collectedFees.toMap
  }

  /**
   * Toplam ücret miktarını döndürür
   * @return Toplam ücret miktarı
   */
  def getTotalFees: Long = {
    collectedFees.values.sum
  }
  
  /**
   * Toplam yakılan ücret miktarını döndürür
   * @return Yakılan ücret miktarı
   */
  def getTotalBurnedFees: Long = {
    burnedFees
  }

  /**
   * Belirli bir validatör için toplanan ücretleri sıfırlar
   * @param validator Validatör adresi
   */
  def resetCollectedFees(validator: String): Unit = {
    collectedFees.put(validator, 0L)
  }

  /**
   * Tüm toplanan ücretleri sıfırlar
   */
  def resetAllCollectedFees(): Unit = {
    collectedFees.clear()
  }
  
  /**
   * Gas fiyat geçmişine yeni bir kayıt ekler
   * @param timestamp Zaman damgası
   * @param gasPrice Gas fiyatı
   * @param baseFee Temel ücret
   */
  def addGasPriceHistoryPoint(timestamp: Long, gasPrice: GasPrice, baseFee: Long): Unit = {
    gasPriceHistory.put(timestamp, (gasPrice, baseFee))
    
    // Geçmiş boyutunu sınırla (son 1000 kayıt)
    if (gasPriceHistory.size > 1000) {
      val oldestTime = gasPriceHistory.keys.min
      gasPriceHistory.remove(oldestTime)
    }
  }
  
  /**
   * Gas fiyat geçmişini alır
   * @param timeRange İsteğe bağlı zaman aralığı (başlangıç, bitiş)
   * @return Geçmiş kayıtları [(zaman, gas fiyatı, temel ücret)]
   */
  def getGasPriceHistory(timeRange: Option[(Long, Long)] = None): Seq[(Long, GasPrice, Long)] = {
    val records = timeRange match {
      case Some((start, end)) =>
        gasPriceHistory.filter { case (time, _) => time >= start && time <= end }
      case None =>
        gasPriceHistory
    }
    
    records.map { case (time, (gasPrice, baseFee)) => (time, gasPrice, baseFee) }.toSeq.sortBy(_._1)
  }
  
  /**
   * Ücret istatistiklerini alır
   * @return İstatistikler
   */
  def getFeeStats: Map[String, Long] = {
    Map(
      "totalTransactions" -> totalTransactions,
      "totalGasUsed" -> totalGasUsed,
      "totalFeesCollected" -> totalFeesCollected,
      "totalPriorityFeesCollected" -> totalPriorityFeesCollected,
      "totalBaseFeesBurned" -> totalBaseFeesBurned,
      "currentBurnedFees" -> burnedFees,
      "currentCollectedFees" -> getTotalFees
    )
  }
  
  /**
   * Ortalama gas fiyatını hesaplar
   * @param timeRange İsteğe bağlı zaman aralığı (son X saniye)
   * @return Ortalama gas fiyatı
   */
  def getAverageGasPrice(timeRange: Option[Long] = None): GasPrice = {
    val records = timeRange match {
      case Some(seconds) =>
        val threshold = System.currentTimeMillis() - seconds * 1000
        gasPriceHistory.filter { case (time, _) => time >= threshold }
      case None =>
        gasPriceHistory
    }
    
    if (records.isEmpty) {
      return GasPrice(1, 0) // Varsayılan
    }
    
    val totalBasePrice = records.values.map(_._1.basePrice).sum
    val totalPriorityFee = records.values.map(_._1.priorityFee).sum
    
    GasPrice(
      totalBasePrice / records.size, 
      totalPriorityFee / records.size
    )
  }
  
  /**
   * Ortalama temel ücreti hesaplar
   * @param timeRange İsteğe bağlı zaman aralığı (son X saniye)
   * @return Ortalama temel ücret
   */
  def getAverageBaseFee(timeRange: Option[Long] = None): Long = {
    val records = timeRange match {
      case Some(seconds) =>
        val threshold = System.currentTimeMillis() - seconds * 1000
        gasPriceHistory.filter { case (time, _) => time >= threshold }
      case None =>
        gasPriceHistory
    }
    
    if (records.isEmpty) {
      return 1 // Varsayılan
    }
    
    val totalBaseFee = records.values.map(_._2).sum
    totalBaseFee / records.size
  }
}

/**
 * Gas sistemi yöneticisi
 */
object GasSystem {
  private val gasPriceAdjuster = new GasPriceAdjuster(
    initialGasPrice = GasPrice(10, 1),
    initialBaseFee = 10,
    targetBlockTime = 15,
    targetBlockGasLimit = 15_000_000
  )
  
  private val feeManager = new FeeManager()
  
  // Maximum gas limit
  val MAX_GAS_LIMIT = 30_000_000
  
  /**
   * İşlem için gas maliyetini tahmin eder
   * @param tx İşlem
   * @return Tahmini gas maliyeti
   */
  def estimateGas(tx: Transaction): Long = {
    GasCalculator.calculateTransactionGas(tx)
  }
  
  /**
   * Mevcut gas fiyatını döndürür
   * @return Gas fiyatı
   */
  def getCurrentGasPrice: GasPrice = {
    gasPriceAdjuster.getCurrentGasPrice
  }
  
  /**
   * Mevcut temel ücreti (base fee) döndürür
   * @return Temel ücret
   */
  def getCurrentBaseFee: Long = {
    gasPriceAdjuster.getCurrentBaseFee
  }
  
  /**
   * Öncelik seviyesine göre önerilen gas fiyatını döndürür
   * @param priority Öncelik seviyesi ("high", "normal", "low")
   * @return Önerilen gas fiyatı
   */
  def getRecommendedGasPrice(priority: String = "normal"): GasPrice = {
    gasPriceAdjuster.getRecommendedGasPrice(priority)
  }
  
  /**
   * EIP-1559 tarzı işlem için önerilen gas parametrelerini döndürür
   * @param priority Öncelik seviyesi ("high", "normal", "low")
   * @return (maxFeePerGas, maxPriorityFeePerGas) - maksimum ücret ve maksimum öncelik ücreti
   */
  def getEIP1559Parameters(priority: String = "normal"): (Long, Long) = {
    gasPriceAdjuster.getEIP1559Parameters(priority)
  }
  
  /**
   * Tavsiye edilen işlem ücreti bilgilerini döndürür
   * @param gasEstimation Tahmini gas miktarı
   * @param priority Öncelik seviyesi ("high", "normal", "low")
   * @param useEIP1559 EIP-1559 kullanılsın mı?
   * @return İşlem ücreti bilgisi
   */
  def getRecommendedFee(
    gasEstimation: Long, 
    priority: String = "normal", 
    useEIP1559: Boolean = true
  ): TransactionFee = {
    val gasLimit = calculateGasLimit(gasEstimation)
    
    if (useEIP1559) {
      // EIP-1559 tarzı ücret
      val (maxFeePerGas, maxPriorityFeePerGas) = getEIP1559Parameters(priority)
      val recommendedGasPrice = getRecommendedGasPrice(priority)
      
      TransactionFee(
        gasLimit = gasLimit,
        gasPrice = recommendedGasPrice,
        maxFeePerGas = maxFeePerGas,
        maxPriorityFeePerGas = maxPriorityFeePerGas
      )
    } else {
      // Legacy tarzı ücret
      val recommendedGasPrice = getRecommendedGasPrice(priority)
      
      TransactionFee(
        gasLimit = gasLimit,
        gasPrice = recommendedGasPrice,
        maxFeePerGas = recommendedGasPrice.total,
        maxPriorityFeePerGas = 0
      )
    }
  }
  
  /**
   * Gas sınırını hesaplar (gas tahminine buffer ekler)
   * @param gasEstimate Tahmini gas miktarı
   * @return Gas sınırı
   */
  def calculateGasLimit(gasEstimate: Long): Long = {
    val buffer = 1.2 // %20 buffer
    val limit = (gasEstimate * buffer).toLong
    
    // Maksimum gas sınırını kontrol et
    Math.min(MAX_GAS_LIMIT, Math.max(21000L, limit))
  }
  
  /**
   * Yeni blok bilgisi ekler
   * @param blockTime Blok süresi
   * @param transactionCount İşlem sayısı
   * @param gasUsed Kullanılan gas miktarı
   */
  def addBlockInfo(blockTime: Long, transactionCount: Int, gasUsed: Long): Unit = {
    gasPriceAdjuster.newBlock(blockTime, transactionCount, gasUsed)
  }
  
  /**
   * Ağ yükünü güncelle
   * @param loadFactor Yük faktörü (0.0-1.0)
   */
  def updateNetworkLoad(loadFactor: Double): Unit = {
    gasPriceAdjuster.updateNetworkLoad(loadFactor)
  }
  
  /**
   * İşlem ücretini toplar ve dağıtır
   * @param tx İşlem
   * @param gasUsed Kullanılan gas
   * @param blockProducer Blok üreticisi
   * @param validators Doğrulayıcılar (opsiyonel)
   * @return Toplanan ücret
   */
  def collectFees(
    tx: Transaction, 
    gasUsed: Long, 
    blockProducer: String,
    validators: Seq[String] = Seq.empty
  ): Long = {
    // Mevcut temel ücreti al
    val baseFee = getCurrentBaseFee
    
    // Etkili gas fiyatını hesapla
    val effectiveGasPrice = tx.fee.getEffectiveGasPrice(baseFee)
    
    // Toplam ücreti hesapla
    val totalFee = gasUsed * effectiveGasPrice
    
    // Temel ücret payını hesapla (EIP-1559 tarzı yakılır veya ağ havuzuna gider)
    val basePortion = gasUsed * baseFee
    
    // Öncelik ücreti payını hesapla (doğrulayıcıya gider)
    val priorityPortion = totalFee - basePortion
    
    // Öncelik ücretini blok üreticisine ver
    if (priorityPortion > 0) {
      feeManager.collectFee(blockProducer, priorityPortion)
    }
    
    // Toplam ücret
    totalFee
  }
  
  /**
   * Doğrulayıcı için toplanan ücretleri döndürür
   * @param validator Doğrulayıcı adresi
   * @return Toplanan ücretler
   */
  def getValidatorFees(validator: String): Long = {
    feeManager.getCollectedFees(validator)
  }
  
  /**
   * Doğrulayıcı için toplanan ücretleri çeker
   * @param validator Doğrulayıcı adresi
   * @return Çekilen miktar
   */
  def withdrawValidatorFees(validator: String): Long = {
    val amount = feeManager.getCollectedFees(validator)
    feeManager.resetCollectedFees(validator)
    amount
  }
  
  /**
   * Gas istatistiklerini al
   * @return İstatistikler
   */
  def getGasStatistics: Map[String, Any] = {
    val adjusterStats = gasPriceAdjuster.getStatistics
    val totalFees = feeManager.getTotalFees
    
    Map(
      "currentGasPrice" -> getCurrentGasPrice,
      "currentBaseFee" -> getCurrentBaseFee,
      "totalCollectedFees" -> totalFees,
      "validatorFees" -> feeManager.getAllCollectedFees,
      "networkLoad" -> adjusterStats("currentNetworkLoad")
    ) ++ adjusterStats
  }
  
  /**
   * Blok doluluk oranından EIP-1559 temel ücret değişimini simüle eder
   * @param currentBaseFee Mevcut temel ücret
   * @param blockFillRate Blok doluluk oranı (0.0-1.0)
   * @return Sonraki blok için hesaplanan temel ücret
   */
  def simulateBaseFeeChange(currentBaseFee: Long, blockFillRate: Double): Long = {
    val targetFillRate = 0.5
    
    if (blockFillRate > targetFillRate) {
      // Doluluk hedefin üzerinde, ücret artar
      val increaseFactor = Math.min(1.125, 1.0 + (blockFillRate - targetFillRate))
      (currentBaseFee * increaseFactor).toLong
    } else if (blockFillRate < targetFillRate) {
      // Doluluk hedefin altında, ücret azalır
      val decreaseFactor = Math.max(0.875, 1.0 - (targetFillRate - blockFillRate))
      (currentBaseFee * decreaseFactor).toLong
    } else {
      // Doluluk tam hedefte, ücret değişmez
      currentBaseFee
    }
  }
} 