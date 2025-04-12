/**
 * Transaction (İşlem) modeli ve ilgili operasyonlar.
 * Gas fiyatlandırma sistemi entegre edilmiştir.
 */
package core.tx

import java.security.{KeyPair, PrivateKey, PublicKey, Signature}
import java.nio.charset.StandardCharsets
import java.util.{Base64, UUID}
import java.time.Instant
import crypto.Crypto
import scala.util.{Try, Success, Failure}
import crypto.{HashUtils, SignatureUtils}
import core.GasPrice

/**
 * İşlem ücret bilgisi
 * @param gasLimit İşlem için ayrılan maksimum gas miktarı
 * @param gasPrice Gas fiyatı
 * @param maxFeePerGas İzin verilen maksimum fee/gas oranı
 * @param maxPriorityFeePerGas Maksimum öncelik ücreti
 */
case class TransactionFee(
  gasLimit: Long, 
  gasPrice: GasPrice, 
  maxFeePerGas: Long,
  maxPriorityFeePerGas: Long = 0
) {
  /**
   * Tahmini işlem ücreti
   * @return Tahmini ücret
   */
  def estimatedFee: Long = gasLimit * gasPrice.total

  /**
   * Maksimum ödeme yapılabilecek işlem ücreti
   * @return Maksimum ücret
   */
  def maxFee: Long = gasLimit * maxFeePerGas

  /**
   * Maksimum öncelik ücreti
   * @return Öncelik ücreti
   */
  def maxPriorityFee: Long = gasLimit * maxPriorityFeePerGas
  
  /**
   * Maksimum işlem maliyetini hesaplar
   * @return İşlemin maksimum maliyeti
   */
  def maxCost: Long = gasLimit * Math.max(gasPrice.total, maxFeePerGas)
  
  /**
   * EIP-1559 formatına göre işlem ücretini hesaplar
   * @param baseFee Blok temel ücreti
   * @return Toplam işlem ücreti
   */
  def getEffectiveGasPrice(baseFee: Long): Long = {
    if (maxFeePerGas > 0) {
      // EIP-1559 tarzı işlem
      val priorityFee = Math.min(maxPriorityFeePerGas, maxFeePerGas - baseFee)
      Math.min(maxFeePerGas, baseFee + priorityFee)
    } else {
      // Legacy işlem
      gasPrice.total
    }
  }
  
  /**
   * Belirli bir gas kullanımı için toplam ücreti hesaplar
   * @param gasUsed Kullanılan gas miktarı
   * @param baseFee Blok temel ücreti
   * @return Toplam ücret
   */
  def calculateTotalFee(gasUsed: Long, baseFee: Long = 0): Long = {
    gasUsed * getEffectiveGasPrice(baseFee)
  }

  /**
   * Öncelik türünü belirler
   * @return Öncelik seviyesi (high, normal, low)
   */
  def getPriorityType: String = {
    val priorityRatio = maxPriorityFeePerGas.toDouble / gasPrice.basePrice
    if (priorityRatio > 1.5) "high"
    else if (priorityRatio > 0.5) "normal"
    else "low"
  }
  
  /**
   * İşlemin EIP-1559 uyumlu olup olmadığını kontrol eder
   * @return EIP-1559 uyumlu ise true
   */
  def isEIP1559Compatible: Boolean = maxFeePerGas > 0 && maxPriorityFeePerGas > 0

  /**
   * Gaz fiyatını yükseltir
   * @param increasePercentage Artış yüzdesi
   * @return Yükseltilmiş ücret
   */
  def boostGasPrice(increasePercentage: Double): TransactionFee = {
    val boostedGasPrice = gasPrice.increase(increasePercentage)
    val boostedMaxFeePerGas = (maxFeePerGas * (1 + increasePercentage)).toLong
    val boostedMaxPriorityFeePerGas = (maxPriorityFeePerGas * (1 + increasePercentage)).toLong
    
    TransactionFee(
      gasLimit,
      boostedGasPrice,
      boostedMaxFeePerGas,
      boostedMaxPriorityFeePerGas
    )
  }
}

/**
 * İşlem durum bilgisi
 */
object TransactionStatus extends Enumeration {
  type TransactionStatus = Value
  val Pending, Confirmed, Failed, Reverted = Value
}

import TransactionStatus._

/**
 * İşlem sonucu bilgisi
 * @param txId İşlem kimliği
 * @param status İşlem durumu
 * @param gasUsed Kullanılan gas miktarı
 * @param fee Toplam ücret
 * @param error Hata mesajı (başarısızlık durumunda)
 * @param blockHeight İşlemin eklendiği blok yüksekliği
 * @param timestamp İşlem zaman damgası
 * @param baseFeeAtExecution İşlem sırasındaki temel ücret
 */
case class TransactionResult(
  txId: String,
  status: TransactionStatus,
  gasUsed: Long,
  fee: Long,
  error: Option[String] = None,
  blockHeight: Option[Long] = None,
  timestamp: Long = Instant.now.getEpochSecond,
  baseFeeAtExecution: Option[Long] = None
)

/**
 * İşlem türü
 */
object TransactionType {
  val TRANSFER = 0 // Değer transferi
  val CONTRACT_CREATION = 1 // Sözleşme oluşturma
  val CONTRACT_CALL = 2 // Sözleşme çağrısı
  val DELEGATE_CALL = 3 // Yetkilendirilmiş çağrı
}

/**
 * Blok zincirinde işlemleri temsil eden sınıf
 * @param nonce İşlem sırası (gönderen adres başına artan sayaç)
 * @param from Gönderen adresi
 * @param to Alıcı adresi (opsiyonel, yoksa sözleşme oluşturma işlemi)
 * @param value Transfer edilecek değer
 * @param fee İşlem ücreti
 * @param data İşlem verisi
 * @param signature İşlem imzası
 * @param timestamp İşlem zaman damgası
 * @param txType İşlem türü
 */
case class Transaction(
  nonce: Long,
  from: String,
  to: Option[String],
  value: Long,
  fee: TransactionFee,
  data: Option[Array[Byte]],
  signature: Option[Array[Byte]] = None,
  timestamp: Long = Instant.now.getEpochSecond,
  txType: Int = TransactionType.TRANSFER
) {
  
  /**
   * İşlem kimliğini hesaplar
   * @return İşlem kimliği (hash)
   */
  def id: String = {
    val dataStr = data.map(d => new String(d)).getOrElse("")
    val toStr = to.getOrElse("")
    val message = s"$from-$toStr-$value-$dataStr-$nonce-$timestamp-${fee.gasPrice.total}-${fee.gasLimit}-${fee.maxFeePerGas}-${fee.maxPriorityFeePerGas}"
    HashUtils.sha256(message)
  }
  
  /**
   * İşlemin sözleşme oluşturma işlemi olup olmadığını kontrol eder
   * @return Sözleşme oluşturma işlemi ise true
   */
  def isContractCreation: Boolean = {
    txType == TransactionType.CONTRACT_CREATION || to.isEmpty
  }
  
  /**
   * İşlemin sözleşme çağrısı olup olmadığını kontrol eder
   * @return Sözleşme çağrısı ise true
   */
  def isContractCall: Boolean = {
    txType == TransactionType.CONTRACT_CALL
  }
  
  /**
   * İşlemin yetkilendirilmiş çağrı olup olmadığını kontrol eder
   * @return Yetkilendirilmiş çağrı ise true
   */
  def isDelegateCall: Boolean = {
    txType == TransactionType.DELEGATE_CALL
  }
  
  /**
   * İşlemin başka bir işleme göre önceliğini belirler (gas fiyatına göre)
   * @param other Karşılaştırılacak işlem
   * @param baseFee Mevcut blok temel ücret değeri
   * @return Bu işlem daha yüksek önceliğe sahipse true
   */
  def hasHigherPriorityThan(other: Transaction, baseFee: Long = 0): Boolean = {
    // Önce gas fiyatlarını karşılaştır
    val thisPrice = fee.getEffectiveGasPrice(baseFee)
    val otherPrice = other.fee.getEffectiveGasPrice(baseFee)
    
    if (thisPrice > otherPrice) {
      return true
    } else if (thisPrice < otherPrice) {
      return false
    }
    
    // Gas fiyatları eşitse, nonce'a göre karşılaştır (daha düşük nonce'lu işlemler öncelikli)
    nonce < other.nonce
  }
  
  /**
   * İşlemin öncelik seviyesini belirler
   * @param baseFee Mevcut blok temel ücret değeri
   * @return Öncelik seviyesi (high, normal, low)
   */
  def getPriorityLevel(baseFee: Long = 0): String = {
    val effectiveGasPrice = fee.getEffectiveGasPrice(baseFee)
    
    // Gas fiyatını baz ücrete göre karşılaştır
    val ratio = if (baseFee > 0) effectiveGasPrice.toDouble / baseFee else 1.0
    
    if (ratio >= 1.5) "high"
    else if (ratio >= 1.0) "normal"
    else "low"
  }
  
  /**
   * İşlemin geçerli bir imzaya sahip olup olmadığını kontrol eder
   * @return İmza geçerliyse true
   */
  def hasValidSignature: Boolean = {
    signature match {
      case Some(sig) => Crypto.verifySignature(from, id, sig)
      case None => false
    }
  }
  
  /**
   * İşlemi belirtilen özel anahtarla imzalar
   * @param privateKey İmza için kullanılacak özel anahtar
   * @return İmzalanmış işlem
   */
  def sign(privateKey: Array[Byte]): Transaction = {
    val sig = Crypto.sign(id, privateKey)
    this.copy(signature = Some(sig))
  }
  
  /**
   * İşlemin gas maliyetini tahmin eder
   * @return Tahmini gas maliyeti
   */
  def estimateGas: Long = {
    var gas = 21000L // Temel işlem gas maliyeti
    
    // Veri boyutuna göre gas ekleme
    val dataBytes = data.getOrElse(Array.emptyByteArray)
    var zeroBytesCount = 0
    var nonZeroBytesCount = 0
    
    for (b <- dataBytes) {
      if (b == 0) zeroBytesCount += 1
      else nonZeroBytesCount += 1
    }
    
    gas += zeroBytesCount * 4L // Sıfır byte'lar için
    gas += nonZeroBytesCount * 16L // Sıfır olmayan byte'lar için
    
    // İşlem türüne göre ek gas ekleme
    if (isContractCreation) {
      gas += 32000L // Sözleşme oluşturma için ek gas
    } else if (isContractCall) {
      gas += 700L // Sözleşme çağrısı için ek gas
    } else if (isDelegateCall) {
      gas += 700L // Yetkilendirilmiş çağrı için ek gas
    }
    
    // Değer transferi için ek gas
    if (value > 0) {
      gas += 9000L
    }
    
    gas
  }
  
  /**
   * İşlemin toplam ücretini hesaplar
   * @param gasUsed Kullanılan gas miktarı
   * @param baseFee Mevcut blok temel ücret değeri
   * @return Toplam ücret
   */
  def calculateFee(gasUsed: Long, baseFee: Long = 0): Long = {
    val effectiveGasPrice = fee.getEffectiveGasPrice(baseFee)
    effectiveGasPrice * gasUsed
  }
  
  /**
   * Gas fiyatı artırılmış bir işlem kopyası oluşturur
   * @param increasePercentage Artış yüzdesi
   * @return Yeni işlem
   */
  def withIncreasedGasPrice(increasePercentage: Double): Transaction = {
    val newFee = fee.boostGasPrice(increasePercentage)
    this.copy(fee = newFee)
  }
}

/**
 * İşlem oluşturmak için yardımcı nesne
 */
object Transaction {
  /**
   * Yeni bir işlem oluşturur
   * @param sender Gönderen adresi
   * @param recipient Alıcı adresi
   * @param value İşlem tutarı
   * @param data İşlem verisi
   * @param fee İşlem ücret bilgisi
   * @param nonce İşlem sırası
   * @return Yeni işlem
   */
  def create(
    sender: String,
    recipient: String,
    value: Long,
    data: Array[Byte] = Array.emptyByteArray,
    fee: TransactionFee,
    nonce: Long
  ): Transaction = {
    Transaction(
      nonce = nonce,
      from = sender,
      to = Some(recipient),
      value = value,
      fee = fee,
      data = Some(data)
    )
  }
  
  /**
   * İmzalanmış bir işlem oluşturur
   * @param sender Gönderen adresi
   * @param recipient Alıcı adresi
   * @param value İşlem tutarı
   * @param data İşlem verisi
   * @param fee İşlem ücret bilgisi
   * @param nonce İşlem sırası
   * @param privateKey Gönderenin özel anahtarı
   * @return İmzalanmış işlem
   */
  def createAndSign(
    sender: String,
    recipient: String,
    value: Long,
    data: Array[Byte] = Array.emptyByteArray,
    fee: TransactionFee,
    nonce: Long,
    privateKey: PrivateKey
  ): Transaction = {
    val tx = create(sender, recipient, value, data, fee, nonce)
    tx.sign(privateKey.getEncoded)
  }
  
  /**
   * Standart bir transfer işlemi oluşturur
   * @param sender Gönderen adresi
   * @param recipient Alıcı adresi
   * @param value İşlem tutarı
   * @param fee İşlem ücret bilgisi
   * @param nonce İşlem sırası
   * @param privateKey Gönderenin özel anahtarı (opsiyonel)
   * @return Transfer işlemi
   */
  def transfer(
    sender: String,
    recipient: String,
    value: Long,
    fee: TransactionFee,
    nonce: Long,
    privateKey: Option[PrivateKey] = None
  ): Transaction = {
    val tx = create(sender, recipient, value, Array.emptyByteArray, fee, nonce)
    privateKey.map(tx.sign).getOrElse(tx)
  }
  
  /**
   * EIP-1559 uyumlu işlem oluşturur
   * @param sender Gönderen adresi
   * @param recipient Alıcı adresi
   * @param value İşlem tutarı
   * @param data İşlem verisi
   * @param gasLimit Gas limiti
   * @param maxFeePerGas Maksimum fee/gas
   * @param maxPriorityFeePerGas Maksimum öncelik fee/gas
   * @param nonce İşlem sırası
   * @param privateKey Gönderenin özel anahtarı (opsiyonel)
   * @return EIP-1559 işlemi
   */
  def createEIP1559Transaction(
    sender: String,
    recipient: String,
    value: Long,
    data: Array[Byte] = Array.emptyByteArray,
    gasLimit: Long,
    maxFeePerGas: Long,
    maxPriorityFeePerGas: Long,
    nonce: Long,
    privateKey: Option[PrivateKey] = None
  ): Transaction = {
    // EIP-1559 işlemlerinde, gasPrice değeri ağın temel ücreti + öncelik ücreti olarak hesaplanacak
    // Başlangıçta temel ücret olmadığı için, maxFeePerGas değerini basePrice olarak kullan
    val gasPrice = GasPrice(maxFeePerGas - maxPriorityFeePerGas, maxPriorityFeePerGas)
    
    val fee = TransactionFee(
      gasLimit = gasLimit,
      gasPrice = gasPrice,
      maxFeePerGas = maxFeePerGas,
      maxPriorityFeePerGas = maxPriorityFeePerGas
    )
    
    val tx = Transaction(
      nonce = nonce,
      from = sender,
      to = Some(recipient),
      value = value,
      fee = fee,
      data = Some(data)
    )
    
    privateKey.map(tx.sign).getOrElse(tx)
  }
} 