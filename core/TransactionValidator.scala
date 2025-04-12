package core

import scala.util.{Try, Success, Failure}
import java.security.PublicKey
import java.time.Instant

/**
 * İşlem doğrulama sonucu
 * @param isValid İşlem geçerli mi?
 * @param error Hata mesajı (başarısızlık durumunda)
 */
case class ValidationResult(
  isValid: Boolean,
  error: Option[String] = None
) {
  def isInvalid: Boolean = !isValid
}

/**
 * İşlemleri doğrulayan sınıf
 */
class TransactionValidator(
  val minGasPrice: Long = 1,
  val maxGasLimit: Long = 10000000L
) {
  // Benzersizlik kontrolü için kullanılan işlem kimlikleri (bellek içi)
  private val processedTransactions = scala.collection.mutable.Set[String]()
  private val processedNonces = scala.collection.mutable.Map[String, Long]() // sender -> son nonce
  
  /**
   * İşlemi doğrular
   * @param tx Doğrulanacak işlem
   * @param publicKeyResolver Gönderenin açık anahtarını bulan fonksiyon
   * @param balanceChecker Bakiye kontrolü yapan fonksiyon
   * @return Doğrulama sonucu
   */
  def validateTransaction(
    tx: Transaction,
    publicKeyResolver: String => Option[PublicKey],
    balanceChecker: String => Long
  ): ValidationResult = {
    // 1. İşlemin tekrarlanan bir işlem olup olmadığını kontrol et
    if (processedTransactions.contains(tx.id)) {
      return ValidationResult(isValid = false, Some("İşlem zaten işlendi"))
    }
    
    // 2. İmzayı doğrula
    val publicKeyOpt = publicKeyResolver(tx.sender)
    if (publicKeyOpt.isEmpty) {
      return ValidationResult(isValid = false, Some(s"Gönderen için açık anahtar bulunamadı: ${tx.sender}"))
    }
    
    if (!tx.verifySignature(publicKeyOpt.get)) {
      return ValidationResult(isValid = false, Some("Geçersiz işlem imzası"))
    }
    
    // 3. Nonce kontrolü
    val expectedNonce = processedNonces.getOrElse(tx.sender, 0L)
    if (tx.nonce < expectedNonce) {
      return ValidationResult(isValid = false, Some(s"Geçersiz nonce: beklenen >= $expectedNonce, alınan: ${tx.nonce}"))
    }
    
    // 4. Zaman kontrolü
    val currentTime = Instant.now.getEpochSecond
    if (tx.timestamp > currentTime + 300) { // 5 dakika tolerans
      return ValidationResult(isValid = false, Some("İşlem zaman damgası gelecekte"))
    }
    
    // 5. Gas limiti kontrolü
    if (tx.fee.gasLimit <= 0 || tx.fee.gasLimit > maxGasLimit) {
      return ValidationResult(isValid = false, Some(s"Geçersiz gas limiti: ${tx.fee.gasLimit}"))
    }
    
    // 6. Gas fiyatı kontrolü
    if (tx.fee.gasPrice.total < minGasPrice) {
      return ValidationResult(isValid = false, Some(s"Gas fiyatı çok düşük: ${tx.fee.gasPrice.total}, minimum: $minGasPrice"))
    }
    
    // 7. Maksimum fee/gas oranı kontrolü
    if (tx.fee.maxFeePerGas < tx.fee.gasPrice.total) {
      return ValidationResult(isValid = false, Some(s"Maksimum fee/gas oranı geçersiz: ${tx.fee.maxFeePerGas} < ${tx.fee.gasPrice.total}"))
    }
    
    // 8. Bakiye kontrolü - tahmini ücret + değer
    val balance = balanceChecker(tx.sender)
    val estimatedCost = tx.totalCost
    
    if (balance < estimatedCost) {
      return ValidationResult(isValid = false, Some(s"Yetersiz bakiye: $balance < $estimatedCost"))
    }
    
    // Tüm doğrulamalar başarılı
    ValidationResult(isValid = true)
  }
  
  /**
   * İşlemin bakiye ile ilgili bölümlerini doğrular
   * @param tx Doğrulanacak işlem
   * @param balanceChecker Bakiye kontrolü yapan fonksiyon
   * @return Doğrulama sonucu
   */
  def validateBalance(tx: Transaction, balanceChecker: String => Long): ValidationResult = {
    val balance = balanceChecker(tx.sender)
    val estimatedCost = tx.totalCost
    
    if (balance < estimatedCost) {
      ValidationResult(isValid = false, Some(s"Yetersiz bakiye: $balance < $estimatedCost"))
    } else {
      ValidationResult(isValid = true)
    }
  }
  
  /**
   * İşlemin gas değerlerini doğrular
   * @param tx Doğrulanacak işlem
   * @return Doğrulama sonucu
   */
  def validateGasParameters(tx: Transaction): ValidationResult = {
    // Gas limiti kontrolü
    if (tx.fee.gasLimit <= 0 || tx.fee.gasLimit > maxGasLimit) {
      return ValidationResult(isValid = false, Some(s"Geçersiz gas limiti: ${tx.fee.gasLimit}"))
    }
    
    // Gas fiyatı kontrolü
    if (tx.fee.gasPrice.total < minGasPrice) {
      return ValidationResult(isValid = false, Some(s"Gas fiyatı çok düşük: ${tx.fee.gasPrice.total}, minimum: $minGasPrice"))
    }
    
    // Maksimum fee/gas oranı kontrolü
    if (tx.fee.maxFeePerGas < tx.fee.gasPrice.total) {
      return ValidationResult(isValid = false, Some(s"Maksimum fee/gas oranı geçersiz: ${tx.fee.maxFeePerGas} < ${tx.fee.gasPrice.total}"))
    }
    
    ValidationResult(isValid = true)
  }
  
  /**
   * İşlemin gas maliyetini tahmin eder
   * @param tx Tahmini yapılacak işlem
   * @return Tahmini gas maliyeti
   */
  def estimateGasForTransaction(tx: Transaction): Long = {
    GasCalculator.calculateTransactionGas(tx)
  }
  
  /**
   * İşlemi başarıyla işlenmiş olarak kaydeder
   * @param tx İşlenen işlem
   */
  def markAsProcessed(tx: Transaction): Unit = {
    processedTransactions.add(tx.id)
    processedNonces.update(tx.sender, tx.nonce + 1)
  }
  
  /**
   * Bir işlem havuzunda işlemleri önceliklendirir
   * @param transactions İşlem havuzu
   * @return Önceliklendirilmiş işlemler
   */
  def prioritizeTransactions(transactions: Seq[Transaction]): Seq[Transaction] = {
    // İşlemleri gas fiyatına göre sırala (yüksekten düşüğe)
    transactions.sortBy(tx => -tx.fee.gasPrice.total)
  }
}

/**
 * İşlem doğrulama yardımcı nesnesi
 */
object TransactionValidator {
  // Varsayılan doğrulayıcı
  private lazy val defaultValidator = new TransactionValidator()
  
  /**
   * Varsayılan doğrulayıcıyı döndürür
   * @return Varsayılan doğrulayıcı
   */
  def default: TransactionValidator = defaultValidator
  
  /**
   * Özel parametrelerle yeni bir doğrulayıcı oluşturur
   * @param minGasPrice Minimum gas fiyatı
   * @param maxGasLimit Maksimum gas limiti
   * @return Yeni doğrulayıcı
   */
  def create(minGasPrice: Long, maxGasLimit: Long): TransactionValidator = {
    new TransactionValidator(minGasPrice, maxGasLimit)
  }
} 