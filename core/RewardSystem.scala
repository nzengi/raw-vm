package core

import scala.collection.mutable
import java.time.{Instant, Duration}
import scala.util.{Try, Success, Failure}

/**
 * Ödül Sistemi
 * Doğrulayıcılar ve diğer ağ katılımcıları için token ödül mekanizması
 */
class RewardSystem {
  // Ödül türleri
  sealed trait RewardType
  case object BlockProduction extends RewardType
  case object Attestation extends RewardType
  case object NetworkParticipation extends RewardType
  case object SlashingReport extends RewardType
  case object GasRebate extends RewardType
  case object DevContribution extends RewardType
  
  // Ödül yapılandırması
  private var blockReward: Double = 2.0  // Blok üretimi için baz ödül
  private var attestationReward: Double = 0.1  // Doğrulama ödülü
  private var participationRate: Double = 0.01  // Ağ katılım ödül oranı
  private var slashingReportReward: Double = 0.5  // Cezalandırma bildirimi ödülü
  private var gasRebatePercentage: Double = 0.3  // Gas ücretlerinin iade yüzdesi
  private var devFundPercentage: Double = 0.1  // Geliştirme fonuna ayrılan yüzde
  
  // Ödül azaltma (halving) parametreleri
  private var blockRewardHalvingInterval: Int = 840000  // Yaklaşık 4 yıl (210000 * 4)
  private var blockRewardHalvingRate: Double = 0.5  // Her aralıkta ödülü %50 azalt
  
  // Doğrulayıcı istatistikleri
  private val validatorStats = mutable.Map[String, ValidatorStats]()
  
  // Toplam ödül istatistikleri
  private var totalBlockRewards: Double = 0.0
  private var totalAttestationRewards: Double = 0.0
  private var totalParticipationRewards: Double = 0.0
  private var totalSlashingRewards: Double = 0.0
  private var totalGasRebates: Double = 0.0
  private var totalDevFundContributions: Double = 0.0
  
  // Ödül geçmişi
  private val rewardHistory = mutable.ArrayBuffer.empty[RewardEvent]()
  private val epochRewards = mutable.Map[Int, EpochRewards]()
  
  /**
   * Blok ödülünü hesapla
   * @param blockHeight Blok yüksekliği
   * @param validatorAddress Doğrulayıcı adresi
   * @return Hesaplanan ödül
   */
  def calculateBlockReward(blockHeight: Int, validatorAddress: String): Double = {
    // Ödül azaltma (halving) hesaplaması
    val halvings = blockHeight / blockRewardHalvingInterval
    val adjustedReward = blockReward * Math.pow(blockRewardHalvingRate, halvings)
    
    // Doğrulayıcı performans faktörü
    val performanceFactor = getValidatorPerformanceFactor(validatorAddress)
    
    adjustedReward * performanceFactor
  }
  
  /**
   * Doğrulama (attestation) ödülünü hesapla
   * @param epochNumber Epoch numarası
   * @param validatorAddress Doğrulayıcı adresi
   * @param attestedBlockCount Doğrulanan blok sayısı
   * @param totalBlocksInEpoch Epoch'taki toplam blok sayısı
   * @return Hesaplanan ödül
   */
  def calculateAttestationReward(
    epochNumber: Int,
    validatorAddress: String,
    attestedBlockCount: Int,
    totalBlocksInEpoch: Int
  ): Double = {
    // Katılım oranı
    val participationRate = attestedBlockCount.toDouble / totalBlocksInEpoch
    
    // Baz ödül
    val baseReward = attestationReward * attestedBlockCount
    
    // Doğrulayıcı performans faktörü
    val performanceFactor = getValidatorPerformanceFactor(validatorAddress)
    
    baseReward * participationRate * performanceFactor
  }
  
  /**
   * Doğrulayıcı performans faktörünü hesapla
   * @param validatorAddress Doğrulayıcı adresi
   * @return Performans faktörü (0.5 - 1.2 arası)
   */
  private def getValidatorPerformanceFactor(validatorAddress: String): Double = {
    val stats = validatorStats.getOrElseUpdate(validatorAddress, new ValidatorStats(validatorAddress))
    
    // Uptime faktörü (0.5 - 1.0 arası)
    val uptimeFactor = 0.5 + (stats.uptime.min(99.9) / 100.0) * 0.5
    
    // Doğrulama oranı faktörü (0.0 - 0.1 arası ek bonus)
    val attestationRateFactor = (stats.attestationRate.min(100.0) / 100.0) * 0.1
    
    // Ceza faktörü (0.0 - 0.1 arası ceza)
    val slashingFactor = 0.1 * (1.0 - (stats.slashCount.min(10).toDouble / 10.0))
    
    uptimeFactor + attestationRateFactor - slashingFactor
  }
  
  /**
   * Ödül dağıt
   * @param rewardType Ödül türü
   * @param recipient Alıcı adresi
   * @param amount Ödül miktarı
   * @param blockHeight Blok yüksekliği
   * @param epochNumber Epoch numarası
   * @return Başarı/başarısızlık durumu
   */
  def distributeReward(
    rewardType: RewardType,
    recipient: String,
    amount: Double,
    blockHeight: Int,
    epochNumber: Int
  ): Try[RewardEvent] = {
    Try {
      // Ödül etkinliği oluştur
      val now = Instant.now()
      val rewardEvent = RewardEvent(
        rewardType = rewardType,
        recipient = recipient,
        amount = amount,
        timestamp = now,
        blockHeight = blockHeight,
        epochNumber = epochNumber
      )
      
      // Ödül geçmişine ekle
      rewardHistory.append(rewardEvent)
      
      // Doğrulayıcı istatistiklerini güncelle
      val stats = validatorStats.getOrElseUpdate(recipient, new ValidatorStats(recipient))
      rewardType match {
        case BlockProduction =>
          stats.blockCount += 1
          stats.totalBlockRewards += amount
          totalBlockRewards += amount
        case Attestation =>
          stats.attestationCount += 1
          stats.totalAttestationRewards += amount
          totalAttestationRewards += amount
        case NetworkParticipation =>
          stats.totalParticipationRewards += amount
          totalParticipationRewards += amount
        case SlashingReport =>
          stats.reportedSlashings += 1
          stats.totalSlashingRewards += amount
          totalSlashingRewards += amount
        case GasRebate =>
          stats.totalGasRebates += amount
          totalGasRebates += amount
        case DevContribution =>
          totalDevFundContributions += amount
      }
      
      // Epoch ödüllerini güncelle
      val epochReward = epochRewards.getOrElseUpdate(epochNumber, new EpochRewards(epochNumber))
      rewardType match {
        case BlockProduction => epochReward.blockRewards += amount
        case Attestation => epochReward.attestationRewards += amount
        case NetworkParticipation => epochReward.participationRewards += amount
        case SlashingReport => epochReward.slashingRewards += amount
        case GasRebate => epochReward.gasRebates += amount
        case DevContribution => epochReward.devContributions += amount
      }
      
      // Toplam epoch ödüllerini güncelle
      epochReward.totalRewards += amount
      
      // Olay geri dön
      rewardEvent
    }
  }
  
  /**
   * Ağ katılım ödülü hesapla ve dağıt
   * @param epoch Epoch numarası
   * @return Dağıtılan toplam ödül
   */
  def distributeParticipationRewards(epoch: Int): Double = {
    var totalDistributed = 0.0
    
    // Aktif doğrulayıcıları al
    val activeValidators = validatorStats.values.filter(_.active)
    if (activeValidators.isEmpty) return 0.0
    
    // Toplam stake ve performans puanlarını hesapla
    val totalStake = activeValidators.map(_.stakedAmount).sum
    val totalPerformancePoints = activeValidators.map { v =>
      v.attestationRate * v.uptime * (1.0 - v.slashCount.min(10).toDouble / 20.0)
    }.sum
    
    // Her doğrulayıcı için katılım ödülü hesapla
    activeValidators.foreach { validator =>
      // Performans puanı
      val performancePoints = validator.attestationRate * validator.uptime * 
                             (1.0 - validator.slashCount.min(10).toDouble / 20.0)
      
      // Stake ağırlığı
      val stakeWeight = validator.stakedAmount / totalStake
      
      // Performans ağırlığı
      val performanceWeight = if (totalPerformancePoints > 0) {
        performancePoints / totalPerformancePoints
      } else 0.0
      
      // Toplam ağırlık (%70 stake, %30 performans)
      val totalWeight = (stakeWeight * 0.7) + (performanceWeight * 0.3)
      
      // Ödül miktarı (toplam havuzun bir kısmı)
      val rewardAmount = participationRate * totalStake * totalWeight
      
      if (rewardAmount > 0) {
        distributeReward(
          NetworkParticipation,
          validator.address,
          rewardAmount,
          0, // Blok yüksekliği önemli değil
          epoch
        ) match {
          case Success(_) => totalDistributed += rewardAmount
          case Failure(_) => // Hata durumunda atla
        }
      }
    }
    
    totalDistributed
  }
  
  /**
   * Gas ücretlerinden rebate hesapla
   * @param validator Doğrulayıcı adresi
   * @param gasCollected Toplanan gas ücretleri
   * @param blockHeight Blok yüksekliği
   * @param epochNumber Epoch numarası
   * @return Rebate miktarı
   */
  def calculateGasRebate(
    validator: String,
    gasCollected: Double,
    blockHeight: Int,
    epochNumber: Int
  ): Double = {
    // Toplam rebate miktarı
    val totalRebate = gasCollected * gasRebatePercentage
    
    // Geliştirme fonuna katkı
    val devContribution = gasCollected * devFundPercentage
    
    // Geliştirme fonuna ödeme yap
    distributeReward(
      DevContribution,
      "dev_fund_address", // Geliştirme fonu adresi
      devContribution,
      blockHeight,
      epochNumber
    )
    
    // Doğrulayıcıya rebate ödemesi yap
    distributeReward(
      GasRebate,
      validator,
      totalRebate,
      blockHeight,
      epochNumber
    )
    
    totalRebate
  }
  
  /**
   * Cezalandırma (slashing) uygula
   * @param validator Doğrulayıcı adresi
   * @param reporter Raporlayan adresi (opsiyonel)
   * @param reason Cezalandırma sebebi
   * @param severity Ceza şiddeti (0.0-1.0 arası)
   * @param blockHeight Blok yüksekliği
   * @param epochNumber Epoch numarası
   * @return Başarı/başarısızlık durumu
   */
  def applySlashing(
    validator: String,
    reporter: Option[String],
    reason: String,
    severity: Double,
    blockHeight: Int,
    epochNumber: Int
  ): Try[Double] = {
    Try {
      val stats = validatorStats.getOrElseUpdate(validator, new ValidatorStats(validator))
      
      // Ceza miktarını hesapla (stake'in belirli bir yüzdesi)
      val slashAmount = stats.stakedAmount * severity.min(1.0).max(0.0)
      
      // Doğrulayıcı istatistiklerini güncelle
      stats.slashCount += 1
      stats.totalSlashed += slashAmount
      
      // Stake miktarını azalt
      stats.stakedAmount -= slashAmount
      
      // Raporlayan varsa ödüllendir
      reporter.foreach { addr =>
        // Ödül miktarı (ceza miktarının %5'i)
        val reporterReward = slashAmount * 0.05
        
        // Ödül dağıt
        distributeReward(
          SlashingReport,
          addr,
          reporterReward,
          blockHeight,
          epochNumber
        )
      }
      
      slashAmount
    }
  }
  
  /**
   * Ödül yapılandırmasını güncelle
   * @param newBlockReward Yeni blok ödülü
   * @param newAttestationReward Yeni doğrulama ödülü
   * @param newParticipationRate Yeni katılım ödül oranı
   * @param newSlashingReportReward Yeni cezalandırma bildirimi ödülü
   * @param newGasRebatePercentage Yeni gas iade yüzdesi
   * @param newDevFundPercentage Yeni geliştirme fonu yüzdesi
   * @param newBlockRewardHalvingInterval Yeni ödül azaltma aralığı
   * @param newBlockRewardHalvingRate Yeni ödül azaltma oranı
   */
  def updateConfiguration(
    newBlockReward: Option[Double] = None,
    newAttestationReward: Option[Double] = None,
    newParticipationRate: Option[Double] = None,
    newSlashingReportReward: Option[Double] = None,
    newGasRebatePercentage: Option[Double] = None,
    newDevFundPercentage: Option[Double] = None,
    newBlockRewardHalvingInterval: Option[Int] = None,
    newBlockRewardHalvingRate: Option[Double] = None
  ): Unit = {
    newBlockReward.foreach(blockReward = _)
    newAttestationReward.foreach(attestationReward = _)
    newParticipationRate.foreach(participationRate = _)
    newSlashingReportReward.foreach(slashingReportReward = _)
    newGasRebatePercentage.foreach(gasRebatePercentage = _)
    newDevFundPercentage.foreach(devFundPercentage = _)
    newBlockRewardHalvingInterval.foreach(blockRewardHalvingInterval = _)
    newBlockRewardHalvingRate.foreach(blockRewardHalvingRate = _)
  }
  
  /**
   * Doğrulayıcı istatistiklerini güncelle
   * @param address Doğrulayıcı adresi
   * @param uptime Çalışma süresi (yüzde)
   * @param attestationRate Doğrulama oranı (yüzde)
   * @param stakedAmount Stake edilmiş miktar
   * @param active Aktif durumu
   */
  def updateValidatorStats(
    address: String,
    uptime: Option[Double] = None,
    attestationRate: Option[Double] = None,
    stakedAmount: Option[Double] = None,
    active: Option[Boolean] = None
  ): Unit = {
    val stats = validatorStats.getOrElseUpdate(address, new ValidatorStats(address))
    
    uptime.foreach(stats.uptime = _)
    attestationRate.foreach(stats.attestationRate = _)
    stakedAmount.foreach(stats.stakedAmount = _)
    active.foreach(stats.active = _)
  }
  
  /**
   * Tüm doğrulayıcı istatistiklerini al
   * @return Doğrulayıcı istatistikleri
   */
  def getAllValidatorStats: Map[String, ValidatorStats] = validatorStats.toMap
  
  /**
   * Belirli bir epoch için ödül istatistiklerini al
   * @param epochNumber Epoch numarası
   * @return Epoch ödül istatistikleri
   */
  def getEpochRewards(epochNumber: Int): Option[EpochRewards] = epochRewards.get(epochNumber)
  
  /**
   * Son N epoch için ödül istatistiklerini al
   * @param count Epoch sayısı
   * @return Epoch ödül istatistikleri
   */
  def getRecentEpochRewards(count: Int): Seq[EpochRewards] = {
    epochRewards.values.toSeq.sortBy(-_.epochNumber).take(count)
  }
  
  /**
   * Belirli bir doğrulayıcının ödül geçmişini al
   * @param address Doğrulayıcı adresi
   * @param limit Maksimum kayıt sayısı
   * @return Ödül geçmişi
   */
  def getValidatorRewardHistory(address: String, limit: Int = 100): Seq[RewardEvent] = {
    rewardHistory.filter(_.recipient == address).takeRight(limit)
  }
  
  /**
   * Toplam ödül istatistiklerini al
   * @return İstatistik bilgileri
   */
  def getTotalRewardStats: Map[String, Double] = {
    Map(
      "totalBlockRewards" -> totalBlockRewards,
      "totalAttestationRewards" -> totalAttestationRewards,
      "totalParticipationRewards" -> totalParticipationRewards,
      "totalSlashingRewards" -> totalSlashingRewards,
      "totalGasRebates" -> totalGasRebates,
      "totalDevFundContributions" -> totalDevFundContributions,
      "totalRewards" -> (totalBlockRewards + totalAttestationRewards + 
                          totalParticipationRewards + totalSlashingRewards + 
                          totalGasRebates)
    )
  }
}

/**
 * Doğrulayıcı İstatistikleri
 * @param address Doğrulayıcı adresi
 */
class ValidatorStats(val address: String) {
  var uptime: Double = 100.0  // Çalışma süresi (yüzde)
  var attestationRate: Double = 100.0  // Doğrulama oranı (yüzde)
  var stakedAmount: Double = 0.0  // Stake edilmiş miktar
  var active: Boolean = true  // Aktif durumu
  
  // İstatistikler
  var blockCount: Int = 0  // Üretilen blok sayısı
  var attestationCount: Int = 0  // Doğrulama sayısı
  var slashCount: Int = 0  // Cezalandırma sayısı
  var reportedSlashings: Int = 0  // Raporlanan cezalandırma sayısı
  
  // Ödül istatistikleri
  var totalBlockRewards: Double = 0.0
  var totalAttestationRewards: Double = 0.0
  var totalParticipationRewards: Double = 0.0
  var totalSlashingRewards: Double = 0.0
  var totalGasRebates: Double = 0.0
  var totalSlashed: Double = 0.0
  
  // Toplam ödülleri hesapla
  def totalRewards: Double = totalBlockRewards + totalAttestationRewards + 
                             totalParticipationRewards + totalSlashingRewards + 
                             totalGasRebates
}

/**
 * Ödül Etkinliği
 * @param rewardType Ödül türü
 * @param recipient Alıcı adresi
 * @param amount Ödül miktarı
 * @param timestamp Zaman damgası
 * @param blockHeight Blok yüksekliği
 * @param epochNumber Epoch numarası
 */
case class RewardEvent(
  rewardType: RewardSystem#RewardType,
  recipient: String,
  amount: Double,
  timestamp: Instant,
  blockHeight: Int,
  epochNumber: Int
)

/**
 * Epoch Ödül İstatistikleri
 * @param epochNumber Epoch numarası
 */
class EpochRewards(val epochNumber: Int) {
  var blockRewards: Double = 0.0
  var attestationRewards: Double = 0.0
  var participationRewards: Double = 0.0
  var slashingRewards: Double = 0.0
  var gasRebates: Double = 0.0
  var devContributions: Double = 0.0
  var totalRewards: Double = 0.0
  
  var startTime: Instant = Instant.now()
  var endTime: Option[Instant] = None
  
  def duration: Duration = {
    val end = endTime.getOrElse(Instant.now())
    Duration.between(startTime, end)
  }
}

/**
 * Ödül Sistemi Yardımcı Sınıfı
 */
object RewardSystem {
  def apply(): RewardSystem = new RewardSystem()
} 