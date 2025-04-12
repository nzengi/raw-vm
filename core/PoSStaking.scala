package core

import scala.collection.mutable
import java.security.MessageDigest
import java.util.{UUID, Base64}

// Stake ve doğrulayıcı yapıları

// Stake bilgisi - bir adresin kilitlenmiş tokenlarını temsil eder
case class Stake(
  stakerId: String,        // Stake sahibi
  amount: Long,            // Stake miktarı
  validatorId: String,     // Stake yapılan doğrulayıcı
  lockPeriod: Long,        // Kilitlenme süresi (saniye)
  startTime: Long,         // Başlangıç zamanı
  stakedAt: Long           // Stake edildiği blok yüksekliği
) {
  // Stake yaşı (ağırlık hesaplamasında kullanılır)
  def age(currentHeight: Long): Long = currentHeight - stakedAt
  
  // Stake çekilebilir mi?
  def isUnlockable(currentTime: Long): Boolean = {
    currentTime >= startTime + lockPeriod
  }
}

// Doğrulayıcı durumu
case class ValidatorStatus(
  isActive: Boolean,            // Aktif mi?
  isJailed: Boolean,            // Cezalandırılmış mı?
  lastProposedBlock: Long,      // Son blok önerdiği zaman
  proposedBlockCount: Long,     // Toplam önerdiği blok sayısı
  missedBlockCount: Long,       // Kaçırdığı blok sayısı
  slashCount: Int,              // Cezalandırılma sayısı
  uptimePercentage: Double      // Çalışma süresi yüzdesi
)

// Doğrulayıcı - ağdaki bir doğrulayıcıyı temsil eder
case class Validator(
  id: String,                           // Doğrulayıcı adresi/ID
  name: String,                         // Doğrulayıcı adı
  publicKey: Array[Byte],               // Doğrulama anahtarı
  commissionRate: Double,               // Komisyon oranı (0.0-1.0)
  totalStake: Long,                     // Toplam stake miktarı
  ownStake: Long,                       // Kendisinin stake ettiği miktar
  status: ValidatorStatus,              // Doğrulayıcı durumu
  createdAt: Long,                      // Doğrulayıcının oluşturulduğu zaman
  lastUpdated: Long                     // Son güncelleme zamanı
) {
  // Etkin stake (cezalar hesaba katılarak)
  def effectiveStake(slashFactor: Double): Long = {
    if (status.isJailed) 0
    else (totalStake * (1.0 - slashFactor * status.slashCount)).toLong
  }
  
  // Doğrulayıcının aktif olup olmadığını kontrol et
  def isActiveValidator(minStake: Long, currentTime: Long, maxInactivityPeriod: Long): Boolean = {
    status.isActive && 
    !status.isJailed && 
    totalStake >= minStake &&
    (currentTime - status.lastProposedBlock) < maxInactivityPeriod
  }
}

// PoS Stake Yönetim Sistemi
class PoSStakingSystem(
  val minStake: Long = 1000,            // Minimum stake miktarı
  val minValidatorStake: Long = 10000,  // Minimum doğrulayıcı stake miktarı
  val maxValidators: Int = 100,         // Maksimum doğrulayıcı sayısı
  val unstakingPeriod: Long = 86400 * 14, // Stake çekme süresi (14 gün)
  val slashingRate: Double = 0.1,       // Cezalandırma oranı
  val maxInactivityPeriod: Long = 86400 // Maksimum inaktiflik süresi (1 gün)
) {
  // Doğrulayıcılar
  private val validators = mutable.Map[String, Validator]()
  
  // Stake kayıtları
  private val stakes = mutable.Map[String, Stake]()
  
  // Bekleyen çekim talepleri (stakerId -> (stake referansı, çekim zamanı))
  private val pendingUnstakes = mutable.Map[String, List[(String, Long)]]()
  
  // Doğrulayıcı havuzu
  private var activeValidatorSet = mutable.Set[String]()
  
  // Son oy hakkı hesaplaması
  private var lastValidatorWeightUpdate: Long = 0
  private val validatorVotingPower = mutable.Map[String, Long]()
  
  // Yeni bir doğrulayıcı kaydet
  def registerValidator(
    id: String,
    name: String,
    publicKey: Array[Byte],
    initialStake: Long,
    commissionRate: Double,
    currentTime: Long,
    currentHeight: Long
  ): Either[String, Validator] = {
    if (validators.contains(id)) {
      Left(s"Validator with ID $id already exists")
    } else if (initialStake < minValidatorStake) {
      Left(s"Initial stake $initialStake is below minimum required validator stake $minValidatorStake")
    } else if (commissionRate < 0.0 || commissionRate > 1.0) {
      Left(s"Commission rate must be between 0 and 1")
    } else {
      // Yeni doğrulayıcı oluştur
      val status = ValidatorStatus(
        isActive = true,
        isJailed = false,
        lastProposedBlock = currentHeight,
        proposedBlockCount = 0,
        missedBlockCount = 0,
        slashCount = 0,
        uptimePercentage = 100.0
      )
      
      val validator = Validator(
        id = id,
        name = name,
        publicKey = publicKey,
        commissionRate = commissionRate,
        totalStake = initialStake,
        ownStake = initialStake,
        status = status,
        createdAt = currentTime,
        lastUpdated = currentTime
      )
      
      // Doğrulayıcının kendi stake'ini ekle
      val stakeId = UUID.randomUUID().toString
      val selfStake = Stake(
        stakerId = id,
        amount = initialStake,
        validatorId = id,
        lockPeriod = unstakingPeriod * 2, // Doğrulayıcılar için daha uzun kilit süresi
        startTime = currentTime,
        stakedAt = currentHeight
      )
      
      validators(id) = validator
      stakes(stakeId) = selfStake
      
      // Aktif doğrulayıcı setini güncelle
      updateValidatorSet(currentTime)
      
      Right(validator)
    }
  }
  
  // Doğrulayıcıya stake ekle
  def addStake(
    stakerId: String,
    validatorId: String,
    amount: Long,
    currentTime: Long,
    currentHeight: Long
  ): Either[String, Stake] = {
    if (amount < minStake) {
      Left(s"Stake amount $amount is below minimum required stake $minStake")
    } else if (!validators.contains(validatorId)) {
      Left(s"Validator $validatorId does not exist")
    } else {
      val validator = validators(validatorId)
      
      if (validator.status.isJailed) {
        Left(s"Validator $validatorId is jailed")
      } else {
        // Yeni stake oluştur
        val stakeId = UUID.randomUUID().toString
        val stake = Stake(
          stakerId = stakerId,
          amount = amount,
          validatorId = validatorId,
          lockPeriod = unstakingPeriod,
          startTime = currentTime,
          stakedAt = currentHeight
        )
        
        // Doğrulayıcının toplam stake'ini güncelle
        val updatedValidator = validator.copy(
          totalStake = validator.totalStake + amount,
          lastUpdated = currentTime
        )
        
        validators(validatorId) = updatedValidator
        stakes(stakeId) = stake
        
        // Aktif doğrulayıcı setini güncelle
        updateValidatorSet(currentTime)
        
        Right(stake)
      }
    }
  }
  
  // Stake çekme talebi
  def requestUnstake(
    stakerId: String,
    stakeId: String,
    currentTime: Long
  ): Either[String, Long] = {
    stakes.get(stakeId) match {
      case Some(stake) if stake.stakerId == stakerId =>
        // Doğrulayıcı kendi stake'ini çekiyorsa özel kontrol
        if (stake.stakerId == stake.validatorId) {
          val validator = validators(stake.validatorId)
          
          if (validator.totalStake - stake.amount < minValidatorStake) {
            return Left(s"Validator would fall below minimum required stake")
          }
        }
        
        // Çekim işlemini başlat
        val releaseTime = currentTime + stake.lockPeriod
        val pending = pendingUnstakes.getOrElse(stakerId, List.empty)
        pendingUnstakes(stakerId) = pending :+ (stakeId, releaseTime)
        
        Right(releaseTime)
        
      case Some(_) =>
        Left(s"Stake $stakeId does not belong to $stakerId")
        
      case None =>
        Left(s"Stake $stakeId does not exist")
    }
  }
  
  // Bekleyen stake çekimlerini tamamla
  def processUnstaking(currentTime: Long): Unit = {
    pendingUnstakes.foreach { case (stakerId, pendingList) =>
      // Tamamlanabilecek çekimleri bul
      val (completed, stillPending) = pendingList.partition { case (_, releaseTime) =>
        currentTime >= releaseTime
      }
      
      // Tamamlanan çekimleri işle
      completed.foreach { case (stakeId, _) =>
        stakes.get(stakeId).foreach { stake =>
          // Doğrulayıcının toplam stake'ini azalt
          validators.get(stake.validatorId).foreach { validator =>
            validators(stake.validatorId) = validator.copy(
              totalStake = validator.totalStake - stake.amount,
              ownStake = if (stake.stakerId == stake.validatorId) 
                           validator.ownStake - stake.amount 
                         else 
                           validator.ownStake,
              lastUpdated = currentTime
            )
          }
          
          // Stake kaydını kaldır
          stakes.remove(stakeId)
        }
      }
      
      // Bekleyen listeyi güncelle
      if (stillPending.isEmpty) {
        pendingUnstakes.remove(stakerId)
      } else {
        pendingUnstakes(stakerId) = stillPending
      }
    }
    
    // Aktif doğrulayıcı setini güncelle
    updateValidatorSet(currentTime)
  }
  
  // Doğrulayıcıyı cezalandır
  def slashValidator(
    validatorId: String,
    reason: String,
    amount: Option[Long],
    jail: Boolean,
    currentTime: Long
  ): Boolean = {
    validators.get(validatorId) match {
      case Some(validator) =>
        // Ceza miktarını hesapla
        val slashAmount = amount.getOrElse((validator.totalStake * slashingRate).toLong)
        
        // Doğrulayıcıyı güncelle
        val updatedStatus = validator.status.copy(
          isJailed = jail || validator.status.isJailed,
          slashCount = validator.status.slashCount + 1
        )
        
        val newStake = math.max(0, validator.totalStake - slashAmount)
        validators(validatorId) = validator.copy(
          totalStake = newStake,
          status = updatedStatus,
          lastUpdated = currentTime
        )
        
        // Aktif doğrulayıcı setini güncelle
        updateValidatorSet(currentTime)
        
        true
        
      case None =>
        false
    }
  }
  
  // Doğrulayıcıyı cezadan çıkar
  def unjailValidator(validatorId: String, currentTime: Long): Boolean = {
    validators.get(validatorId) match {
      case Some(validator) if validator.status.isJailed =>
        val updatedStatus = validator.status.copy(isJailed = false)
        validators(validatorId) = validator.copy(
          status = updatedStatus,
          lastUpdated = currentTime
        )
        
        // Aktif doğrulayıcı setini güncelle
        updateValidatorSet(currentTime)
        
        true
        
      case _ =>
        false
    }
  }
  
  // Aktif doğrulayıcı setini güncelle
  private def updateValidatorSet(currentTime: Long): Unit = {
    // Aktif doğrulayıcıları bul
    val activeValidators = validators.values
      .filter(_.isActiveValidator(minValidatorStake, currentTime, maxInactivityPeriod))
      .toSeq
      .sortBy(v => (-v.effectiveStake(slashingRate), v.id)) // Stake'e göre azalan sırada
      .take(maxValidators)
      .map(_.id)
      .toSet
    
    activeValidatorSet = mutable.Set.from(activeValidators)
    
    // Oy haklarını güncelle
    updateValidatorVotingPower(currentTime)
  }
  
  // Doğrulayıcıların oy haklarını güncelle
  private def updateValidatorVotingPower(currentTime: Long): Unit = {
    lastValidatorWeightUpdate = currentTime
    validatorVotingPower.clear()
    
    activeValidatorSet.foreach { validatorId =>
      validators.get(validatorId).foreach { validator =>
        // Oy hakkı = etkili stake
        val power = validator.effectiveStake(slashingRate)
        validatorVotingPower(validatorId) = power
      }
    }
  }
  
  // Doğrulayıcının konsensusa katılabilir olup olmadığını kontrol et
  def isEligibleForConsensus(validatorId: String, currentTime: Long): Boolean = {
    activeValidatorSet.contains(validatorId)
  }
  
  // Doğrulayıcının oy hakkını al
  def getValidatorVotingPower(validatorId: String): Long = {
    validatorVotingPower.getOrElse(validatorId, 0L)
  }
  
  // Aktif doğrulayıcıların listesini al
  def getActiveValidators: Set[String] = activeValidatorSet.toSet
  
  // Tüm doğrulayıcıların listesini al
  def getAllValidators: Map[String, Validator] = validators.toMap
  
  // Doğrulayıcının stake'ini al
  def getValidatorStake(validatorId: String): Option[Long] = {
    validators.get(validatorId).map(_.totalStake)
  }
  
  // Tur için doğrulayıcı seç (ağırlıklı rastgele)
  def selectProposer(seed: Array[Byte], currentTime: Long): Option[String] = {
    if (activeValidatorSet.isEmpty) return None
    
    // Toplam oy gücünü hesapla
    val totalVotingPower = activeValidatorSet.map(validatorVotingPower.getOrElse(_, 0L)).sum
    if (totalVotingPower <= 0) return None
    
    // Tohumu hash'le
    val digest = MessageDigest.getInstance("SHA-256").digest(seed)
    val randomValue = new java.math.BigInteger(1, digest).longValue().abs % totalVotingPower
    
    // Ağırlıklı seçim yap
    var cumulativePower: Long = 0
    for (validatorId <- activeValidatorSet) {
      val power = validatorVotingPower.getOrElse(validatorId, 0L)
      cumulativePower += power
      if (randomValue < cumulativePower) {
        return Some(validatorId)
      }
    }
    
    // Güvenlik için son validator'ü seç
    activeValidatorSet.lastOption
  }
} 