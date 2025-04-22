package core

/**
 * Slash conditions for validators
 * Contains rules for when validators should be punished for bad behavior
 */
case class SlashingConditions(
  // Double signing penalty (percent of stake)
  doubleSignPenalty: Double = 0.1,
  
  // Unavailability penalty (percent of stake)
  unavailabilityPenalty: Double = 0.01,
  
  // Block withholding penalty
  blockWithholdingPenalty: Double = 0.05,
  
  // Violation threshold before slashing
  violationThreshold: Int = 3,
  
  // Maximum offline time before penalty (in seconds)
  maxOfflineTime: Long = 3600,
  
  // Time window for detecting double signing (in seconds)
  doubleSignWindow: Long = 86400
) {
  // Check if validator should be slashed for double signing
  def shouldSlashForDoubleSigning(violations: Int): Boolean = 
    violations >= violationThreshold
  
  // Check if validator should be slashed for unavailability
  def shouldSlashForUnavailability(offlineSeconds: Long): Boolean = 
    offlineSeconds > maxOfflineTime
  
  // Calculate slashing amount based on stake and violation type
  def calculateSlashAmount(stake: Long, violationType: String): Long = {
    violationType match {
      case "double_sign" => (stake * doubleSignPenalty).toLong
      case "unavailable" => (stake * unavailabilityPenalty).toLong
      case "withholding" => (stake * blockWithholdingPenalty).toLong
      case _ => 0L
    }
  }
}

// Companion object
object SlashingConditions {
  def apply(): SlashingConditions = new SlashingConditions()
} 