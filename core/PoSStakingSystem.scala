/**
 * DEVRE DIŞI - MODEL ÇAKIŞMALARI
 * 
 * Bu dosya core.PoSStaking ile model çakışmaları var.
 * Bu dosyanın içeriği PoSStaking.scala dosyasına entegre edilmelidir.
 * Derleyici hatalarını önlemek için devre dışı bırakıldı.
 */

/*
package core

import scala.collection.mutable
import scala.util.Random
import java.security.MessageDigest
import java.nio.ByteBuffer
import java.util.UUID

// Doğrulayıcı durumu
case class ValidatorStatus(
  isActive: Boolean = true,
  lastActiveTime: Long = 0,
  lastProposedBlock: Long = 0,
  proposedBlockCount: Long = 0,
  missedBlockCount: Long = 0,
  totalRewards: Long = 0,
  jailedUntil: Option[Long] = None
)

// Doğrulayıcı
case class Validator(
  id: String,
  name: String,
  publicKey: Array[Byte],
  stake: Long,
  commissionRate: Double,
  status: ValidatorStatus = ValidatorStatus(),
  createdAt: Long = System.currentTimeMillis(),
  lastUpdated: Long = System.currentTimeMillis()
)

// Stake bilgisi
case class Stake(
  id: String,
  stakerId: String,
  validatorId: String,
  amount: Long,
  createdAt: Long = System.currentTimeMillis(),
  unbondingHeight: Option[Long] = None,
  unbondingTime: Option[Long] = None,
  isActive: Boolean = true
)

// PoS Staking Sistemi
class PoSStakingSystem {
  // Burada tüm sınıfın içeriği var
}
*/ 