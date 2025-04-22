package core

import scala.collection.mutable
import java.security.MessageDigest
import java.util.{Base64, UUID}
import java.time.Instant

// Nesne Modeli
// Sui ve Move dilinin nesne modelinden esinlenilmiştir

// Nesne referansı - bir nesnenin benzersiz tanımlayıcısı ve versiyonu
case class ObjectRef(id: String, version: Long) {
  override def toString: String = s"$id#$version"
}

// Nesne meta verileri
case class ObjectMetadata(
  creator: String,     // Nesneyi oluşturan adres
  createdAt: Long,     // Oluşturulma zamanı
  owners: Set[String], // Nesnenin sahibi/sahipleri (paylaşımlı olabilir)
  typeTag: String,     // Nesnenin tipi
  isShared: Boolean,   // Paylaşımlı nesne mi?
  isMutable: Boolean   // Değiştirilebilir mi?
)

/**
 * Represents blockchain object data
 */
case class ObjectData(
  id: String,
  creator: String,
  typeTag: String,
  data: Array[Byte],
  owners: Set[String],
  version: Long = 0,
  createdAt: Long = Instant.now.getEpochSecond,
  lastModified: Long = Instant.now.getEpochSecond,
  isShared: Boolean = false,
  isMutable: Boolean = true
) {
  // Simplified equality for arrays
  override def equals(obj: Any): Boolean = obj match {
    case other: ObjectData =>
      id == other.id &&
      creator == other.creator &&
      typeTag == other.typeTag &&
      java.util.Arrays.equals(data, other.data) &&
      owners == other.owners &&
      version == other.version &&
      createdAt == other.createdAt &&
      lastModified == other.lastModified &&
      isShared == other.isShared &&
      isMutable == other.isMutable
    case _ => false
  }
  
  // Ensure hashCode is consistent with equals
  override def hashCode(): Int = {
    val prime = 31
    var result = 1
    result = prime * result + id.hashCode
    result = prime * result + creator.hashCode
    result = prime * result + typeTag.hashCode
    result = prime * result + java.util.Arrays.hashCode(data)
    result = prime * result + owners.hashCode
    result = prime * result + version.hashCode.toInt
    result = prime * result + createdAt.hashCode.toInt
    result = prime * result + lastModified.hashCode.toInt
    result = prime * result + isShared.hashCode
    result = prime * result + isMutable.hashCode
    result
  }
}

// Nesne deposu - blockchain sisteminin dünya durumunu (world state) yönetir
class ObjectStore {
  private val objects = mutable.Map[String, ObjectData]()
  private val versionsByType = mutable.Map[String, mutable.Set[String]]()
  
  // Geçmiş sürümleri depolar (ID -> Version -> ObjectData)
  private val objectHistory = mutable.Map[String, mutable.Map[Long, ObjectData]]()
  
  // Nesne ekleme
  def addObject(obj: ObjectData): Unit = {
    objects(obj.id) = obj
    
    val typeSet = versionsByType.getOrElseUpdate(obj.typeTag, mutable.Set.empty)
    typeSet.add(obj.id)
    
    // Geçmiş versiyonları kaydet
    val history = objectHistory.getOrElseUpdate(obj.id, mutable.Map.empty)
    history(obj.version) = obj
  }
  
  // ID'ye göre nesne alma
  def getObject(id: String): Option[ObjectData] = objects.get(id)
  
  // Tip etiketine göre nesneleri alma
  def getObjectsByType(typeTag: String): Seq[ObjectData] = {
    versionsByType.getOrElse(typeTag, mutable.Set.empty)
      .flatMap(id => objects.get(id))
      .toSeq
  }
  
  // Tüm nesneleri tip etiketine göre gruplandırarak alma
  def getAllObjectsByType(): Map[String, Seq[ObjectData]] = {
    versionsByType.keys.map { typeTag =>
      typeTag -> getObjectsByType(typeTag)
    }.toMap
  }
  
  // Nesneyi güncelleme (değişmez, yeni bir versiyon oluşturur)
  def updateObject(id: String, updateFn: ObjectData => Array[Byte]): Option[ObjectData] = {
    objects.get(id).map { obj =>
      if (!obj.isMutable) {
        throw new IllegalStateException(s"Object ${obj.id} is not mutable")
      }
      
      val newData = updateFn(obj)
      val newObj = obj.copy(data = newData, lastModified = Instant.now.getEpochSecond)
      
      // Yeni versiyonu nesne deposuna ekle
      objects(id) = newObj
      
      // Geçmiş versiyonu da kaydet
      val history = objectHistory.getOrElseUpdate(id, mutable.Map.empty)
      history(obj.version) = obj
      history(newObj.version) = newObj
      
      newObj
    }
  }
  
  // Nesne sahipliğini değiştirme
  def transferObject(id: String, newOwner: String): Option[ObjectData] = {
    objects.get(id).map { obj =>
      val newMetadata = obj.copy(owners = Set(newOwner))
      val newData = obj.data // veri değişmiyor, sadece meta veri
      val newObj = obj.copy(owners = Set(newOwner), lastModified = Instant.now.getEpochSecond)
      
      // Yeni versiyonu nesne deposuna ekle
      objects(id) = newObj
      
      // Geçmiş versiyonu da kaydet
      val history = objectHistory.getOrElseUpdate(id, mutable.Map.empty)
      history(obj.version) = obj
      history(newObj.version) = newObj
      
      newObj
    }
  }
  
  // Yeni bir nesne oluştur
  def createObject(
    creator: String,
    typeTag: String,
    data: Array[Byte],
    owners: Set[String],
    isShared: Boolean = false,
    isMutable: Boolean = true
  ): ObjectData = {
    val id = UUID.randomUUID().toString
    val ref = ObjectRef(id, 0) // İlk versiyon
    val metadata = ObjectMetadata(
      creator,
      System.currentTimeMillis(),
      owners,
      typeTag,
      isShared,
      isMutable
    )
    
    val obj = ObjectData(id, creator, typeTag, data, owners, 0, System.currentTimeMillis(), System.currentTimeMillis(), isShared, isMutable)
    addObject(obj)
    obj
  }
  
  // Geçmiş versiyona erişim (MVCC)
  def getObjectVersion(id: String, version: Long): Option[ObjectData] = {
    objectHistory.get(id).flatMap { versions =>
      versions.get(version)
    }
  }
  
  // Nesne geçmişini temizle (belirli bir versiyondan önceki tüm versiyonları sil)
  def pruneObjectHistory(id: String, beforeVersion: Long): Int = {
    objectHistory.get(id) match {
      case Some(versions) =>
        val keysToRemove = versions.keys.filter(_ < beforeVersion).toSeq
        keysToRemove.foreach(versions.remove)
        keysToRemove.size
      case None => 0
    }
  }
  
  // Belirli bir nesne tipinin tüm geçmişini temizle
  def pruneTypeHistory(typeTag: String, beforeVersion: Long): Int = {
    versionsByType.get(typeTag) match {
      case Some(idSet) =>
        idSet.map(id => pruneObjectHistory(id, beforeVersion)).sum
      case None => 0
    }
  }
} 