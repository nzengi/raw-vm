package core

import scala.collection.mutable
import java.security.MessageDigest
import java.util.{Base64, UUID}

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

// Blockchain üzerindeki veri nesnesi
case class ObjectData(
  ref: ObjectRef,
  metadata: ObjectMetadata,
  data: Array[Byte],   // Nesne içeriği (serileştirilmiş)
  previousVersions: Seq[ObjectRef] = Seq.empty  // MVCC için önceki versiyonlar
) {
  lazy val digest: String = {
    val content = s"${ref.toString}|${metadata.typeTag}|${data.length}"
    val hash = MessageDigest.getInstance("SHA-256").digest(content.getBytes) 
    Base64.getEncoder.encodeToString(hash)
  }
  
  // Yeni bir versiyon oluştur
  def withNewVersion(newData: Array[Byte]): ObjectData = {
    val newVersion = ref.version + 1
    val newRef = ObjectRef(ref.id, newVersion)
    ObjectData(
      newRef,
      metadata,
      newData,
      previousVersions :+ ref
    )
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
    objects(obj.ref.id) = obj
    
    val typeSet = versionsByType.getOrElseUpdate(obj.metadata.typeTag, mutable.Set.empty)
    typeSet.add(obj.ref.id)
    
    // Geçmiş versiyonları kaydet
    val history = objectHistory.getOrElseUpdate(obj.ref.id, mutable.Map.empty)
    history(obj.ref.version) = obj
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
      if (!obj.metadata.isMutable) {
        throw new IllegalStateException(s"Object ${obj.ref} is not mutable")
      }
      
      val newData = updateFn(obj)
      val newObj = obj.withNewVersion(newData)
      
      // Yeni versiyonu nesne deposuna ekle
      objects(id) = newObj
      
      // Geçmiş versiyonu da kaydet
      val history = objectHistory.getOrElseUpdate(id, mutable.Map.empty)
      history(obj.ref.version) = obj
      history(newObj.ref.version) = newObj
      
      newObj
    }
  }
  
  // Nesne sahipliğini değiştirme
  def transferObject(id: String, newOwner: String): Option[ObjectData] = {
    objects.get(id).map { obj =>
      val newMetadata = obj.metadata.copy(owners = Set(newOwner))
      val newData = obj.data // veri değişmiyor, sadece meta veri
      val newObj = ObjectData(
        ObjectRef(obj.ref.id, obj.ref.version + 1),
        newMetadata,
        newData,
        obj.previousVersions :+ obj.ref
      )
      
      // Yeni versiyonu nesne deposuna ekle
      objects(id) = newObj
      
      // Geçmiş versiyonu da kaydet
      val history = objectHistory.getOrElseUpdate(id, mutable.Map.empty)
      history(obj.ref.version) = obj
      history(newObj.ref.version) = newObj
      
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
    
    val obj = ObjectData(ref, metadata, data)
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