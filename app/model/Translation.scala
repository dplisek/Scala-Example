package model

case class TranslationWithoutLanguage(code: String, text: String)

case class Translation(code: String, text: String, language: String)

class TranslationId(val id: Long, val constantId: Long, val resourceVersion: Long, code: String, text: String, language: String) extends Translation(code, text, language) with Cloneable {

  def this(id: Long, constantId: Long, resourceVersion: Long, translation: Translation) {
    this(id, constantId, resourceVersion, translation.code, translation.text, translation.language)
  }

  override def clone(): TranslationId = super.clone().asInstanceOf[TranslationId]
}

object TranslationRepository {

  private var translations = Map[Long, TranslationId] ()

  def findAll = translations.values

  def find(id: Long) = translations.get(id)

  def findLatestResourceVersion(lessThanOrEqual: Option[Long]) = {
    val t = lessThanOrEqual.fold(translations.values) {limit => translations.values.filter(_.resourceVersion <= limit)}
    if (t.isEmpty) 0L else t.maxBy(_.resourceVersion).resourceVersion
  }

  def findByResourceVersion(version: Long) = translations.values.filter(_.resourceVersion == version)

  def findByConstantIdAndResourceVersion(constantId: Long, resourceVersion: Long) = translations.values.filter(_.constantId == constantId).find(_.resourceVersion == resourceVersion)

  def add(translation: Translation, resourceVersion: Long) = {
    var newTranslation: TranslationId = null
    translation match {
      case id: TranslationId => newTranslation = new TranslationId(if (translations.keys.isEmpty) 0 else translations.keys.max + 1, id.constantId, resourceVersion, id)
      case _ => newTranslation = new TranslationId(if (translations.keys.isEmpty) 0 else translations.keys.max + 1, if (translations.values.isEmpty) 0 else translations.values.maxBy(_.constantId).constantId + 1, resourceVersion, translation)
    }
    translations += (newTranslation.id -> newTranslation)
  }

  def update(old: TranslationId, updated: Translation) {
    val newTranslation = new TranslationId(old.id, old.constantId, old.resourceVersion, updated)
    translations += (newTranslation.id -> newTranslation)
  }

  def delete(t: TranslationId) {
    translations -= t.id
  }
}