package model

case class Language(code: String, name: String)

class LanguageId(val id: Long, code: String, name: String) extends Language(code, name) {

  def this(id: Long, language: Language) {
    this(id, language.code, language.name)
  }
}

object LanguageRepository {

  private var languages = Map[Long, LanguageId](
    0L -> new LanguageId(0, "en", "English"),
    1L -> new LanguageId(1, "cs", "Čeština")
  )

  def findAll = languages.values

  def find(id: Long) = languages.get(id)

  def add(language: Language) = {
    val newLang = new LanguageId(if (languages.keys.isEmpty) 0 else languages.keys.max + 1, language)
    languages += (newLang.id -> newLang)
  }

  def update(old: LanguageId, updated: Language) = {
    val newLang = new LanguageId(old.id, updated)
    languages += (newLang.id -> newLang)
  }

  def delete(l: LanguageId) {
    languages -= l.id
  }
}
