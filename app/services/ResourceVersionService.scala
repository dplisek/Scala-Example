package services

import com.google.inject.Singleton
import model.{Translation, TranslationId, TranslationRepository}
import play.api.mvc.Session

@Singleton
class ResourceVersionService {

  var currentVersion = 0L

  def selectedResourceVersion(session: Session) = session.get("selectedResourceVersion").fold(Option.empty[Long]) {version => Option(version.toLong)}

  def sessionWithResourceVersion(session: Session, version: Long) = session + ("selectedResourceVersion" -> version.toString)

  def copyCurrentResourcesAsNewVersion(session: Session, providedVersion: Option[Long] = Option.empty[Long]) = {
    this.synchronized {
      val version = providedVersion.fold(selectedResourceVersion(session)) { _ => providedVersion }
      val translations: Iterable[TranslationId] = TranslationRepository.findByResourceVersion(TranslationRepository.findLatestResourceVersion(version))
      currentVersion += 1
      for (translation <- translations) {
        TranslationRepository.add(translation.clone(), currentVersion)
      }
      currentVersion
    }
  }

  def createNewVersionWithResourcesFromOtherLanguages(language: String, session: Session) = {
    this.synchronized {
      val translations: Iterable[TranslationId] = TranslationRepository.findByResourceVersion(TranslationRepository.findLatestResourceVersion(selectedResourceVersion(session)))
      val translationsFromOtherLanguages = translations.filter(_.language != language)
      currentVersion += 1
      for (translation <- translationsFromOtherLanguages) {
        TranslationRepository.add(translation.clone(), currentVersion)
      }
      currentVersion
    }
  }
}
