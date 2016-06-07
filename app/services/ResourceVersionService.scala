package services

import com.google.inject.Singleton
import model.{Translation, TranslationId, TranslationRepository}
import play.api.mvc.Session

@Singleton
class ResourceVersionService {

  var currentVersion = 0L

  def selectedResourceVersion(session: Session) = session.get("selectedResourceVersion").fold(Option.empty[Long]) {version => Option(version.toLong)}

  def copyCurrentResourcesAsNewVersion(session: Session) = {
    this.synchronized {
      val translations: Iterable[TranslationId] = TranslationRepository.findByResourceVersion(TranslationRepository.findLatestResourceVersion(selectedResourceVersion(session)))
      currentVersion += 1
      for (translation <- translations) {
        TranslationRepository.add(translation.clone(), currentVersion)
      }
      currentVersion
    }
  }
}
