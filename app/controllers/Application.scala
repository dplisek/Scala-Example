package controllers

import java.io.{File, FileInputStream}

import com.fasterxml.jackson.databind.JsonMappingException
import com.google.inject.Inject
import model._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._
import services.ResourceVersionService
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.format.Formats._

class Application @Inject()(val messagesApi: MessagesApi, val resourceVersionService: ResourceVersionService) extends Controller with I18nSupport with Secured {

  val translationForm = Form(
    mapping(
      "code" -> nonEmptyText,
      "text" -> nonEmptyText,
      "language" -> of[Long]
    ) (Translation.apply) (Translation.unapply)
  )

  val languageForm = Form(
    "language" -> of[Long]
  )

  implicit val translationWrites: Writes[TranslationWithoutLanguage] = (
    (JsPath \ "code").write[String] and
    (JsPath \ "text").write[String]
  ) (unlift(TranslationWithoutLanguage.unapply))

  implicit val translationReads: Reads[TranslationWithoutLanguage] = (
    (JsPath \ "code").read[String] and
    (JsPath \ "text").read[String]
  ) (TranslationWithoutLanguage.apply _)

  def index = withAuth { user => implicit request =>
    val translations = TranslationRepository.findByResourceVersion(TranslationRepository.findLatestResourceVersion(resourceVersionService.selectedResourceVersion(request.session)))
    Ok(views.html.index(translations.map {t => TranslationWithLanguageName(t.id, t.constantId, t.code, t.text, LanguageRepository.find(t.languageId).get.name)}))
  }

  def create = withAuth { user => implicit request =>
    Ok(views.html.create(translationForm, LanguageRepository.findAll))
  }

  def save = withAuth { user => implicit request =>
    translationForm.bindFromRequest.fold(
      errors => BadRequest(views.html.create(errors, LanguageRepository.findAll)),
      translation => {
        val newVersion = resourceVersionService.copyCurrentResourcesAsNewVersion(request.session)
        TranslationRepository.add(translation, newVersion)
        Redirect(routes.Application.index()).withSession(resourceVersionService.sessionWithResourceVersion(request.session, newVersion))
      }
    )
  }

  def edit(id: Long) = withAuth { user => implicit request =>
    val translation = TranslationRepository.find(id)
    translation.fold(NotFound("Translation not found.")) { t => Ok(views.html.edit(translationForm.fill(t), t.constantId, LanguageRepository.findAll)) }
  }

  def update(constantId: Long) = withAuth { user => implicit request =>
    translationForm.bindFromRequest.fold(
      errors => BadRequest(views.html.edit(errors, constantId, LanguageRepository.findAll)),
      translation => {
        val newVersion = resourceVersionService.copyCurrentResourcesAsNewVersion(request.session)
        val existingTranslation = TranslationRepository.findByConstantIdAndResourceVersion(constantId, newVersion)
        existingTranslation.fold(BadRequest("Cannot find translation to update.")) { t =>
          TranslationRepository.update(t, translation)
          Redirect(routes.Application.index()).withSession(resourceVersionService.sessionWithResourceVersion(request.session, newVersion))
        }
      }
    )
  }

  def delete(constantId: Long) = withAuth { user => implicit request =>
    if (TranslationRepository.findByResourceVersion(TranslationRepository.findLatestResourceVersion(resourceVersionService.selectedResourceVersion(request.session))).size == 1) {
      BadRequest("Cannot delete last one.")
    } else {
      val newVersion = resourceVersionService.copyCurrentResourcesAsNewVersion(request.session)
      val existingTranslation = TranslationRepository.findByConstantIdAndResourceVersion(constantId, newVersion)
      existingTranslation.fold(BadRequest("Cannot find translation to delete.")) { t =>
        TranslationRepository.delete(t)
        Redirect(routes.Application.index()).withSession(resourceVersionService.sessionWithResourceVersion(request.session, newVersion))
      }
    }
  }

  def history = withAuth { user => implicit request =>
    Ok(views.html.history(resourceVersionService.selectedResourceVersion(request.session).getOrElse(resourceVersionService.currentVersion), resourceVersionService.currentVersion))
  }

  def selectVersion(version: Long) = withAuth { user => implicit request =>
    Redirect(routes.Application.index()).withSession(resourceVersionService.sessionWithResourceVersion(request.session, version))
  }

  def revertTo(version: Long) = withAuth { user => implicit request =>
    val newVersion = resourceVersionService.copyCurrentResourcesAsNewVersion(request.session, Option(version))
    Redirect(routes.Application.index()).withSession(resourceVersionService.sessionWithResourceVersion(request.session, newVersion))
  }

  def export = withAuth { user => implicit request =>
    Ok(views.html.export(languageForm, LanguageRepository.findAll))
  }

  def exportJson = withAuth { user => implicit request =>
    languageForm.bindFromRequest().fold(
      errors => BadRequest(views.html.export(errors, LanguageRepository.findAll)),
      language => {
        val translations = TranslationRepository.findByResourceVersion(TranslationRepository.findLatestResourceVersion(resourceVersionService.selectedResourceVersion(request.session)))
        val json = Json.toJson(translations.filter(_.languageId == language).map {translation => TranslationWithoutLanguage(translation.code, translation.text)})
        Ok(json).withHeaders("Content-disposition" -> "attachment;filename=translations.json")
      }
    )
  }

  def importFile = withAuth( user => implicit request =>
    Ok(views.html.import_(languageForm, LanguageRepository.findAll))
  )

  def importJson = Action(parse.multipartFormData) { implicit request =>
    languageForm.bindFromRequest().fold(
      errors => BadRequest(views.html.import_(errors, LanguageRepository.findAll)),
      language => {
        request.body.file("file").map { file =>
          val tempFile = file.ref.moveTo(new File("/tmp/file"), replace = true)
          val stream = new FileInputStream(tempFile)
          (try {
            Json.parse(stream)
          } catch {
            case e: JsonMappingException => BadRequest(views.html.import_(languageForm.fill(language).withError("language", "Invalid file"), LanguageRepository.findAll))
            case _ => BadRequest(views.html.import_(languageForm.fill(language).withError("language", "Something went wrong. Please try again."), LanguageRepository.findAll))
          } finally {
            stream.close()
          }) match {
            case json: JsValue => {
              val validationResult = json.validate[Seq[TranslationWithoutLanguage]]
              validationResult match {
                case s: JsSuccess[Seq[TranslationWithoutLanguage]] => {
                  val newVersion = resourceVersionService.createNewVersionWithResourcesFromOtherLanguages(language, request.session)
                  for (t <- s.get) {
                    TranslationRepository.add(Translation(t.code, t.text, language), newVersion)
                  }
                  Redirect(routes.Application.index()).withSession(resourceVersionService.sessionWithResourceVersion(request.session, newVersion))
                }
                case e: JsError => BadRequest(views.html.import_(languageForm.fill(language).withError("language", e.errors.toString), LanguageRepository.findAll))
              }
            }
            case r: Result => r
          }
        }.getOrElse(BadRequest(views.html.import_(languageForm.fill(language).withError("language", "No file provided."), LanguageRepository.findAll)))
      }
    )
  }
}
