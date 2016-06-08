package controllers

import com.google.inject.Inject
import model._
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

class LanguageController @Inject()(val messagesApi: MessagesApi) extends Controller with I18nSupport with Secured {

  val languageForm = Form(
    mapping(
      "code" -> nonEmptyText,
      "name" -> nonEmptyText
    ) (Language.apply) (Language.unapply)
  )

  def index = withAuth { user => implicit request =>
    Ok(views.html.languages(LanguageRepository.findAll))
  }

  def create = withAuth { user => implicit request =>
    Ok(views.html.createLanguage(languageForm))
  }

  def save = withAuth { user => implicit request =>
    languageForm.bindFromRequest.fold(
      errors => BadRequest(views.html.createLanguage(errors)),
      language => {
        LanguageRepository.add(language)
        Redirect(routes.LanguageController.index())
      }
    )
  }

  def edit(id: Long) = withAuth { user => implicit request =>
    val language = LanguageRepository.find(id)
    language.fold(NotFound("Language not found.")) { l => Ok(views.html.editLanguage(languageForm.fill(l), l.id)) }
  }

  def update(id: Long) = withAuth { user => implicit request =>
    languageForm.bindFromRequest.fold(
      errors => BadRequest(views.html.editLanguage(errors, id)),
      language => {
        val existingLanguage = LanguageRepository.find(id)
        existingLanguage.fold(BadRequest("Cannot find language to update.")) { l =>
          LanguageRepository.update(l, language)
          Redirect(routes.LanguageController.index())
        }
      }
    )
  }

  def delete(id: Long) = withAuth { user => implicit request =>
    val existingLanguage = LanguageRepository.find(id)
    existingLanguage.fold(BadRequest("Cannot find language to delete.")) { l =>
      LanguageRepository.delete(l)
      Redirect(routes.LanguageController.index())
    }
  }
}
