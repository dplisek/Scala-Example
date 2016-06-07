package controllers

import com.google.inject.Inject
import model.{Translation, TranslationRepository, User, UserRepository}
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{AnyContent, Request, Result, _}
import services.ResourceVersionService


class Application @Inject()(val messagesApi: MessagesApi, val resourceVersionService: ResourceVersionService) extends Controller with I18nSupport with Secured {

  val translationForm = Form(
    mapping(
      "code" -> nonEmptyText,
      "text" -> nonEmptyText,
      "language" -> nonEmptyText
    ) (Translation.apply) (Translation.unapply)
  )

  def index = withAuth { user => implicit request =>
    val translations = TranslationRepository.findByResourceVersion(TranslationRepository.findLatestResourceVersion(resourceVersionService.selectedResourceVersion(request.session)))
    Ok(views.html.index(translations))
  }

  def create = withAuth { user => implicit request =>
    Ok(views.html.create(translationForm))
  }

  def save = withAuth { user => implicit request =>
    translationForm.bindFromRequest.fold(
      errors => BadRequest(views.html.create(errors)),
      translation => {
        val newVersion = resourceVersionService.copyCurrentResourcesAsNewVersion(request.session)
        TranslationRepository.add(translation, newVersion)
        Redirect(routes.Application.index())
      }
    )
  }

  def edit(id: Long) = withAuth { user => implicit request =>
    val translation = TranslationRepository.find(id)
    translation.fold(NotFound("Translation not found.")) { t => Ok(views.html.edit(translationForm.fill(t), t.constantId)) }
  }

  def update(constantId: Long) = withAuth { user => implicit request =>
    translationForm.bindFromRequest.fold(
      errors => BadRequest(views.html.edit(errors, constantId)),
      translation => {
        val newVersion = resourceVersionService.copyCurrentResourcesAsNewVersion(request.session)
        val existingTranslation = TranslationRepository.findByConstantIdAndResourceVersion(constantId, newVersion)
        existingTranslation.fold(BadRequest("Cannot find translation to update.")) { t =>
          TranslationRepository.update(t, translation)
          Redirect(routes.Application.index())
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
        Redirect(routes.Application.index())
      }
    }
  }
}

trait Secured {

  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Auth.index())

  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }

  def withUser(f: User => Request[AnyContent] => Result) = withAuth { username => implicit request =>
    UserRepository.findByUsername(username).map { user =>
      f(user)(request)
    }.getOrElse(onUnauthorized(request))
  }
}