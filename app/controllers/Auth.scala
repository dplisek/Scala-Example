package controllers

import com.google.inject.Inject
import model.{User, UserRepository}
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

class Auth @Inject()(val messagesApi: MessagesApi) extends Controller with I18nSupport {

  val loginForm = Form(
    mapping(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText
    ) (User.apply) (User.unapply) verifying("Invalid user name or password.", _ match {
      case user => check(user)
    })
  )

  def check(user: User) = UserRepository.findByUsername(user.username).fold(false) {_.password == user.password}

  def index = Action {
    Ok(views.html.login(loginForm))
  }

  def login = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      errors => BadRequest(views.html.login(errors)),
      user => Redirect(routes.Application.index()).withSession(Security.username -> user.username)
    )
  }

  def logout = Action {
    Redirect(routes.Auth.index()).withNewSession
  }
}
