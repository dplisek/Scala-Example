package model

case class User(username: String, password: String)

class UserId(val id: Long, username: String, password: String) extends User(username, password) {

  def this(id: Long, user: User) {
    this(id, user.username, user.password)
  }
}

object UserRepository {

  private val users = Map[Long, UserId](0L -> new UserId(0, "dplisek", "dplisekpass"))

  def findAll = users.values

  def findByUsername(username: String): Option[User] = users.values.find(_.username == username)
}