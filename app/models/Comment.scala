package models
import anorm._
import anorm.SqlParser._
import java.util.Date
import play.api.db._
import play.api.Play.current


case class Comment(
  id: Pk[Long],
  user: Long, content: String, postedAt: Date, postId: Long)

case class FullComment(
  id: Pk[Long],
  user: PlatformUser, content: String, postedAt: Date, postId: Long)

object FullComment {
  /* Parsers */

  /**
   * Parse a Post from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("Comment.id") ~
      get[Long]("Comment.userId") ~
      get[String]("Comment.content") ~
      get[Date]("Comment.postedAt") ~
      get[Long]("Comment.post_id") map {
        case id ~ author ~ content ~ postedAt ~ postId => FullComment(id, (PlatformUser.findById(author)).map(u => u).getOrElse(null), content, postedAt, postId)
      }
  }

}
object Comment {

  def create(comment: Comment): Comment = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into Comment(userId, content, postedAt, post_id) values (
            {author}, {content}, {postedAt}, {post_id}
          )
        """).on(
          'author -> comment.user,
          'content -> comment.content,
          'postedAt -> comment.postedAt,
          'post_id -> comment.postId).executeUpdate()
      comment
    }
  }

  def count() = {
    DB.withConnection { implicit connection =>
      SQL("select count(*) from Comment").as(scalar[Long].single)
    }
  }

  /**
   *  report abuse the Comment
   */

  def reportAbuse(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update Comment set reportAbuse={report}
          )
        """).on(
          'report -> 1).executeUpdate()
    }
  }
}