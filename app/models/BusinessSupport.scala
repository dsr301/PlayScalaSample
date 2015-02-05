package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import scala.util.Try

//class to have all the support content for business classes

/**
 * representing the reviews and rating for business
 */
case class Reviews_ratings(id: Pk[Long], businessId: Option[Long], userId: Option[Long], text: Option[String], rating: Option[Float], abusive: Option[Boolean])
case class Reviews_ratingsForm(id: Pk[Long], businessId: Option[Long], userId: Option[Long], text: Option[String], rating: Option[Long], abusive: Option[Boolean])


object Reviews_ratings {
  /**
   * Parse a review from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("reviews_ratings.id") ~
      get[Option[Long]]("reviews_ratings.businessId") ~
      get[Option[Long]]("reviews_ratings.userId") ~
      get[Option[String]]("reviews_ratings.text") ~
      get[Option[Float]]("reviews_ratings.rating") ~
      get[Option[Boolean]]("reviews_ratings.reportAbuse") map {
        case id ~ businessId ~ userId ~ text ~ rating ~ abusive => Reviews_ratings(id, businessId, userId, text, rating, abusive)
      }
  }
  
 implicit def rowToFloat: Column[Float] = Column.nonNull { (value, meta) =>
  val MetaDataItem(qualified, nullable, clazz) = meta
  value match {
    case d: Float => Right(d)
    case _ => Left(TypeDoesNotMatch("Cannot convert " + value + ":" + value.asInstanceOf[AnyRef].getClass + " to Float for column " + qualified))
  }
}

  /** 
   * Retrieve a reviews from the business id.
   */
  def findById(id: Long): Seq[Reviews_ratings] = {
    DB.withConnection { implicit connection =>
      SQL("select * from reviews_ratings where businessId = {id} ").on('id -> id).as(Reviews_ratings.simple *)
    }
  }

  /**
   *  Add review to a business
   */
  def insert(ratings: Reviews_ratingsForm) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into reviews_ratings (businessId,userId,text,rating,reportAbuse) values ( 
            {businessId}, {userId}, {text}, {rating},{abusive}
          )
        """).on(
          'businessId -> ratings.businessId,
          'userId -> ratings.userId,
          'text -> ratings.text,
          'rating -> ratings.rating,
          'abusive -> ratings.abusive).executeUpdate()
    }
  }

  /**
   *  report abuse the comment/review
   */

  def reportAbuse(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update reviews_ratings set reportAbuse={report}
          )
        """).on(
          'report -> 1).executeUpdate()
    }
  }
}