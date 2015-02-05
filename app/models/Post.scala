package models

import java.util.Date
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import collection.immutable.{ Nil, :: }


case class Post_Categories(id: Pk[Long] = NotAssigned, name: String)
case class Post(
  id: Pk[Long] = NotAssigned,
  content: String, postedAt: Date, authorId: Long, categoryId: Long) {

  def prevNext: (Option[Post], Option[Post]) = {
    DB.withConnection {
      implicit connection =>
        val result = SQL(
          """
              (
                  select p.*, 'next' as pos from Post p
                  where postedAt < {date} order by postedAt desc limit 1
              )
                  union
              (
                  select p.*, 'prev' as pos from Post p
                  where postedAt > {date} order by postedAt asc limit 1
              )

              order by postedAt desc

          """).on("date" -> postedAt).as(
            Post.withPrevNext *).partition(_._2 == "prev")

        (result._1 match {
          case List((post, "prev")) => Some(post)
          case _ => None
        },
          result._2 match {
            case List((post, "next")) => Some(post)
            case _ => None
          })
    }
  }

  def tagItWith(name: String) = {
    val tag = Tag.findOrCreateByName(name)
    TagsForPosts.link(tag.id.get, id.get)
  }

  // Returns the list of Tag for this Post 
  def getTags: List[String] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          SELECT 
    		  t.NAME 
          FROM Tag t 
    		  JOIN TagsForPosts tfp ON tfp.tag_id=t.id 
    		  JOIN Post p on p.id=tfp.post_id 
          WHERE 
    		  p.id={id}
        """).on('id -> id.get)
        .as(get[String]("Tag.name") *)
    }
  }
}

object Post {

  def apply(content: String, postedAt: Date, authorId: Long, categoryId: Long) = new Post(NotAssigned, content, postedAt, authorId, categoryId)

  // -- Parsers

  /**
   * Parse a Post from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("Post.id") ~
      get[String]("Post.content") ~
      get[Date]("Post.postedAt") ~
      get[Long]("Post.author_id") ~
      get[Long]("Post.category_id") map {
        case id ~ content ~ postedAt ~ authorId ~ categoryId => Post(id, content, postedAt, authorId, categoryId)
      }
  }

  /**
   * Parse a (Post,User) from a ResultSet
   */
  val withUser = Post.simple ~ PlatformUser.simple map {
    case post ~ user => (post, user)
  }

  /**
   * Parse a (Post,User) from a ResultSet
   */
  val withPrevNext = {
    get[Pk[Long]]("id") ~ get[String]("content") ~ get[Date]("postedAt") ~ get[Long]("author_id") ~ get[Long]("category_id") ~ get[String]("pos") map {
      case id ~ content ~ postedAt ~ authorId ~ categoryId ~ pos => (Post(id, content, postedAt, authorId, categoryId), pos)
    }
  }

  /**
   * Parse a (Post, User, List[Comment]) from a ResultSet
   */
  val withAuthorAndComments = Post.simple ~ PlatformUser.simple ~ (FullComment.simple ?) map {
    case post ~ user ~ comments => (post, user, comments)
  }

  /**
   * Create a Post.
   */
  def create(post: Post): Long = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            insert into Post(content, postedAt, author_id,category_id) values (
        		{content}, {postedAt}, {author_id},{category_id}
            )
          """).on(
            'content -> post.content,
            'postedAt -> post.postedAt,
            'author_id -> post.authorId,
            'category_id -> post.categoryId).executeInsert()
    }.get
  }

  def setGeoId(postId: Long, cId: Long, rId: Long) {
    val geo = Geo.getGeoId(cId, rId)
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            insert into GeosForPosts(geo_id, post_id) values (
        		{geo}, {post}
            )
          """).on(
            'geo -> geo,
            'post -> postId).executeInsert()
    }.get

  }
  def allWithAuthor: List[(Post, PlatformUser)] = {
    DB.withConnection {
      implicit connection =>
        SQL(
          """
              select p.*, u.* from Post p
              join User u on p.author_id = u.id
              order by p.postedAt desc
          """).as(withUser *)
    }
  }

  def allWithAuthorAndComments: List[(Post, PlatformUser, List[FullComment])] =
    DB.withConnection {
      implicit connection =>
        SQL(
          """
              select p.*, u.*, c.* from Post p
              join users u on p.author_id = u.id
              left join Comment c on c.post_id = p.id
              order by p.postedAt desc
          """).as(withAuthorAndComments *)
          .groupBy(row => (row._1, row._2))
          .mapValues(_.unzip3._3.map(_.orNull))
          .map(row => row._2 match {
            case List(null) => (row._1._1, row._1._2, List())
            case comments => (row._1._1, row._1._2, comments)
          }).toList
    }

  def byIdWithAuthorAndComments(id: Long): Option[(Post, PlatformUser, List[FullComment])] =
    DB.withConnection {
      implicit connection =>
        Some(
          SQL(
            """
                select * from Post p
                join users u on p.author_id = u.id
                left join Comment c on c.post_id = p.id
                where p.id = {id}
            """)
            .on("id" -> id)
            .as(withAuthorAndComments *)
            .groupBy(row => (row._1, row._2))
            .mapValues(_.unzip3._3.map(_.orNull))
            .map(row => row._2 match {
              case List(null) => (row._1._1, row._1._2, List())
              case comments => (row._1._1, row._1._2, comments)
            }).head)
    }

  def count() = {
    DB.withConnection {
      implicit connection =>
        SQL("select count(*) from Post").as(scalar[Long].single)
    }
  }

  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%", city: Int, region: Int): List[(Post, PlatformUser, List[FullComment])] = {

    val offest = pageSize * page
    val geoIdsList = Geo.getGeoIds(city, region);

    //    var businessQuery = ""
    //    var businessCountQuery = ""

    var (postsQuery, postsCountQuery) = ("""
              select p.*, u.*, c.* from Post p
              join users u on p.author_id = u.id
              left join Comment c on c.post_id = p.id
		 	  left join GeosForPosts g on p.id = g.post_id
		 	  where p.content like {filter} and g.geo_id in (%s)
              order by p.postedAt
          """, """
              select count(*) from Post p
              join users u on p.author_id = u.id
              left join Comment c on c.post_id = p.id
		 	  left join GeosForPosts g on p.id = g.post_id
		 	  where p.content like {filter} and g.geo_id in (%s)
        """)

    DB.withConnection { implicit connection =>

      var posts = SQL(postsQuery.format(geoIdsList.map("'%s'".format(_)).mkString(","))).on('pageSize -> pageSize,
        'offset -> offest,
        'filter -> filter,
        'orderBy -> orderBy).as(withAuthorAndComments *)
        .groupBy(row => (row._1, row._2))
        .mapValues(_.unzip3._3.map(_.orNull))
        .map(row => row._2 match {
          case List(null) => (row._1._1, row._1._2, List())
          case comments => (row._1._1, row._1._2, comments)
        }).toList

      //*getting geoId

      var totalRows = SQL(
        postsCountQuery.format(geoIdsList.map("'%s'".format(_)).mkString(","))).on(
          'filter -> filter).as(scalar[Long].single)

      posts
    }

  }
  def findTaggedWith(name: String): List[Post] =
    DB.withConnection { implicit connection =>
      SQL("""
            select p.* from Post p
            join TagsForPosts tfp on p.id=tfp.post_id
            join Tag t on tfp.tag_id=t.id
            where t.name={name}
        """).on('name -> name).as(Post.simple *)
    }

  /**
   *  report abuse the Post
   */

  def reportAbuse(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update Post set reportAbuse={report}
          )
        """).on(
          'report -> 1).executeUpdate()
    }
  }
}

object Post_Categories {

  /**
   * Parse a Company from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("post_categories.id") ~
      get[String]("post_categories.name") map {
        case id ~ name => Post_Categories(id, name)
      }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Map[String, String] = DB.withConnection { implicit connection =>
    SQL("select * from post_categories order by name").as(Post_Categories.simple *).map(c => c.id.toString -> c.name).toMap
  }

}
