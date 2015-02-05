package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import play.api.libs.json._
import play.api.libs.json.util._
import play.api.libs.json.Writes._
import play.api.libs.functional.syntax._
import play.api.cache.Cache

/**
 * Helper for pagination.
 */
case class Page[A](items: List[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

/**
 *  table having unique combination of city and region
 */
case class Geo(id: Pk[Long], country: Option[Long], city: Option[Long], region: Option[Long])

/**
 *
 */
case class Region(id: Pk[Long], name: String)

/**
 *
 */
case class City(id: Pk[Long], name: String)

/**
 *
 */
case class Users(id: Pk[Long], fullname: String, password: String, geoId: Option[Long])

/**
 *
 */
object Geo {

  /**
   * Parse a Geo table from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("geo.id") ~
      get[Option[Long]]("geo.country") ~
      get[Option[Long]]("geo.city") ~
      get[Option[Long]]("geo.region") map {
        case id ~ country ~ city ~ region => Geo(id, country, city, region)
      }
  }

  val cityreg = {
    get[Long]("city") ~
      get[Long]("region") map {
        case city ~ region => (city, region)
      }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Seq[(String, String)] = DB.withConnection { implicit connection =>
    SQL("select * from geo").as(Geo.simple *).map(c => (c.id.toString, c.region.toString()))
  }

  /**
   *  return the json having regions for specific city
   */
  def getRegionJson(cityId: Long) = {
    Cache.getOrElse[JsValue]("regionJson" + cityId.toString) {
      implicit var anormLongPkFormat = new Format[Pk[Long]] {
        def writes(key: Pk[Long]): JsValue = Json.toJson(key.toString)
        def reads(jv: JsValue): JsResult[Pk[Long]] = JsError()
      }
      implicit val regReads = Json.writes[Region]
      Json.toJson(getRegions(cityId))
    }

  }
  
  /**
   *  return regions for city
   */
  def getRegions(cityId: Long): List[Region] = {
    val q = """
    SELECT r.name,r.id FROM region r join geo g on g.region = r.id where g.city={id} group by r.id
      """

    DB.withConnection { implicit connection =>
      var regions = SQL(q).on('id -> cityId).as(str("name") ~ get[Pk[Long]]("id") map (flatten) *)
      regions.map((x => Region(x._2, x._1)))

    }
  }

  /**
   * get region Ids for specific city
   */
  def getRegionsIds(cityId: Long): List[Long] = {
    val q = """
    SELECT r.id FROM region r join geo g on g.region = r.id where g.city={id} group by r.id
      """

    DB.withConnection { implicit connection =>
      SQL(q).on('id -> cityId).as(get[Long]("id") *)
    }
  }

  /**
   *  get the city name for the given id
   */
  def getCityName(cityId: Long) = {
    Cache.getOrElse[Option[String]]("cityName" + cityId.toString) {
      val query = """
      select name from city where id = {id}
      """

      DB.withConnection { implicit connection =>
        println(SQL(query))
        SQL(query).on('id -> cityId).as(scalar[String].singleOpt)

      }
    }

  }

  /**
   *  get the region name for the given id
   */
  def getRegionName(regId: Long) = {
    Cache.getOrElse[Option[String]]("regionName" + regId.toString) {
      val query = """
      select name from region where id = {id}
      """

      DB.withConnection { implicit connection =>
        println(SQL(query))
        SQL(query).on('id -> regId).as(scalar[String].singleOpt)

      }
    }

  }

  /**
   *  get the corresponding geo id for the given city and region 
   */
  def getGeoId(city: Long, region: Long): Long = {

    val query = """
      select id from geo where city = {city} and region = {region}
      """

    DB.withConnection { implicit connection =>
      println(SQL(query))
      SQL(query).on('city -> city,
        'region -> region).as(scalar[Long].single)

    }
  }

  
  /**
   *  get the applicable geo ids for the given city and region based on the values passed
   */
  def getGeoIds(city: Long, region: Long): List[Long] = {
    val geoId = if (city == 0 && region != 0) {
      """
         select id from geo where region = {region}
       """
    } else if (city != 0 && region == 0) {
      """
         select id from geo where city = {city}
       """
    } else if (city == 0 && region == 0) {
      """
         select id from geo 
       """
    } else {
      """
          select id from geo where city = {city} and region = {region}
       """
    }
    DB.withConnection { implicit connection =>
      println(SQL(geoId))
      SQL(geoId).on('city -> city,
        'region -> region).as(get[Long]("id") *)

    }

  }

  /**
   * get the region and city for given geo id
   */
  def getCityRegionId(geoId: Long): Option[(Long, Long)] = {

    DB.withConnection { implicit connection =>
      SQL("select city,region from geo where id = {id}").on('id -> geoId).as(Geo.cityreg.singleOpt)

    }

  }
}

object Region {

  /**
   * Parse a region from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("region.id") ~
      get[String]("region.name") map {
        case id ~ name => Region(id, name)
      }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Seq[(String, String)] = DB.withConnection { implicit connection =>
    SQL("select * from region").as(Region.simple *).map(c => (c.id.toString, c.name))
  }

  def options(cityId: Long): Seq[(String, String)] = {
    val rIds = Geo.getRegionsIds(cityId);
    val q = "select * from region where id in (%s)"
    DB.withConnection { implicit connection =>
      SQL(q.format(rIds.map("'%s'".format(_)).mkString(","))).as(Region.simple *).map(c => (c.id.toString, c.name))
    }
  }

}

object City {

  /**
   * Parse a city from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("city.id") ~
      get[String]("city.name") map {
        case id ~ name => Region(id, name)
      }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Seq[(String, String)] = DB.withConnection { implicit connection =>
    SQL("select * from city").as(City.simple *).map(c => (c.id.toString, c.name))
  }

}

object Users {

  /**
   * Parse a users from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("users.id") ~
      get[String]("users.fullname") ~
      get[String]("users.password") ~
      get[Option[Long]]("users.geoId") map {
        case id ~ name ~ password ~ geoId => Users(id, name, password, geoId)
      }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Seq[(String, String)] = DB.withConnection { implicit connection =>
    SQL("select * from users").as(Users.simple *).map(c => (c.id.toString(), c.fullname))
  }

}
