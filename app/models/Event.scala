package models

import java.util.Date
import java.sql.Time
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import java.text.DateFormat
import java.text.SimpleDateFormat


case class Event_Categories(id: Pk[Long] = NotAssigned, name: String)
case class Event(
  id: Pk[Long] = NotAssigned,
  name: String, description: String, venue: String, from: Date, to: Date, startTime: String, endTime: String, postedAt: Date, authorId: Long, categoryId: Long)

case class EventForm(
  id: Pk[Long] = NotAssigned,
  name: String, description: String, venue: String, from: Date, to: Date, startTime: String, endTime: String, postedAt: Date, authorId: Long, categoryId: Long,cityId:Long,regId:Long)  
object Event {

  def apply(name: String, description: String, venue: String, from: Date, to: Date, startTime: String, endTime: String, postedAt: Date, authorId: Long, categoryId: Long) = new Event(NotAssigned, name, description, venue, from, to, startTime, endTime, postedAt, authorId, categoryId)

  // -- Parsers

  /**
   * Parse a Post from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("event.id") ~
      get[String]("event.name") ~
      get[String]("event.description") ~
      get[String]("event.venue") ~
      get[Date]("event.fromDate") ~
      get[Date]("event.toDate") ~
      get[Date]("event.startTime") ~
      get[Date]("event.endTime") ~
      get[Date]("event.postedAt") ~
      get[Long]("event.author_id") ~
      get[Long]("event.category_id") map {
        case id ~ name ~ description ~ venue ~ from ~ to ~ start ~ end ~ postedAt ~ authorId ~ categoryId => Event(id, name, description, venue, from, to, start.toString, end.toString, postedAt, authorId, categoryId)
      }
  }

  /**
   * Create a Post.
   */
  def create(event: EventForm): Long = {
     val outputFormatter:DateFormat = new SimpleDateFormat("yyyy-MM-dd")
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            insert into event(name,description,venue,fromDate,toDate,startTime,endTime,postedAt,author_id,category_id) values (
            {name},{description},{venue},{from},{to},{startTime},{endTime},{postedAt},{author_id},{category_id}
            )
          """).on(
            'name -> event.name,
            'description -> event.description,
            'venue -> event.venue,
            'from -> outputFormatter.format(event.from),
            'to -> outputFormatter.format(event.to),
            'startTime -> event.startTime,
            'endTime -> event.endTime,
            'postedAt -> outputFormatter.format(event.postedAt),
            'author_id -> event.authorId,
            'category_id -> event.categoryId).executeInsert()
    }.get
  }

  def setGeoId(eventId: Long, cId: Long, rId: Long) {
    val geo = Geo.getGeoId(cId, rId)
    DB.withConnection {
      implicit connection =>
        SQL(
          """
            insert into GeosForEvents(geo_id, event_id) values (
        		{geo}, {event}
            )
          """).on(
            'geo -> geo,
            'event -> eventId).executeInsert()
    }.get

  }

  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%", city: Int, region: Int,category:Long): Page[Event] = {

    val offest = pageSize * page
    val geoIdsList = Geo.getGeoIds(city, region);

    //    var businessQuery = ""
    //    var businessCountQuery = ""

    var (eventsQuery, eventsCountQuery) = if(category==0) {
      ("""
                        select * from event e
		 	  left join GeosForEvents g on e.id = g.event_id
		 	  where e.name like {filter} and g.geo_id in (%s)
              order by e.toDate desc
          """, """
              select count(*) from event e
		 	  left join GeosForEvents g on e.id = g.event_id
		 	  where e.name like {filter} and g.geo_id in (%s)
        """)
    }else{
           ("""
                        select * from event e
		 	  left join GeosForEvents g on e.id = g.event_id
		 	  where e.name like {filter} and g.geo_id in (%s) and category_id = {category}
              order by e.toDate desc
          """, """
              select count(*) from event e
		 	  left join GeosForEvents g on e.id = g.event_id
		 	  where e.name like {filter} and category_id = {category} and g.geo_id in (%s) 
        """)
    }


    DB.withConnection { implicit connection =>

      var events = SQL(eventsQuery.format(geoIdsList.map("'%s'".format(_)).mkString(","))).on('pageSize -> pageSize,
        'offset -> offest,
        'filter -> filter,
        'orderBy -> orderBy,
        'category->category).as(simple *)

      //*getting geoId

      var totalRows = SQL(
        eventsCountQuery.format(geoIdsList.map("'%s'".format(_)).mkString(","))).on(
          'filter -> filter).as(scalar[Long].single)

      Page(events, page, offest, totalRows)
    }

  }

}

object Event_Categories {

  /**
   * Parse a Company from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("event_categories.id") ~
      get[String]("event_categories.name") map {
        case id ~ name => Post_Categories(id, name)
      }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Map[String, String] = DB.withConnection { implicit connection =>
    SQL("select * from event_categories order by name").as(Event_Categories.simple *).map(c => c.id.toString -> c.name).toMap
  }
  
    def optionsSeq: Seq[(String, String)] = DB.withConnection { implicit connection =>
    SQL("select * from event_categories order by name").as(Event_Categories.simple *).map(c => c.id.toString -> c.name)
  }

}
