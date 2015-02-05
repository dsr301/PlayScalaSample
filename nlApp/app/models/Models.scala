package models

import java.util.{Date}

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class Business_Level(id: Pk[Long] = NotAssigned, name: String,parentId:Option[Long])

case class Business(id: Pk[Long],name: String, geoId: Option[Long],ownerId: Option[Long],typeId: Option[Long],categoryId: Option[Long])

/**
 * Helper for pagination.
 */
case class Page[(Business,Option[Business_Level],Option[geo])](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

object Business {
  
  // -- Parsers
  
  /**
   * Parse a Computer from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("business_services.id") ~
    get[String]("business_services.name") ~
    get[Option[Long]]("business_services.geoId")~
    get[Option[Long]]("business_services.ownerId")~ 
    get[Option[Long]]("business_services.typeId")~ 
    get[Option[Long]]("business_services.categoryId") map {
      case id~name~geoId~ownerId~typeId~categoryId => Business(id,name,geoId,ownerId,typeId,categoryId)
    }
  }
  
  /**
   * Parse a (Computer,Company) from a ResultSet
   */
//  val withCompany = Business.simple ~ (Business_Level.simple ?) map {
//    case business~level => (business,level)
//  }
  
    val withCompany = Business.simple ~ (Business_Level.simple ?) ~(geo.simple ?) map {
    case business~level~geo => (business,level,geo)
  }
  
  // -- Queries
  
  /**
   * Retrieve a computer from the id.
   */
  def findById(id: Long): Option[Business] = {
    DB.withConnection { implicit connection =>
      SQL("select * from business_services where id = {id}").on('id -> id).as(Business.simple.singleOpt)
    }
  }
  
  /**
   * Return a page of (Computer,Company).
   *
   * @param page Page to display
   * @param pageSize Number of computers per page
   * @param orderBy Computer property used for sorting
   * @param filter Filter applied on the name column
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%"): Page[(Business, Option[Business_Level],Option[geo])] = {
    
    val offest = pageSize * page
    
    DB.withConnection { implicit connection =>
      
      val business = SQL(
        """
          select * from business_services 
          left join business_hierarchy on business_services.typeId = business_hierarchy.id
          where business_services.name like {filter}
          order by {orderBy}
          limit {pageSize} offset {offset}
        """
      ).on(
        'pageSize -> pageSize, 
        'offset -> offest,
        'filter -> filter,
        'orderBy -> orderBy
      ).as(Business.withCompany *)

      val totalRows = SQL(
        """
          select count(*) from business_services 
          left join business_hierarchy on business_services.typeId = business_hierarchy.id
          where business_services.name like {filter}
        """
      ).on(
        'filter -> filter
      ).as(scalar[Long].single)

      Page(business, page, offest, totalRows)
      
    }
    
  }
  
  /**
   * Update a computer.
   *
   * @param id The computer id
   * @param computer The computer values.
   */
  def update(id: Long, business: Business) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          update business_services
          set name = {name}, geoId = {geoId}, ownerid = {ownerid}, typeid = {typeid},categoryId = {categoryId}
          where id = {id}
        """
      ).on(
        'id -> id,
        'name -> business.name,
        'geoId -> business.geoId,
        'ownerid -> business.ownerId,
        'typeid-> business.typeId,
        'categoryId->business.categoryId
      ).executeUpdate()
    }
  }
  
  /**
   * Insert a new computer.
   *
   * @param computer The computer values.
   */
  def insert(business: Business) = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into business_services values (
            (select next value for computer_seq), 
            {name}, {geoId}, {ownerid}, {typeid},{categoryId}
          )
        """
      ).on(
        'name -> business.name,
        'geoId -> business.geoId,
        'ownerid -> business.ownerId,
        'typeid-> business.typeId,
        'categoryId->business.categoryId
      ).executeUpdate()
    }
  }
  
  /**
   * Delete a computer.
   *
   * @param id Id of the computer to delete.
   */
  def delete(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL("delete from computer where id = {id}").on('id -> id).executeUpdate()
    }
  }
  
}

object Business_Level {
    
  /**
   * Parse a Company from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("business_hierarchy.id") ~
    get[String]("business_hierarchy.name")~ 
    get[Option[Long]]("business_hierarchy.parentid") map {
      case id~name~parentid => Business_Level(id, name,parentid)
    }
  }
  
  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Seq[(String,String)] = DB.withConnection { implicit connection =>
    SQL("select * from business_hierarchy order by name").as(Business_Level.simple *).map(c => c.id.toString -> c.name)
  }
  
}

