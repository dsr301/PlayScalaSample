package models

import java.util.{ Date }
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import collection.immutable.IndexedSeq
import play.api.cache.Cache

/**
 *  Class to indicate the level/type of business
 */
case class Business_Level(id: Pk[Long] = NotAssigned, name: String, parentId: Option[Long])

/**
 * Class to represent the business
 */
case class Business(id: Pk[Long], name: String, geoId: Option[Long], ownerId: Option[Long], typeId: Option[Long], phone: String, address: String, avgRating: Option[String], numRatings: Option[Long])

/**
 * Util class used as form representation for business. Required as Business just has geo and we need both city and region
 */
case class BusinessForm(name: String, cityId: Option[Long], regId: Option[Long], ownerId: Option[Long], typeId: Option[Long], phone: String, address: String)

object Business {

  /**
   * Parse a Business from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("business_services.id") ~
      get[String]("business_services.name") ~
      get[Option[Long]]("business_services.geoId") ~
      get[Option[Long]]("business_services.ownerId") ~
      get[Option[Long]]("business_services.typeId") ~
      get[Option[String]]("business_services.phone") ~
      get[String]("business_services.address") ~
      get[Option[java.math.BigDecimal]]("avgRating") ~
      get[Option[Long]]("numRating") map {
        case id ~ name ~ geoId ~ ownerId ~ typeId ~ phone ~ address ~ avgRating ~ numRatings => Business(id, name, geoId, ownerId, typeId, phone.map(_.toString).getOrElse(""), address, avgRating.map(_.toString), numRatings)
      }
  }

  val withCompany = Business.simple ~ (Business_Level.simple ?) ~ (Geo.simple ?) ~ (Users.simple ?) ~ (Region.simple ?) map {
    case business ~ level ~ geo ~ users ~ region => (business, level, geo, users, region)
  }

  /**
   * to convert the business object to corresponding for object
   */
  def toForm(bus: Business): BusinessForm = {
    val cityreg = Geo.getCityRegionId(bus.geoId.map(x => x).getOrElse(0)).get
    BusinessForm(bus.name, Some(cityreg._1), Some(cityreg._2), bus.ownerId, bus.typeId, bus.phone, bus.address)
  }

  // -- Queries

  /**
   * Retrieve a Business from the id.
   */
  def findById(id: Long): Option[Business] = {
    DB.withConnection { implicit connection =>
      SQL("select *,round(avg(rr.rating),1) as avgrating,count(rr.rating) as numRating from business_services left join reviews_ratings rr on business_services.id = rr.businessId where business_services.id = {id} ").on('id -> id).as(Business.simple.singleOpt)
    }
  }

  /**
   * Retrieve a Business level from the id.
   */
  def getBusinessTypeName(typeId: Long) = {
    Cache.getOrElse[Option[String]]("businessTypeName" + typeId.toString) {
      val query = """
      select name from business_hierarchy where id = {id}
      """

      DB.withConnection { implicit connection =>
        println(SQL(query))
        SQL(query).on('id -> typeId).as(scalar[String].singleOpt)

      }
    }

  }

  /**
   * Return a page of (Business,Other dependent data).
   *
   * @param page Page to display
   * @param pageSize Number of Businesss per page
   * @param orderBy Business property used for sorting
   * @param filter Filter applied on the name column
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%", businessType: Int, city: Int, region: Int): Page[(Business, Option[Business_Level], Option[Geo], Option[Users], Option[Region])] = {

    val offest = pageSize * page
    val geoIdsList = Geo.getGeoIds(city, region);

    //check the businessType and if o return all businesses

    var (businessQuery, businessCountQuery) = if (businessType == 0) {
      ("""
           select *,round(avg(rr.rating),2) as avgrating,count(rr.rating) as numRating from business_services 
          left join business_hierarchy on business_services.typeId = business_hierarchy.id
          left join geo on business_services.geoId = geo.id 
          left join users on business_services.ownerId = users.id
          left join reviews_ratings rr on business_services.id = rr.businessId 
          join region on geo.region = region.id
          where business_services.name like {filter}  and business_services.geoId in (%s)
          group by business_services.id order by {orderBy} 
          limit {pageSize} offset {offset}
        """, """
          select count(*) from business_services 
          left join business_hierarchy on business_services.typeId = business_hierarchy.id
          left join geo on business_services.geoId = geo.id 
          left join users on business_services.ownerId = users.id
          where business_services.name like {filter} and business_services.geoId in (%s)
        """)
    } else {
      ("""
           select *,round(avg(rr.rating),2) as avgrating,count(rr.rating) as numRating from business_services 
          left join business_hierarchy on business_services.typeId = business_hierarchy.id
          left join geo on business_services.geoId = geo.id 
          left join users on business_services.ownerId = users.id
          left join reviews_ratings rr on business_services.id = rr.businessId 
    	  join region on geo.region = region.id
          where business_services.name like {filter} and business_services.typeId = {businessId}  and business_services.geoId in (%s)
          group by business_services.id order by {orderBy} 
          limit {pageSize} offset {offset}
        """, """
          select count(*) from business_services 
          left join business_hierarchy on business_services.typeId = business_hierarchy.id
          left join geo on business_services.geoId = geo.id 
          left join users on business_services.ownerId = users.id
          where business_services.name like {filter} and business_services.typeId = {businessId} and business_services.geoId in (%s)
        """)
    }

    DB.withConnection { implicit connection =>

      var business = SQL(businessQuery.format(geoIdsList.map("'%s'".format(_)).mkString(","))).on('pageSize -> pageSize,
        'offset -> offest,
        'filter -> filter,
        'orderBy -> orderBy,
        'businessId -> businessType).as(Business.withCompany *)

      var totalRows = SQL(
        businessCountQuery.format(geoIdsList.map("'%s'".format(_)).mkString(","))).on(
          'filter -> filter,
          'businessId -> businessType,
          'city -> city,
          'region -> region).as(scalar[Long].single)

      Page(business, page, offest, totalRows)

    }

  }

  /**
   * Update a Business.
   *
   * @param id The Business id
   * @param Business The Business values.
   */
  def update(id: Long, business: BusinessForm) = {
    var goe = Geo.getGeoId(business.cityId.get, business.regId.get);
    DB.withConnection { implicit connection =>
      SQL(
        """
          update business_services
          set name = {name}, geoId = {geoId}, ownerid = {ownerid}, typeid = {typeid},phone={phone},address={address}
          where id = {id}
        """).on(
          'id -> id,
          'name -> business.name,
          'geoId -> goe,
          'ownerid -> business.ownerId,
          'typeid -> business.typeId,
          'phone -> business.phone,
          'address -> business.address).executeUpdate()
    }
  }

  /**
   * Insert a new Business.
   *
   * @param Business The Business values.
   */
  def insert(business: BusinessForm, city: Long, reg: Long) = {
    val geo = Geo.getGeoId(city, reg)
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into business_services (name,geoId,ownerId,typeId,phone,address) values ( 
            {name}, {geoId}, {ownerid}, {typeid},{categoryId},{phone},{address}
          )
        """).on(
          'name -> business.name,
          'geoId -> geo,
          'ownerid -> business.ownerId,
          'typeid -> business.typeId,
          'phone -> business.phone,
          'address -> business.address).executeUpdate()
    }
  }

  /**
   * Delete a Business.
   *
   * @param id Id of the Business to delete.
   */
  def delete(id: Long) = {
    DB.withConnection { implicit connection =>
      SQL("delete from business_services where id = {id}").on('id -> id).executeUpdate()
    }
  }

}

object Business_Level {

  /**
   * Parse Business_Level from ResultSet
   */
  val simple = {
    get[Pk[Long]]("business_hierarchy.id") ~
      get[String]("business_hierarchy.name") ~
      get[Option[Long]]("business_hierarchy.parentid") map {
        case id ~ name ~ parentid => Business_Level(id, name, parentid)
      }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Seq[(String, String)] = DB.withConnection { implicit connection =>
    SQL("select * from business_hierarchy order by name").as(Business_Level.simple *).map(c => c.id.toString -> c.name)
  }

}
