package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import anorm._
import views._
import models._
import play.api.data._
import play.api.data.validation.Constraints._
import securesocial.core.{ Identity, Authorization }
import play.api.libs.concurrent.Execution.Implicits.defaultContext

object Application extends Controller with securesocial.core.SecureSocial {

  /**
   * This result directly redirect to the application home.
   */
  val Home = routes.Application.list(0, 2, "", 0, 0, 0)

  /**
   * Describe the business form (used in both edit and create screens).
   */
  val businessForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "cityId" -> optional(longNumber),
      "regId" -> optional(longNumber),
      "ownerId" -> optional(longNumber),
      "typeId" -> optional(longNumber),
      "phone" -> nonEmptyText,
      "address" -> nonEmptyText)(BusinessForm.apply)(BusinessForm.unapply))

  /**
   * Describe the business review form (used rating screens).
   */
  val reviewForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "businessId" -> optional(longNumber),
      "userId" -> optional(longNumber),
      "text" -> optional(text),
      "rating" -> optional(longNumber(min = 0, max = 5)),
      "abusive" -> optional(boolean))(Reviews_ratingsForm.apply)(Reviews_ratingsForm.unapply))
  // -- Actions

  /**
   * Handle default path requests, redirect to business list
   */
  def index = Action { static }

  def static = Ok(html.home())
  /**
   * Display the paginated list of business.
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on Business names
   */
  def list(page: Int, orderBy: Int, filter: String, businessType: Int, city: Int, region: Int) = Action.async { implicit request =>
    
    scala.concurrent.Future {
          Ok(html.list(
      Business.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"), businessType = businessType, city = city, region = region),
      orderBy, filter, businessType, getName(city, region, businessType), "")).withNewSession
    }


  }

  def getLastRegId = Action { request =>
    request.session.get("regId").map { reg =>
      Ok(reg)
    }.getOrElse {
      Ok("none")
    }
  }
  /**
   * Display the 'edit form' of a existing business.
   *
   * @param id Id of the business to edit
   */
  def edit(id: Long) = Action {
    Business.findById(id).map { business =>
      val bf = businessForm.fill(Business.toForm(business))
      val c = business.geoId.map(x => Geo.getCityRegionId(x).get).get
      Ok(html.editForm(id, bf, Business_Level.options, City.options, Users.options, Region.options(c._1)))
    }.getOrElse(NotFound)
  }

  def getRegion(cityId: Long): Action[AnyContent] = Action {

    Ok(Geo.getRegionJson(cityId))
  }
  /**
   *
   */
  def show(id: Long) = Action {
    showIt(id).withSession(
      "regId" -> Integer.toString(Business.findById(id).flatMap(b => b.geoId.flatMap(gId => Geo.getCityRegionId(gId))).map(k => k._2.intValue).get))
  }

  def uploadForm(id: Long) = Action {
    Ok(html.uploadForm(id))
  }

  def uploadPhotos(id: Long) = Action(parse.multipartFormData) { implicit request =>
    val files = List("l", "p1", "p2", "p3")
    files.map { name =>
      request.body.file(name).map { picture =>
        import java.io.File
        val filename = picture.filename
        val contentType = picture.contentType
        picture.ref.moveTo(new File("/home/suman/Documents/nearlook/public/images/" + (Business.findById(id).get.name.replaceAll("\\s", "") + "/" + id + name) + ".png"))
        Ok("File uploaded")
      }
    }

    showIt(id)
    //Home.flashing("success" -> "Business %s has been created".format(business.name))
  }
  /**
   *
   */
  def showIt(id: Long) = Ok(html.show(Business.findById(id), Reviews_ratings.findById(id)))

  /**
   * Handle the 'edit form' submission
   *
   * @param id Id of the business to edit
   */
  def update(id: Long) = Action { implicit request =>
    businessForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.editForm(id, formWithErrors, Business_Level.options, City.options, Users.options, Region.options(businessForm.get.cityId.get))),
      business => {
        Business.update(id, business)
        val cr = (business.cityId.get.intValue(), business.regId.get.intValue())
        Ok(html.list(
          Business.list(businessType = 0, city = cr._1, region = cr._2),
          2, "", 1, getName(cr._1, cr._2, 1), "%s is updated".format(business.name)))
      })
  }

  /**
   * Display the 'new Business form'.
   */
  def create = Action {
    Ok(html.createForm(businessForm, Business_Level.options, Geo.options, Users.options, Region.options))
  }

  /**
   * Display the 'new business form'.
   */
  def rate(bId: Long) = SecuredAction { implicit request =>
    Ok(html.rateForm(reviewForm, Business.findById(bId)))
  }

  def saveRating = Action { implicit request =>
    reviewForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.rateForm(formWithErrors, Business.findById(reviewForm.get.businessId.map(x => x).getOrElse(0)))),
      rating => {
        Reviews_ratings.insert(rating)
        showIt(rating.businessId.map(x => x).getOrElse(0))
      })
  }

  /**
   *
   */
  def save() = Action(parse.multipartFormData) { implicit request =>
    businessForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.createForm(formWithErrors, Business_Level.options, Geo.options, Users.options, Region.options)),
      business => {
        var c: Long = business.cityId.map(x => x).getOrElse(0);
        var r: Long = business.regId.map(x => x).getOrElse(0);
        val id = Business.insert(business, c, r)
        request.body.file("picture").map { picture =>
          import java.io.File
          val filename = picture.filename
          val contentType = picture.contentType
          picture.ref.moveTo(new File("/home/suman/Documents/nearlook/public/images/" + business.name.replaceAll("\\s", "") + "/" + id + "l" + ".png"))
          Ok("File uploaded")
        }
        Ok(html.list(
          Business.list(businessType = 0, city = c.intValue(), region = r.intValue()),
          2, "", 1, getName(c, r, 1), "%s is added".format(business.name)))
        //Home.flashing("success" -> "Business %s has been created".format(business.name))
      })
  }

  /**
   * Handle Business deletion.
   */
  def delete(id: Long) = Action { implicit request =>
    Business.delete(id)
    Ok(html.list(
      Business.list(businessType = 0, city = 0, region = 0),
      2, "", 1, getName(0, 0, 1), "Deleted"))
  }

  def showBusiness = Action { implicit request =>
    Ok(html.businessPage())
  }

  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsRoutes")(routes.javascript.Application.getRegion, routes.javascript.Application.getLastRegId, routes.javascript.Application.list, routes.javascript.Application.save)).as("text/javascript")
  }

  def getName(cityId: Long, regId: Long, typeId: Long) = {
    (Geo.getCityName(cityId).map(x => x).getOrElse("City"), Geo.getRegionName(regId).map(x => x).getOrElse("Region"), if (typeId == 0) "Businesses" else Business.getBusinessTypeName(typeId).map(x => x).getOrElse("Total"))
  }
}
            
