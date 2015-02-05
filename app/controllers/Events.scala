package controllers

import play.api._
import play.api.mvc._
import models._
import play.api.data._
import play.api.data.Forms._
import anorm._
import play.api.data.validation.Constraints._
import anorm.NotAssigned
import java.util.Date
import javax.imageio.ImageIO
import java.io.ByteArrayOutputStream
import java.util.Locale
import play.api.cache.Cache



object Events extends Controller {

  val eventsForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "name" -> nonEmptyText,
      "description" -> nonEmptyText,
      "venue" -> nonEmptyText,
      "from" -> date("yyyy-MM-dd"),
      "to" -> date("yyyy-MM-dd"),
      "startTime"-> nonEmptyText,
      "endTime"-> nonEmptyText,
      "postedAt" -> date("yyyy-MM-dd"),
      "authorId" -> longNumber,
      "categoryId" -> longNumber,
      "cityId" -> longNumber,
      "regId" -> longNumber)(EventForm.apply)(EventForm.unapply))

  def list(page: Int, orderBy: Int, filter: String, city: Int, region: Int,category:Long) = Action { implicit request =>
    val allPosts = Event.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"), city = city, region = region,category=category)
    Ok(views.html.events.list(
      allPosts, orderBy, filter, Event_Categories.options))

  }

  def create = Action {
    Ok(views.html.events.createForm(eventsForm,Event_Categories.optionsSeq))
  }

  def addEvent(cityId:Long,regId:Long) = Action(parse.multipartFormData)  { implicit request =>
    eventsForm.bindFromRequest.fold(
      formWithErrors => {
//       val allPosts = Event.list(0, 1, filter = ("%" + "" + "%"), city = cityId.toInt, region = regId.toInt)
        BadRequest(views.html.events.createForm(
          formWithErrors, Event_Categories.optionsSeq))
      },
      event => {
        val eId = Event.create(event)
        Event.setGeoId(eId, event.cityId, event.regId)
                request.body.file("Photo").map { picture =>
          import java.io.File
          val filename = picture.filename
          val contentType = picture.contentType
          picture.ref.moveTo(new File("/home/suman/Documents/nearlook/public/images/events/"+eId+".png"))
          Ok("File uploaded")
        }
        val allEvents = Event.list(0, 1, filter = ("%" + "" + "%"), city = event.cityId.toInt, region = event.regId.toInt,category=0)
        Ok(views.html.events.list(
           allEvents, 0,"", Event_Categories.options))
        //        Redirect(routes.Application.index).flashing("success" -> "Thanks for posting")
      })
  }

}