package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import anorm._
import views._
import models._
import play.data.format.Formats.NonEmpty

/**
 * Manage a database of computers
 */
object Application extends Controller { 
  
  /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Application.list(0, 2, ""))
  
  /**
   * Describe the computer form (used in both edit and create screens).
   */ 
  val businessForm = Form(
    mapping(
      "id" -> ignored(NotAssigned:Pk[Long]),
      "name" -> nonEmptyText,
      "geoId" -> optional(longNumber),
      "ownerId" -> optional(longNumber),
      "typeId" -> optional(longNumber),
      "categoryId" -> optional(longNumber)
    )(Business.apply)(Business.unapply)
  )
  
  // -- Actions

  /**
   * Handle default path requests, redirect to computers list
   */  
  def index = Action { Home }
  
  /**
   * Display the paginated list of computers.
   *
   * @param page Current page number (starts from 0)
   * @param orderBy Column to be sorted
   * @param filter Filter applied on computer names
   */
  def list(page: Int, orderBy: Int, filter: String) = Action { implicit request =>
    Ok(html.list(
      Business.list(page = page, orderBy = orderBy, filter = ("%"+filter+"%")),
      orderBy, filter
    ))
  }
  
  /**
   * Display the 'edit form' of a existing Computer.
   *
   * @param id Id of the computer to edit
   */
  def edit(id: Long) = Action {
    Business.findById(id).map { business =>
      Ok(html.editForm(id, businessForm.fill(business), Business_Level.options))
    }.getOrElse(NotFound)
  }
  
  /**
   * Handle the 'edit form' submission 
   *
   * @param id Id of the computer to edit
   */
  def update(id: Long) = Action { implicit request =>
    businessForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.editForm(id, formWithErrors, Business_Level.options)),
      business => {
        Business.update(id, business)
        Home.flashing("success" -> "Business %s has been updated".format(business.name))
      }
    )
  }
  
  /**
   * Display the 'new computer form'.
   */
  def create = Action {
    Ok(html.createForm(businessForm, Business_Level.options))
  }
  
  /**
   * Handle the 'new computer form' submission.
   */
  def save = Action { implicit request =>
    businessForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.createForm(formWithErrors, Business_Level.options)),
      business => {
        Business.insert(business)
        Home.flashing("success" -> "Business %s has been created".format(business.name))
      }
    )
  }
  
  /**
   * Handle computer deletion.
   */
  def delete(id: Long) = Action {
    Business.delete(id)
    Home.flashing("success" -> "Computer has been deleted")
  }

}
            
