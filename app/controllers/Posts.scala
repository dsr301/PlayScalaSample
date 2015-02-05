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


object Posts extends Controller {
  val commentForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "user" -> longNumber,
      "content" -> nonEmptyText,
      "postedAt" -> date("yyyy-MM-dd"),
      "postId" -> longNumber)(Comment.apply)(Comment.unapply))

  val postForm = Form(
    mapping(
      "id" -> ignored(NotAssigned: Pk[Long]),
      "content" -> nonEmptyText,
      "postedAt" -> date("yyyy-MM-dd"),
      "authorId" -> longNumber,
      "categoryId" -> longNumber)(Post.apply)(Post.unapply))

  def list(page: Int, orderBy: Int, filter: String, city: Int, region: Int) = Action { implicit request =>
    val allPosts = Post.list(page = page, orderBy = orderBy, filter = ("%" + filter + "%"), city = city, region = region)
    Ok(views.html.posts.index(
      allPosts, commentForm, Post_Categories.options))

  }

  def disqus = Action {
    Ok(views.html.posts.disqus())
  }

  def index = Action {
    val allPosts = Post.allWithAuthorAndComments
    Ok(views.html.posts.index(
      allPosts, commentForm, Post_Categories.options))
  }

  def show(id: Long) = Action { implicit request =>
    Post.byIdWithAuthorAndComments(id).map {
      post =>
        Ok(views.html.posts.show(post, post._1.prevNext, commentForm))
    } getOrElse {
      NotFound("No such Post")
    }
  }

  def postComment(postId: Long) = Action { implicit request =>
    commentForm.bindFromRequest.fold(
      formWithErrors => {
        val post = Post.byIdWithAuthorAndComments(postId).get
        BadRequest(views.html.posts.show(post, post._1.prevNext, formWithErrors))
      },
      comment => {
        Comment.create(comment)
        val allPosts = Post.allWithAuthorAndComments
        Ok(views.html.posts.index(
          allPosts, commentForm, Post_Categories.options))
      })
  }

  def addPost(cId: Int, rId: Int) = Action { implicit request =>
    postForm.bindFromRequest.fold(
      formWithErrors => {
        val allPosts = Post.allWithAuthorAndComments
        BadRequest(views.html.posts.index(
          allPosts, commentForm, Post_Categories.options))
      },
      post => {
        Post.setGeoId(Post.create(post), cId, rId)
        val allPosts = Post.list(page = 1, orderBy = 2, filter = ("%" + "" + "%"), city = cId, region = rId)
        Ok(views.html.posts.index(
          allPosts, commentForm, Post_Categories.options))
        //        Redirect(routes.Application.index).flashing("success" -> "Thanks for posting")
      })
  }
  /*
  val captchaService: ImageCaptchaService = new DefaultManageableImageCaptchaService

  def captcha(id: String) = Action { implicit request =>
    // https://groups.google.com/forum/?fromgroups#!searchin/play-framework/2.0$20image/play-framework/5h5wh3eCiYo/1uTKQ2AQ3g4J
    // http://stackoverflow.com/questions/8305853/how-to-render-a-binary-with-play-2-0
    // http://d.hatena.ne.jp/kaiseh/20090502/1241286415
    val baos = new ByteArrayOutputStream
    ImageIO.write(captchaService.getImageChallengeForID(id, Locale.getDefault()), "jpg", baos);
    Ok(baos.toByteArray()).as("image/jpeg")
  }*/

  def listTagged(tag: String) = Action { implicit request =>
    Ok(views.html.posts.listTagged(Post.findTaggedWith(tag), tag))
  }
}