package models

import securesocial.core.Identity
import securesocial.core.IdentityId
import securesocial.core.AuthenticationMethod
import securesocial.core.OAuth1Info
import securesocial.core.OAuth2Info
import securesocial.core.PasswordInfo
import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import securesocial.core.SocialUser

case class PlatformUser(id: Pk[Long] = NotAssigned,identityId: IdentityId, firstName: String, lastName: String, fullName: String, email: Option[String],
                      avatarUrl: Option[String], authMethod: AuthenticationMethod,
                      oAuth1Info: Option[OAuth1Info] = None,
                      oAuth2Info: Option[OAuth2Info] = None,
                      passwordInfo: Option[PasswordInfo] = None) extends Identity
object PlatformUser {

  val simple = {
	  get[Pk[Long]]("users.id") ~
      get[String]("users.uName") ~
      get[String]("users.providerId") ~
      get[String]("users.firstName") ~
      get[String]("users.lastName") ~
      get[String]("users.fullName") ~
      get[Option[String]]("users.email") ~
      get[Option[String]]("users.avatarUrl") ~
      get[String]("users.authMethod") ~
      get[Option[String]]("users.oa1Token") ~
      get[Option[String]]("users.oa1Secret") ~ 
      get[Option[String]]("users.oa2Token") ~ 
      get[Option[String]]("users.oa2TokenType") ~ 
      get[Option[Int]]("users.oa2ExpiresIn") ~ 
      get[Option[String]]("users.oa2RefreshToken") ~ 
      get[Option[String]]("users.hasher") ~ 
      get[Option[String]]("users.password") ~  
      get[Option[String]]("users.salt") map {
        case id ~uName ~ providerId ~ firstName ~ lastName ~ fullName ~ email ~ avatarUrl ~ authMethod ~ oa1Token ~ oa1Secret ~ oa2Token ~ oa2TokenType ~ oa2ExpiresIn ~ oa2RefreshToken ~ hasher ~ password ~ salt=> PlatformUser(id,IdentityId(uName,providerId), firstName, lastName, fullName, email, avatarUrl,AuthenticationMethod(authMethod),oa1Token.map(oa1T=>OAuth1Info(oa1T,oa1Secret.get)),oa2Token.map(oa2T=>OAuth2Info(oa2T,oa2TokenType,oa2ExpiresIn,oa2RefreshToken)),password.map(pwd=>PasswordInfo(hasher.get,pwd,salt)))
      }
  }
  
    /**
   * Create a users.
   */
  def save(users: SocialUser): SocialUser = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into users(uName, providerId, firstName, lastName,fullName, email, avatarUrl, authMethod,oa1Token, oa1Secret, oa2Token, oa2TokenType,oa2ExpiresIn, oa2RefreshToken, hasher, password,salt) values (
          {uName}, {providerId}, {firstName}, {lastName},{fullName}, {email}, {avatarUrl}, {authMethod},{oa1Token}, {oa1Secret}, {oa2Token}, {oa2TokenType},{oa2ExpiresIn}, {oa2RefreshToken}, {hasher}, {password},{salt}
          ) ON DUPLICATE KEY UPDATE providerId={providerId}, firstName={firstName}, lastName={lastName},fullName={fullName}, email={email}, avatarUrl={avatarUrl}, authMethod={authMethod},oa1Token={oa1Token}, oa1Secret={oa1Secret}, oa2Token={oa2Token}, oa2TokenType={oa2TokenType},oa2ExpiresIn={oa2ExpiresIn}, oa2RefreshToken={oa2RefreshToken}, hasher={hasher}, password={password},salt={salt}
        """).on(
          'uName -> users.identityId.userId,
          'providerId -> users.identityId.providerId,
          'firstName -> users.firstName,
          'lastName -> users.lastName,
          'fullName -> users.fullName,
          'email -> users.email,
          'avatarUrl -> users.avatarUrl,
          'authMethod -> users.authMethod.method,
          'oa1Token -> users.oAuth1Info.map(oa1=>oa1.token),
          'oa1Secret -> users.oAuth1Info.map(oa1=>oa1.secret),
          'oa2Token -> users.oAuth2Info.map(oa2=>oa2.accessToken),
          'oa2TokenType -> users.oAuth2Info.map(oa2=>oa2.tokenType),
          'oa2ExpiresIn -> users.oAuth2Info.map(oa2=>oa2.expiresIn),
          'oa2RefreshToken -> users.oAuth2Info.map(oa2=>oa2.refreshToken),
          'hasher -> users.passwordInfo.map(pwd=>pwd.hasher),
          'password -> users.passwordInfo.map(pwd=>pwd.password),
          'salt -> users.passwordInfo.map(pwd=>pwd.salt)).execute()
      users
    }
  }
  
  def find(uId:String,providerId:String): Option[PlatformUser] ={
        DB.withConnection { implicit connection =>
      SQL("select * from users where uName = {uId} and providerId={providerId}").on('uId -> uId,
          'providerId->providerId).as(PlatformUser.simple.singleOpt)
    }
  }

  def findByEmailAndProvider(email: String, providerId: String): Option[PlatformUser]={
            DB.withConnection { implicit connection =>
      SQL("select * from users where email = {email} and providerId={providerId}").on('email -> email,
          'providerId->providerId).as(PlatformUser.simple.singleOpt)
    }
  }
  
  def findById(id: Long): Option[PlatformUser] = {
    DB.withConnection { implicit connection =>
      SQL("select * from users where id = {id} ").on('id -> id).as(PlatformUser.simple.singleOpt)
    }
  }
    
}