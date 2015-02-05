/**
 * Copyright 2012 Jorge Aliss (jaliss at gmail dot com) - twitter: @jaliss
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package service

import play.api.{Logger, Application}
import securesocial.core._
import securesocial.core.providers.Token
import securesocial.core.IdentityId
import models.PlatformUser


/**
 * A Sample In Memory user service in Scala
 *
 * IMPORTANT: This is just a sample and not suitable for a production environment since
 * it stores everything in memory.
 */
class NLUserService(application: Application) extends UserServicePlugin(application) {
  private var tokens = Map[String, Token]()

  def find(id: IdentityId): Option[PlatformUser] = {
    if ( Logger.isDebugEnabled ) {
//      Logger.debug("users = %s".format(users))
    }
//    users.get(id.userId + id.providerId)
    PlatformUser.find(id.userId, id.providerId)
  }

  def findByEmailAndProvider(email: String, providerId: String): Option[PlatformUser] = {
    if ( Logger.isDebugEnabled ) {
//      Logger.debug("users = %s".format(users))
    }
//    users = users + ("dsr301@gmail.com",null)
//    users.values.find( u => u.email.map( e => e == email).getOrElse(false))
//    users.values.find( u => u.email.map( e => e == email && u.identityId.providerId == providerId).getOrElse(false))

      PlatformUser.findByEmailAndProvider(email, providerId)
  
  }

  def save(user: Identity): Identity = {
//    users = users + (user.identityId.userId + user.identityId.providerId -> user)
      PlatformUser.save(SocialUser(user.identityId,user.firstName,user.lastName,user.fullName,user.email,user.avatarUrl,user.authMethod,user.oAuth1Info,user.oAuth2Info,user.passwordInfo))

    // this sample returns the same user object, but you could return an instance of your own class
    // here as long as it implements the Identity trait. This will allow you to use your own class in the protected
    // actions and event callbacks. The same goes for the find(id: IdentityId) method.
    user
  }

  def save(token: Token) {
    tokens += (token.uuid -> token)
  }

  def findToken(token: String): Option[Token] = {
    tokens.get(token)
  }

  def deleteToken(uuid: String) {
    tokens -= uuid
  }

  def deleteTokens() {
    tokens = Map()
  }

  def deleteExpiredTokens() {
    tokens = tokens.filter(!_._2.isExpired)
  }
}
