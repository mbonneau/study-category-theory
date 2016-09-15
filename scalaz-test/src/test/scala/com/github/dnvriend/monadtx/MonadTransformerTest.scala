/*
 * Copyright 2016 Dennis Vriend
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
 */

package com.github.dnvriend.monadtx

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._
import scala.concurrent.Future
import scala.util.Random

class MonadTransformerTest extends TestSpec {

  // The classical example; the 'Future[Option[A]]' problem

  final case class User(id: Long, name: String)

  final case class Role(id: Long, role: String)

  final case class Permission(id: Long, name: String)

  class Dao {
    val names = List("foo", "bar", "baz")
    val roles = List("admin", "manager", "user")
    val permissions = List(Permission(1, "edit"), Permission(2, "copy"), Permission(3, "delete"), Permission(4, "print"), Permission(5, "view"))

    def randomName: String = names(Random.nextInt(names.length))

    def randomRole: String = roles(Random.nextInt(roles.length))

    def randomPermission: Permission = permissions(Random.nextInt(permissions.length))

    def randomId: Long = Math.abs(Random.nextLong())

    def getUserById(id: Long): Future[Option[User]] =
      (id == 2) ? Future.successful(Option.empty[User]) | Future.successful(User(id, randomName).some)

    def getRoleForUser(user: User): Future[Option[Role]] =
      (user.id == 3) ? Future.successful(Option.empty[Role]) | Future.successful(Role(user.id, randomRole).some)

    def getPermissionForRole(role: Role): Future[Option[Permission]] = role.id match {
      case 1 => Future.successful(permissions.headOption)
      case 4 => Future.successful(Option.empty[Permission])
      case _ => Future.successful(randomPermission.some)
    }
  }

  def withDao(f: Dao => Unit): Unit =
    f(new Dao)

  class Service(dao: Dao) {
    def findPermissionForUserId(userId: Long): Future[Option[Permission]] =
      (for {
        user <- OptionT(dao.getUserById(userId))
        role <- OptionT(dao.getRoleForUser(user))
        permission <- OptionT(dao.getPermissionForRole(role))
      } yield permission).run

    def findPermissionForUserIdBetterFeedback(userId: Long): Future[Disjunction[String, Permission]] =
      (for {
        user <- EitherT(dao.getUserById(userId).map(_.toRightDisjunction(s"No user found for id: '$userId'")))
        role <- EitherT(dao.getRoleForUser(user).map(_.toRightDisjunction(s"No role found for user.id: '${user.id}'")))
        permission <- EitherT(dao.getPermissionForRole(role).map(_.toRightDisjunction(s"No permission found for role.id: '${role.id}'")))
      } yield permission).run
  }

  def withService(f: Service => Unit): Unit =
    withDao(dao => f(new Service(dao)))

  it should "compose multiple Future[Option[A]] ie. 'stacked monads' in a for-expression using an Option transformer and should be defined" in withService { service =>
    service.findPermissionForUserId(1).futureValue shouldBe Some(Permission(1, "edit"))
  }

  it should "compose stacked Future[Option[A]] in a for-expression using an Option transformer and should not be defined" in withService { service =>
    service.findPermissionForUserId(2).futureValue shouldBe None
  }

  it should "compose stacked Future[Disjunction] types, to solve the 'silent-failure' problem that using the Option type introduce" in withService { service =>
    service.findPermissionForUserIdBetterFeedback(1).futureValue shouldBe \/-(Permission(1, "edit"))
    service.findPermissionForUserIdBetterFeedback(2).futureValue shouldBe -\/("No user found for id: '2'")
    service.findPermissionForUserIdBetterFeedback(3).futureValue shouldBe -\/("No role found for user.id: '3'")
    service.findPermissionForUserIdBetterFeedback(4).futureValue shouldBe -\/("No permission found for role.id: '4'")
  }
}
