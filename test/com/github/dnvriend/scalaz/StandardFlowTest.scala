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

package com.github.dnvriend.scalaz

import akka.Done
import akka.stream.scaladsl.Source
import com.github.dnvriend.TestSpec

import scala.concurrent.Future
import scalaz._
import Scalaz._

class StandardFlowTest extends TestSpec {
  //  /**
  //   * 1. Process single cmd elements eg: Source(() => request.iterator)
  //   * 2. Get the domain objects from persistency .mapAsync(1)(getDomainObjects)
  //   * 3. Validate using business rules .map(validate)
  //   * 4. Determine instructions to be executed .map(determineActions)
  //   * 5. Execute the actions .mapAsync(1)(executeActions)
  //   * 6. Collect the results and return .runFold(List.empty[ReturnType])(_ :+ _)
  //   */
  //  sealed trait AccountDomain
  //  final case class Account(id: Long, amount: Double) extends AccountDomain
  //
  //  object AccountRepository {
  //    def findById(id: Long): Future[Option[Account]] = id match {
  //      case 1            => Future.successful(Option(Account(1, 1000.00)))
  //      case 2            => Future.successful(Option(Account(2, 5000.00)))
  //      case 3            => Future.successful(Option(Account(3, 0.0)))
  //      case 4            => Future.successful(Option(Account(4, 10000.00)))
  //      case 5            => Future.successful(Option(Account(5, 500.00)))
  //      case _ if id <= 0 => Future.failed(new RuntimeException("Database error"))
  //      case _            => Future.successful(None)
  //    }
  //
  //    def update(id: Long, amount: Double): Future[Unit] =
  //      if (id <= 0) Future.failed(new RuntimeException("Database error"))
  //      else Future.successful(())
  //  }
  //
  //  sealed trait UpdateAccountServiceError
  //  case object AccountNotEnoughAmount extends UpdateAccountServiceError
  //  final case class AccountNotFound(id: Long) extends UpdateAccountServiceError
  //  final case class DatabaseError(error: String) extends UpdateAccountServiceError
  //
  //  case class GetAccountByIdResult(id: Long, account: Option[Account])
  //
  //  object UpdateAccountService {
  //    def updateAccount(id: Long, amount: Double): Future[Done] =
  //      Source.fromFuture(AccountRepository.findById(id))
  //        .map(account => GetAccountByIdResult(id, account))
  //        .map(validateAccount)
  //        .runForeach(_ => ())
  //
  //    def validateAccount(getAccountByIdResult: GetAccountByIdResult) = {
  //      validateAccountFound(getAccountByIdResult) *> (getAccountByIdResult)
  //
  //    }
  //
  //    def validateAccountFound: PartialFunction[GetAccountByIdResult, UpdateAccountServiceError \/ Account] = {
  //      case GetAccountByIdResult(_, None) => AccountNotFound(1L).left
  //    }
  //
  //    def validateAccountAmount: PartialFunction[GetAccountByIdResult, UpdateAccountServiceError \/ Account] = {
  //      case GetAccountByIdResult(_, Some(account)) if account.amount < 1000 => AccountNotEnoughAmount.left
  //      case GetAccountByIdResult(_, Some(account))                          => account.right
  //    }
  //  }
}
