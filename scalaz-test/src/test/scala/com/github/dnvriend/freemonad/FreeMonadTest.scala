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

package com.github.dnvriend.freemonad

import com.github.dnvriend.TestSpec

import scalaz._
import Scalaz._

class FreeMonadTest extends TestSpec {

  /**
   * // see: http://underscore.io/blog/posts/2015/04/14/free-monads-are-simple.html
   * ==========================================================
   * The Free-monad is all about the 'Interpreter Pattern'
   * ==========================================================
   * The free monad brings together two concepts:
   *
   * 1. monads,
   * 2. interpreters
   *
   * This allows the creation of composable monadic interpreters.
   *
   * The free monad allows simple solutions to difficult problems.
   *
   * ==========================================================
   * 1. Monads
   * ==========================================================
   * A monad is defined by two operators, 'point' and 'flatMap'.
   *
   * Point is not very interesting, it just wraps a monad around a value.
   * - point[M[_], A](a: A): M[A]
   *
   * FlatMap is more interesting; it is the distinguishing feature of a monad and
   * it tells us something very important: monads are fundamentally about control flow.
   *
   * - flatMap[M[_], A, B](fa: M[A])(f: A => M[B]): M[B]
   *
   * The signature of flatMap says you combine a M[A] and a function A => M[B] to create a M[B].
   * The only way to do this is to get the A out of the M[A] and apply it to the A => M[B] function.
   *
   * There is a clear ordering of operations here, and repeated applications of flatMap creates a sequence
   * of operations that must execute from left to right. So we see that monads explicitly encode control flow.
   *
   * We usually use monads to glue together pure functions with special purpose control-flow,
   * such as fail fast error handling using Scalaz Disjunction or asynchronous computation using scala.concurrent.Future.
   *
   * The free monad allows us to abstractly specify control flow between pure functions,
   * and separately define an implementation. This last part, "separately define an implementation" is what
   * is interesting about the Free-monad; its about the Interpreter.
   *
   * ==========================================================
   * 2. Interpreters
   * ==========================================================
   * Ok, so that’s monads: control flow. What about interpreters.
   *
   * Interpreters are about separating the representation of a computation from the way it is run.
   *
   * Any interpreter has two parts:
   *
   * 1. An Abstract Syntax Tree (AST) that represents the computation,
   * 2. A process that gives meaning to the AST; the bit that actually runs it.
   *
   * A simple example is in order. Consider the expression 1 + 2 + 3.
   *
   * We can execute this directly, evaluating to 6, or we could represent it as an AST such as
   * Add(1, Add(2, 3)). Given the AST we could choose from many different ways to interpret it:
   *
   * - We could represent results using Ints, Doubles, or arbitrary precision numbers.
   * - We could perform our calculations using dual numbers, calculating the derivative at the same time
   * (very useful for machine learning applications).
   * - We could transform our calculation to run on the processor’s vector unit, or on a GPU!
   *
   * Hopefully this gives you a feel for the structure and power of the interpreter pattern.
   *
   * ==========================================================
   * Free-monads
   * ==========================================================
   * We have talked about monads and interpreters. I said the free monad is just the combination of the two.
   * Concretely this means the free monad provides:
   *
   * 1. An AST to express monadic operations;
   * 2. An API to write interpreters that give meaning to this AST.
   *
   * What does the AST look like? It simply represents the monad operations without giving meaning to them.
   *
   * The usual representation of the free monad represents the monadic operations in terms of
   * 'point' along with 'join', instead of the more familiar 'flatMap', but the idea is still the same.
   *
   * Now what does a free monad interpreter look like? It’s just a function from F[_],
   * the representation inside the free monad, to G[_],
   * which is some monad in which we really run the computation (the Id monad is a popular choice).
   *
   * This type of function has a special name, a natural transformation
   *
   * ==========================================================
   * Example: Service Orchestration
   * ==========================================================
   * Here’s a simple example of service orchestration. We start with some imports and other basic definitions.
   */

  type UserId = Int
  type UserName = String
  type UserPhoto = String

  final case class Tweet(userId: UserId, msg: String)
  final case class User(id: UserId, name: UserName, photo: UserPhoto)

  // Services represent web services we can call to fetch data
  sealed trait Service[A]
  final case class GetTweets(userId: UserId) extends Service[List[Tweet]]
  final case class GetUserName(userId: UserId) extends Service[UserName]
  final case class GetUserPhoto(userId: UserId) extends Service[UserPhoto]

  // A request represents a request for data
  final case class Request[A](service: Service[A])

  def fetch[A](service: Service[A]): Free[Request, A] =
    Free.liftF[Request, A](Request(service): Request[A])

  // getting a user
  def getUser(id: UserId): Free[Request, User] = for {
    name <- fetch(GetUserName(id))
    photo <- fetch(GetUserPhoto(id))
  } yield User(id, name, photo)

  // the process i.e. control flow
  def theProcess(theId: UserId): Free[Request, List[(String, User)]] = for {
    tweets <- fetch(GetTweets(theId))
    result <- (tweets map { tweet: Tweet =>
      for {
        user <- getUser(tweet.userId)
      } yield (tweet.msg -> user)
    }).sequenceU
  } yield result

  object PrintLineInterpreter extends (Request ~> Id.Id) {
    import Id._

    def apply[A](in: Request[A]): Id[A] = in match {
      case Request(service) =>
        service match {
          case GetTweets(userId) =>
            println(s"Getting tweets for user $userId")
            List(Tweet(1, "Hi"), Tweet(2, "Hi"), Tweet(1, "Bye"))

          case GetUserName(userId) =>
            println(s"Getting user name for user $userId")
            userId match {
              case 1 => "Agnes"
              case 2 => "Brian"
              case _ => "Anonymous"
            }

          case GetUserPhoto(userId) =>
            println(s"Getting user photo for user $userId")
            userId match {
              case 1 => ":-)"
              case 2 => ":-D"
              case _ => ":-|"
            }
        }
    }
  }

  def run(theId: UserId): List[(String, User)] =
    theProcess(theId).foldMap(PrintLineInterpreter)

  it should "run the example" in {
    run(1)
  }
}

