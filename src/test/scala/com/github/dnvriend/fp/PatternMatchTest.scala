package com.github.dnvriend.fp

import com.github.dnvriend.TestSpec

class PatternMatchTest extends TestSpec {

  // Scala has a built-in general pattern matching mechanism. It allows to match
  // on any sort of data with a first-match policy.
  // most importantly, the match 'expression' returns a value, in this case it is
  // a method from (Int => String) and thus returns a String
  def matchNumber(x: Int): String = x match {
    case 1 => "one"
    case 2 => "two"
    case _ => "many"
  }

  // this is a method from (Any => Any),
  // so it accepts Any, and returns Any
  def matchAny(x: Any): Any = x match {
    case 1 => "one"
    case "two" => 2
    case y: Int => "scala.Int"
    case couldBeAnyting => couldBeAnyting
  }

  // it is also possible to set guards
  // on the matches
  def bigger(x: Any): Any = x match {
    case i: Int if i < 0 => i - 1
    case i: Int => i + 1
    case d: Double if d < 0.0 => d - 0.1
    case d: Double => d + 0.1
    case text: String => text + "s"
  }

  "matchNumber" should "match numbers" in {
    matchNumber(1) shouldBe "one"
    matchNumber(2) shouldBe "two"
    matchNumber(3) shouldBe "many"
  }

  "matchAny" should "match literally anything" in {
    matchAny(1) shouldBe "one"
    matchAny("two") shouldBe 2
    matchAny(2) shouldBe "scala.Int"
    matchAny(List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

  "bigger" should "match with guard" in {
    bigger(-1) shouldBe -2
    bigger(1) shouldBe 2
    bigger(-1.0) shouldBe -1.1
    bigger(1.0) shouldBe 1.1
    bigger("text") shouldBe "texts"
  }

    /**
     * But how does it all work?
     *
     * see: http://danielwestheide.com/blog/2012/11/21/the-neophytes-guide-to-scala-part-1-extractors.html
     *
     * The 'pattern matching mechanism' we talked about above (which is really cool by the way)
     * actually `decomposes/deconstruct` a given data structure, binding the values it was
     * constructed with to variables.
     *
     * You cannot decompose any kind of data structure, only data structures for where there exists an
     * `extractor`. An extractor has the opposite role of a constructor; it extracts the parameters from which
     * an object passed to it was created. The Scala library contains some predefined extractors.
     *
     * In scala you can construct an instant of a class with the Builder pattern that uses the companion object
     * that contains the .apply() method to construct that object. For example:
     */

    object Foo {
      def apply(message: String) = new Foo(message)
    }

    class Foo(val message: String)

  "Foo" should "be created using the builder pattern" in {
    Foo("Hello World!").message shouldBe "Hello World!"
  }

  /**
   * An `extractor` is a method called .unapply(). It has the opposite role of a constructor.
   * While the latter creates an object from a given list of parameters, an extractor extracts the
   * parameters from which an object passed to it was created. The signature of an unapply method is:
   *
   *  def unapply(object: S): Option[T]
   *
   * The method expects some object of type S and returns an Option of type T, which is the type of
   * the parameter it extracts. So the method returns either Some[T] (if it could successfully extract
   * the parameter from the given object) or None, which means that the parameters could not be extracted,
   * as per the rules determined by the extractor implementation.
   *
   * Just so you know, the compiler will create valid constructor and extractor methods for
   * case classes, but you can also write your own.
   *
   * Let's create our first extractor:
   */

  trait User {
    def name: String
  }
  class FreeUser(val name: String) extends User
  class PremiumUser(val name: String) extends User

  object FreeUser {
    def apply(name: String): FreeUser = new FreeUser(name) // the constructor
    def unapply(user: FreeUser): Option[String] = Some(user.name) // the extractor
  }
  object PremiumUser {
    def apply(name: String): PremiumUser = new PremiumUser(name) // the constructor
    def unapply(user: PremiumUser): Option[String] = Some(user.name) // the extractor
  }

  /**
   * The companion objects of FreeUser and PremiumUser now contain both the
   * constructor .apply() and extractor .unapply() methods. We can use them directly
   * eg:
   */

  "constructor and extractor" should "be used directly" in {
    FreeUser.unapply(FreeUser("John Doe")) shouldBe Some("John Doe")
    PremiumUser.unapply(PremiumUser("John McClane")) shouldBe Some("John McClane")
  }

  /**
   * But you wouldn’t usually call this method directly. Scala calls an extractor’s .unapply()
   * method if the extractor is used as an `extractor pattern`.
   *
   * If the result of calling unapply is Some[T], this means that the pattern matches,
   * and the extracted value is bound to the variable declared in the pattern. If it is None,
   * this means that the pattern doesn’t match and the next case statement is tested.
   *
   * Let’s use our extractors for pattern matching:
   */

  "Extractors" should "be used for pattern matching" in {
    def identifyUser(user: User): String = user match {
      case FreeUser(name) => s"FreeUser: $name"
      case PremiumUser(name) => s"PremiumUser: $name"
    }
    val freeUser = FreeUser("John Doe")
    val premiumUser = PremiumUser("John McClane")
    identifyUser(freeUser) shouldBe "FreeUser: John Doe"
    identifyUser(premiumUser) shouldBe "PremiumUser: John McClane"
  }

  /**
   * We can also extract multiple fields. The extractor method has the following signature:
   *
   *  def unapply(object: S): Option[(T1, ..., Tn)]
   *
   * The method expects some object of type S and returns an Option of type TupleN, where N
   * is the number of parameters to extract.
   */

  trait ScoreUser {
    def name: String
    def score: Int
  }
  class ScoreFreeUser(val name: String, val score: Int, val upgradeProbability: Double) extends ScoreUser
  class ScorePremiumUser(val name: String, val score: Int) extends ScoreUser

  object ScoreFreeUser {
    def apply(name: String, score: Int, upgradeProbability: Double) = new ScoreFreeUser(name, score, upgradeProbability)
    def unapply(user: ScoreFreeUser): Option[(String, Int, Double)] =
      Some((user.name, user.score, user.upgradeProbability))
  }
  object ScorePremiumUser {
    def apply(name: String, score: Int) = new ScorePremiumUser(name, score)
    def unapply(user: ScorePremiumUser): Option[(String, Int)] = Some((user.name, user.score))
  }

  "New extractors" should "be used in pattern matching" in {
    def identifyUser(user: ScoreUser): String = user match {
      case ScoreFreeUser(name, _, p) =>
        if (p > 0.75) name + ", what can we do for you today?" else "Hello " + name
      case ScorePremiumUser(name, _) => "Welcome back, dear " + name
    }
    val user = ScoreFreeUser("Daniel", 3000, 0.7d)
    identifyUser(user) shouldBe "Hello Daniel"
  }

  /**
   * When you don't need to extract parameters, but only want to check a data structure against
   * which you want to match you can just do a simple Boolean check. The signature is:
   *
   *  def unapply(object: S): Boolean
   *
   * Used in a pattern, the pattern will match if the extractor returns true. Otherwise the next case,
   * if available, is tried.
   *
   * Note: Boolean extractors are not used that often.
   */

  /**
   * As you can see here, it is not necessary for an extractor to reside in the companion object
   * of the class for which it is applicable. In a match pattern you should refer to the appropriate
   * object however
   */
  object PremiumCandidate {
    def unapply(user: ScoreFreeUser): Boolean = user.upgradeProbability > 0.75
  }

  "Boolean extractor" should "be used in pattern match" in {
    def determineMailProgram(user: ScoreUser): String = user match {
        // note the use of empty parenths (empty parameter list) because the
        // extractor doesn't extract any parameters to be bound to variables
      case freeUser @ PremiumCandidate() => s"SpamProgram: ${freeUser.name}"
      case _ => s"RegularNewsLetter: ${user.name}"
    }
    val user = ScoreFreeUser("Daniel", 2500, 0.8d)
    determineMailProgram(user) shouldBe "SpamProgram: Daniel"
  }







}
