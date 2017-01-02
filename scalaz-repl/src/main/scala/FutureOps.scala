import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits._

object FutureOps {
  implicit class FutureImplicits[A](val that: Future[A]) extends AnyVal {
    def futureValue: A = Await.result(that, Duration.Inf)
  }
}