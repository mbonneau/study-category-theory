# Note to self
Be careful when testing Disjunctions with ScalaTest.
 
The verbs `.left` is already taken by Scalatest, but moreover, its 
better to use Disjunction.left("") and Disjunction.right("") to make it clear to 
new users what it all means instead of using the symbols -\/("") and \/-("").
