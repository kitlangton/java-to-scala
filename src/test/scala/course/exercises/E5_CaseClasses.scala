package course.exercises

import zio.test.TestAspect.ignore
import zio.test._

//  ██████╗ █████╗ ███████╗███████╗     ██████╗██╗      █████╗ ███████╗███████╗███████╗███████╗
// ██╔════╝██╔══██╗██╔════╝██╔════╝    ██╔════╝██║     ██╔══██╗██╔════╝██╔════╝██╔════╝██╔════╝
// ██║     ███████║███████╗█████╗      ██║     ██║     ███████║███████╗███████╗█████╗  ███████╗
// ██║     ██╔══██║╚════██║██╔══╝      ██║     ██║     ██╔══██║╚════██║╚════██║██╔══╝  ╚════██║
// ╚██████╗██║  ██║███████║███████╗    ╚██████╗███████╗██║  ██║███████║███████║███████╗███████║
//  ╚═════╝╚═╝  ╚═╝╚══════╝╚══════╝     ╚═════╝╚══════╝╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚══════╝

object E5_CaseClasses extends Exercise {

  /** ✏ EXERCISE
    *
    * Create a Person case class to get free getters (fields) for all the
    * constructor parameters of the class.
    */
  val testFields =
    test("fields") {
      class Person(name: String, age: Int) // <- Convert this to a case class

      def getName(person: Person): String = ??? // <- Complete these
      def getAge(person: Person): Int     = ??? // <- Complete these

      val holmes = new Person("Sherlock Holmes", 42)

      assertTrue(
        getName(holmes) == "Sherlock Holmes",
        getAge(holmes) == 42
      )
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Create a Person case class with a name (String) and an age (Int), and
    * delete the fake apply constructor to observe the free constructor that all
    * case classes receive in their companion objects.
    */
  val testApply =
    test("apply") {
      class Person(name: String, age: Int) // <- Convert this to a case class

      object Person {
        def apply(name: String, age: Int) = new Person(name, age)
      }

      assertTrue(Person("Sherlock Holmes", 42) == Person("Sherlock Holmes", 42))
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Get a free implementation of equality for the `Profile` class by turning
    * it into a case class.
    */
  val testEquals = test("equals") {
    class Profile(val age: Int) // <- Convert this to a case class

    assertTrue(new Profile(42) == new Profile(42))
  } @@ ignore

  /** ✏ EXERCISE
    *
    * Get a free implementation of hash code for the `CreditCard` class by
    * turning it into a case class.
    */
  val testHashCode =
    test("hashCode") {
      class CreditCard(val number: String) // <- Convert this to a case class

      assertTrue(new CreditCard("123").hashCode == new CreditCard("123").hashCode)
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Get a free implementation of `toString` for the `Address` class by turning
    * it into a case class.
    */
  val testToString =
    test("toString") {
      class Address(val street: String) // <- Convert this to a case class

      assertTrue(new Address("221B Baker Street").toString == "Address(221B Baker Street)")
    } @@ ignore

  /** ✏ EXERCISE
    *
    * Get a free implementation of `copy` for the `Permissions` class by turning
    * it into a case class.
    */
  val testCopy =
    test("copy") {
      class Permissions(canRead: Boolean, canWrite: Boolean, canShare: Boolean) { // <- Convert this to a case class
        def copy(
            canRead: Boolean = this.canRead,
            canWrite: Boolean = this.canWrite,
            canShare: Boolean = this.canShare
        ): Permissions = ???
      }

      val perms = new Permissions(true, false, false)

      assertTrue(perms.copy(canRead = false) == new Permissions(false, false, false))
    } @@ ignore

  def exercise = suite("Case Classes")(
    testFields,
    testApply,
    testEquals,
    testHashCode,
    testToString,
    testCopy
  )

}
