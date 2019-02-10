package dddblueprint
package compiler

import io.scalaland.pulp.Provider
import org.specs2.specification.Scope

class ValidateTransitionSpec extends CompilerSpec {

  "ValidateTransition" should {

    "check if removed definitions are not used (all definitions exist)" in new Fixture {}

    "check if types matches between versions" in new Fixture {}

    "calculate required migrations for changed enum definitions" in new Fixture {}

    "calculate required migrations for changed record definitions" in new Fixture {}

    "calculate required migrations for changed service definitions" in new Fixture {}

    "calculate required migrations for changed publisher definitions" in new Fixture {}

    "calculate required migrations for changed subscriber definitions" in new Fixture {}
  }

  // TODO: tuple can only be used inside its own domain as argument

  // TODO: event can only be published by publisher from its own domain

  private trait Fixture extends Scope {
    val ValidateTransition = Provider.get[ValidateTransition[F]]
  }
}
