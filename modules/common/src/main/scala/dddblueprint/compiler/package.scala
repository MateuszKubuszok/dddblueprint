package dddblueprint

import cats.data.ValidatedNel
import cats.implicits._

package object compiler {

  type Result[A] = ValidatedNel[SchemaError, A]
  object Result {
    def valid[A](a: A):           Result[A] = a.validNel[SchemaError]
    def error[A](e: SchemaError): Result[A] = e.invalidNel[A]
  }
}
