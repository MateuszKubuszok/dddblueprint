package dddblueprint
package compiler

trait syntax {

  implicit class InputArgumentOps(val argument: input.Argument) {

    def compile[F[_]: ArgumentCompiler]: F[output.Argument] =
      ArgumentCompiler[F].apply(argument)
  }

  implicit class InputDefinitionRefOps(val definitionRef: input.DefinitionRef) {

    def requireExists[F[_]: SnapshotOperations]: F[output.DefinitionRef] =
      SnapshotOperations[F].requireDefinitionExists(definitionRef)

    def requireNotExisted[F[_]: SnapshotOperations]: F[output.DefinitionRef] =
      SnapshotOperations[F].requireDefinitionNotExisted(definitionRef)

    def translate[F[_]: SnapshotOperations]: F[output.DefinitionRef] =
      SnapshotOperations[F].translateDefinitionRef(definitionRef)
  }

  implicit class InputMigrationOps(val migration: input.Migration) {

    def compile[F[_]: MigrationCompiler]: F[Unit] =
      MigrationCompiler[F].apply(migration)
  }

  implicit class OutputDefinitionRefOps(val definitionRef: output.DefinitionRef) {

    def toDomain[F[_]: SnapshotOperations]: F[output.DomainRef] =
      SnapshotOperations[F].definitionToDomain(definitionRef)

    def getDefinition[F[_]: SnapshotOperations]: F[Option[output.Data.Definition]] =
      SnapshotOperations[F].getDefinition(definitionRef)

    def hasDefinition[F[_]: SnapshotOperations]: F[Boolean] =
      SnapshotOperations[F].hasDefinition(definitionRef)

    def setDefinition[F[_]: SnapshotOperations](body: output.Data.Definition): F[Unit] =
      SnapshotOperations[F].setDefinition(definitionRef, body)

    def removeDefinition[F[_]: SnapshotOperations]: F[Unit] =
      SnapshotOperations[F].removeDefinition(definitionRef)

    def renameDefinition[F[_]: SnapshotOperations](newName: String): F[Unit] =
      SnapshotOperations[F].renameDefinition(definitionRef, newName)
  }

  implicit class OutputSnapshotOps(val snapshot: output.Snapshot) {

    def validateTransition[F[_]: ValidateTransition](newVersion: output.Snapshot): F[Unit] =
      ValidateTransition[F].apply(snapshot, newVersion)
  }
}

object syntax extends syntax // scalastyle:ignore object.name
