package dddblueprint
package parser

import input._
import io.scalaland.pulp.Provider
import org.specs2.specification.Scope

class CorrectParsingSpec extends ParserSpec {

  "Parser" should {

    "create correct migration out of resource files" in new Fixture {

      val expected = {
        val usersDomainRef        = DomainRef("users")
        val sexRef                = DefinitionRef(usersDomainRef, "Sex")
        val userRef               = DefinitionRef(usersDomainRef, "User")
        val credentialRef         = DefinitionRef(usersDomainRef, "Credential")
        val userCreatedRef        = DefinitionRef(usersDomainRef, "UserCreated")
        val createUserRef         = DefinitionRef(usersDomainRef, "createUser")
        val notificationDomainRef = DomainRef("notification")
        val onUserCreatedRef      = DefinitionRef(notificationDomainRef, "onUserCreated")
        val mailSentRef           = DefinitionRef(notificationDomainRef, "MailSent")
        val onMailSentRef         = DefinitionRef(notificationDomainRef, "onMailSent")
        History(
          List(
            // v1__create-domains.ddd
            Migration(
              List(
                Action.CreateDefinition(Data.Definition.Enum(sexRef, ListSet("male", "female"), Data.String)),
                Action.CreateDefinition(
                  Data.Definition.Record.Entity(userRef,
                                                ListMap(
                                                  "name" -> Data.String,
                                                  "surname" -> Data.String,
                                                  "sex" -> sexRef
                                                ))
                ),
                Action.CreateDefinition(
                  Data.Definition.Record.Value(credentialRef,
                                               ListMap(
                                                 "user" -> Data.Collection.Option(userRef),
                                                 "email" -> Data.String,
                                                 "passwordHash" -> Data.String
                                               ))
                ),
                Action.CreateDefinition(
                  Data.Definition.Record.Event(userCreatedRef,
                                               ListMap(
                                                 "user" -> userRef,
                                                 "emails" -> Data.Collection.Array(Data.String)
                                               ))
                ),
                Action.CreateDefinition(
                  Data.Definition.Service(createUserRef,
                                          ListMap(
                                            "user" -> userRef,
                                            "credentials" -> Data.Collection.Array(credentialRef)
                                          ),
                                          ListSet(userCreatedRef))
                ),
                Action.CreateDefinition(Data.Definition.Subscriber(onUserCreatedRef, ListSet(userCreatedRef))),
                Action.CreateDefinition(Data.Definition.Record.Event(mailSentRef, ListMap("email" -> Data.String))),
                Action.CreateDefinition(Data.Definition.Publisher(onMailSentRef, ListSet(mailSentRef)))
              )
            ),
            // v2__update-domains.ddd
            Migration(
              List(
                Action.AddEnumValues(sexRef, ListSet("undefined")),
                Action.RenameEnumValues(sexRef, ListMap("male" -> "M", "female" -> "F")),
                Action.RemoveRecordFields(userRef, ListSet("surname")),
                Action.RenameRecordFields(credentialRef, ListMap("passwordHash" -> "hash")),
                Action.RenameDefinition(createUserRef, "newUser"),
                Action.RemoveDefinition(onMailSentRef),
                Action.AddRecordFields(mailSentRef, ListMap("when" -> Data.String))
              )
            )
          )
        )
      }

      new TestParsing(DirectoryParser("correct-blueprint")) {
        history === expected
      }
    }
  }

  private trait Fixture extends Scope {
    val DirectoryParser = Provider.get[DirectoryParser[IO]]
  }
}
