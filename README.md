# DDD Blueprint

> This is for fun project + experimental sandbox, don't use it on production!

This project is a small experiment that test some idea:

 * what if we had some migration schema of our domain
 * what if it worked kind of like Flyway for domain models
 * what if we used it as source of truth to generate our domain models,
   classes, migrations, and whatever we want once we parse it to confirm
   that migration is valid

This project is a PoC of such idea.

## Example domain definition

`tests/src/test/resources/correct-blueprint` stores an example of a correct domain

You can use it as input by running with sbt

```
tests/test:run src/test/resources/correct-blueprint
```

## Progress of PoC

- [x] parsing of input
- [x] validation of input models
- [x] emission of output model
- [x] generation of happy-path entities
- [x] generation of happy-path services models
- [ ] generation of automatic migrations between models where possible

## Contribution

This is

1. PoC
2. the worst FP code I have ever written

so I suggest grabbing a beer instead.
