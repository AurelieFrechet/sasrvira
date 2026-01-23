
Person <- new_class(
  "Person",
  properties = list(
    name = S7::class_character,
    age  = S7::class_numeric
  ),
  constructor =
    function(name, age) {
      new_object(
        .parent = S7_object(),
        name = name,
        age  = age
      )
    }
)

introduce <- S7::new_generic("introduce", "x")

S7::method(introduce, Person) <- function(x) {
  talk <- paste("Hello my name is", x@name, "and I'm", x@age)
  return(talk)
}


Employee <- new_class(
  "Employee",
  parent = Person,
  properties = list(profession = S7::class_character)
)


S7::method(introduce, Employee) <- function(x) {
  talk <- paste("Hello my name is", x@name, ", I'm", x@age, "and I working as", x@profession)
  return(talk)
}


People <- list(Jacqueline = Person(name = "Jacqueline", age = 62),
               Michel = Employee(name = "Michel", age = 46, profession = "Pet Sitter"))

lapply(People, introduce)
