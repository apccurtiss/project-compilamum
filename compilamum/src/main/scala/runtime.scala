package runtime

import ast._

trait RuntimeObject {
    def frontend: Option[String]
    def backend: Option[String]
    def typ: Typ
}

object Print extends RuntimeObject {
    def frontend = Some("function print(x) { console.log(x); }\n")
    def backend = Some("function print(x) { console.log(x); }\n")
    def typ = FuncType(List(Any()), Void())
}

object Alert extends RuntimeObject {
    def frontend = Some("function popup(x) { alert(x); }\n")
    def backend = None
    def typ = FuncType(List(Str()), Void())
}

object Runtime{
    val objects = List(
        Print,
        Alert
    )
}
