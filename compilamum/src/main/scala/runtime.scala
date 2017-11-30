package runtime

object AddFrontendRuntime{
  def apply(frontend: String): String = {
    val print = "function print(s) { console.log(s); }"
    List(print, frontend) mkString("\n")
  }
}

object AddBackendRuntime{
  def apply(backend: String): String = {
    backend
  }
}
