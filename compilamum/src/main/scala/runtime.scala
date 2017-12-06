package runtime

import ast._

trait RuntimeObject {
  def export: Option[(String, Typ)];
  def code: String;
}

object Print extends RuntimeObject {
  def export = Some(("print", FuncType(List(Any()), Void())));
  def code = "function print(x) { console.log(x); }\n";
}

object Alert extends RuntimeObject {
  def export = Some(("popup", FuncType(List(Str()), Void())));
  def code = "function popup(x) { alert(x); }\n";
}

object Serve extends RuntimeObject {
  def export = None
  def code = """
const http = require('http')
const url = require('url')
const fs = require('fs')
const path = require('path')
const baseDirectory = __dirname

http.createServer(function (request, response) {
  try {
    const requestUrl = url.parse(request.url)
    var fsPath = baseDirectory+path.normalize(requestUrl.pathname)
    if (fsPath.endsWith("/") || fsPath.endsWith("\\")) {
      fsPath += "index.html"
    }
    console.log(fsPath)

    var fileStream = fs.createReadStream(fsPath)
    fileStream.pipe(response)
    fileStream.on('open', function() {
      response.writeHead(200)
    })
    fileStream.on('error',function(e) {
      response.writeHead(404)
      response.end()
    })
  } catch (err) {
    response.writeHead(500)
    response.end()
    console.log(e.stack)
  }
}).listen(1234)
  """
}

object Runtime {
  def client: List[RuntimeObject] = List(Print, Alert);
  def server: List[RuntimeObject] = List(Print, Serve);
}
