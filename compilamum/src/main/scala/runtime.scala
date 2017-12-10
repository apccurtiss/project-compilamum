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

object JSONParse extends RuntimeObject {
  def export = Some(("JSONParse", FuncType(List(Str()), Void())));
  def code = "function JSONParse(x) { JSON.parse(x); }\n";
}

object GetBackendFunction extends RuntimeObject {
  def export = Some(("get_backend_function"), FuncType(List(Str()), Void()));
  def code = """
function get_backend_function(f) {
  return () => {
    xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST","/call", true);
    xmlhttp.onreadystatechange=function(){
       if (xmlhttp.readyState == 4 && xmlhttp.status == 200){
         console.log(xmlhttp.responseText);
       }
    }
    data = {
      func: f,
      args: Array.prototype.slice.call(arguments, 1),
    }
    xmlhttp.send(JSON.stringify(data));
  }
}
""";
}

object Serve extends RuntimeObject {
  def export = None
  def code = """
require('http').createServer(function (request, response) {
  const url = require('url')
  const fs = require('fs')
  const path = require('path')
  const baseDirectory = __dirname

  try {
    const requestUrl = url.parse(request.url)

    if (request.method == "POST" && requestUrl.pathname == "/call") {
      var data = '';
      request.on('data', function(chunk) {
        data += chunk;
      });
      request.on('end', function() {
        data = JSON.parse(data);
        response.writeHead(200, { 'Content-Type': 'application/json' });
        response.end(JSON.stringify(funcs[data.func].apply(this, data.args)));
      });
      return;
    }

    var fsPath = baseDirectory+path.normalize(requestUrl.pathname)
    if (fsPath.endsWith("/") || fsPath.endsWith("\\")) {
      fsPath += "index.html"
    }

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
}).listen(1234);
""";
}

object Runtime {
  def client: List[RuntimeObject] = List(Print, JSONParse, Alert, GetBackendFunction);
  def server: List[RuntimeObject] = List(Print, JSONParse, Serve);
}
