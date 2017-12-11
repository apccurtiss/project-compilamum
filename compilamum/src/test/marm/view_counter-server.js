function print(x) { console.log(x); }
function JSONParse(x) { JSON.parse(x); }

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
        response.end(JSON.stringify(eval(data.func).apply(this, data.args)));
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
}).listen(80);
var counter = 0.0;
function get_and_increment() {
counter = (counter + 1.0);
print(counter);
return counter;
}