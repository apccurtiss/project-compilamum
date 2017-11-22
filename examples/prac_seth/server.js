// Boilerplate here. 
wss.on('connection', function(ws) {
    ws.on('message', function(message) {
        var message = JSON.parse(message);
        var args = message.args;
        var name = message.name;
        var value = functions[name](args);
        var req = JSON.stringify({"Key":message.key,"data":message.data});
        ws.send(req);
    });
});

// init all the globals here.
var i = 0;

// Init all the functions in this
var functions = {
  "handle_counter":function (){
    i = i+1;
    return i;
  }
};


