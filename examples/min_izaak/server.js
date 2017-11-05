var WebSocketServer = require('ws').Server;

wss = new WebSocketServer({port: 8080});

wss.on('connection', function(ws) {
    ws.on('message', function(message) {
        var message = JSON.parse(message);
        
        if (message.callee === "handle_counter") {
            r = handle_counter();
            obj = {
                caller: message.caller,
                response: r
            };
            ws.send(JSON.stringify(obj));
        }
    });
});


var i = 0;

function handle_counter() {
    i = i + 1;
    return i;
}
