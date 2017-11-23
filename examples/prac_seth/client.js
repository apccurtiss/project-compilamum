// boilerplate (Lock away in a seperate file possibly hosted elsewhere
var socket = new WebSocket("ws://localhost:8080", "marmalum");

to_send = []
keyNum = 1;
responces = {}

socket.addEventListener('open', function (event) {
    for (var i = 0; i < to_send.length; i++) {
        socket.send(to_send[i]);
    }
});

socket.onmessage = function RESOLVE_RESPONSE(event) {
    // message is a {key:...,data:...}
    // responce object is a {key:{callback:...,keep:#},...}
    console.log(event.data);
    var msg = JSON.parse(event.data);
    if(msg.key in responces){
      var obj = responces[msg.key];
      var call = obj.callback;
      if(obj.keep <= 1){
        delete responces[msg.key];
      } else {
        obj.keep -= 1;
      }
      call(msg.data);
    }
}



function MAKE_BACKEND_CALL(callback,numTimes,backendName,args) {
    var resp = {
        callback: callback,
        keep: numTimes,
    };
    // possible race condition
    var key = keyNum;
    keyNum +=1;
    
    responces[key] = resp;
    
    var obj = {
      "name":backendName,
      "args":args,
      "key":key,
    };
    var req = JSON.stringify(obj)
    if(socket.readyState !== socket.OPEN) {
        to_send.push(req);
    } else {
        socket.send(req);
    }
}

// Generated code:
function change_element(value) {
    document.getElementById("p1").innerHTML = value;
}

function get_counter() {
    MAKE_BACKEND_CALL(get_counter_2, 1,"get_counter", []);
}

function get_counter_2(res) {
    x = res;
    change_element(x);
}
