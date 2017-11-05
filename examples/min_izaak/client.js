var socket = new WebSocket("ws://localhost:8080", "marmalum");

to_send = []

socket.addEventListener('open', function (event) {
    for (var i = 0; i < to_send.length; to_send++) {
        socket.send(to_send[i]);
    }
});



socket.onmessage = function RESOLVE_RESPONSE(event) {
    console.log(event.data);
    var msg = JSON.parse(event.data);
    if (msg.caller == 'get_counter_1') {
        get_counter_2(msg.response);
    }
}



function MAKE_BACKEND_CALL(from, func_name, args) {
    obj = {
        caller: from,
        callee: func_name,
        arguments: args
    };
    
    if(socket.readyState !== socket.OPEN) {
        to_send.push((JSON.stringify(obj)));
    } else {
        socket.send(JSON.stringify(obj));
    }
}



function change_element(value) {
    document.getElementById("p1").innerHTML = value;
}

function get_counter() {
    get_counter_1();
}

function get_counter_1() {
    MAKE_BACKEND_CALL("get_counter_1", "handle_counter", []);
}

function get_counter_2(res) {
    x = res;
    change_element(x);
}
