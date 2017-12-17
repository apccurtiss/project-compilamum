function print(x) { console.log(x); }
function JSONParse(x) { JSON.parse(x); }
function popup(x) { alert(x); }

function get_backend_function(f) {
  return (nf) => {
    xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST","/call", true);
    xmlhttp.onreadystatechange=function(){
       if (xmlhttp.readyState == 4 && xmlhttp.status == 200){
         if (xmlhttp.responseText === "") {
             nf()
         } {
             nf(JSON.parse(xmlhttp.responseText));
         }
         
       }
    }
    data = {
      func: f,
      args: Array.prototype.slice.call(arguments, 1),
    }
    xmlhttp.send(JSON.stringify(data));
  }
}
var set = (id, str) => { document.getElementById(id).innerHTML = str }
function pageload() {
function pageload0(asdf) {
var x = asdf;
set("counter", x);
};
return get_backend_function('get_and_increment', )(pageload0);
}