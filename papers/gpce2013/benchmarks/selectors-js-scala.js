function selectorsJsScala() {
var x16 = document.getElementById("add-user");
var x24 = null;
if (x16 !== null) {
var x17 = x16;
var x18 = x17.getElementsByTagName("fieldset");
var x22 = [];
for (var x26 = 0, x27 = x18.length ; x26 < x27 ; x26++) {
var x19 = x18.item(x26);
var x20 = x19.getElementsByClassName("word");
x22.push(x20);
}
x24 = x22;
}
return x24
}
