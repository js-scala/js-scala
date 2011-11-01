var connections = 0; // count active connections
self.addEventListener("connect", function (e) {
    var port = e.ports[0];
    connections++;
    port.addEventListener("message", function (e) {
	port.postMessage("Hello " + e.data + " (port #" + connections + ")");
    }, false);
    port.start();
}, false);
