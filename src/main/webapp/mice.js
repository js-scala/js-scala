var socket = new WebSocket(document.location.toString().replace('http://','ws://').replace('https://','wss://'));
socket.onmessage=function(m){
    data = JSON.parse(m.data);
    if(data['action'] == 'close'){
	$('#mouse_'+data['id']).remove();
    } else if(data['action'] == 'move'){
	move(data);
    };
}

function ratelimit(fn, ms) {
    var last = (new Date()).getTime();
    return (function() {
	var now = (new Date()).getTime();
	if (now - last > ms) {
	    last = now;
	    fn.apply(null, arguments);
	}
    });
}

function move(mouse){
    if($('#mouse_'+mouse['id']).length == 0) {
	$('body').append('<span class="mouse" id="mouse_'+mouse['id']+'"><span style="display:none;" class="chat"/></span>');
    }
    $('#mouse_'+mouse['id']).css({
	'left' : (($(window).width() - mouse['w']) / 2 + mouse['cx']) + 'px',
	'top' : mouse['cy'] + 'px',
        'background-color' : mouse['color']
    })
};

$(document).mousemove(
    ratelimit(function(e){
	socket.send(JSON.stringify({
	    action: 'move',
	    cx: e.pageX,
	    cy: e.pageY,
	    w: $(window).width(),
	    h: $(window).height()
	}))
    }, 40)
);
