
var disabled = false;
var wslocation = document.location.toString().replace('http://','ws://').replace('https://','wss://');
var socket = new WebSocket(wslocation);
var timeouts = {};
socket.onmessage=function(m){
    data = JSON.parse(m.data);
    if(data['action'] == 'close'){
	$('#mouse_'+data['id']).remove();
    } else if(data['action'] == 'speak') {
	if(data['id']) {
	    speak(data);
	} else {
	    preview(data);
	}
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
    if(disabled == false){                               
	if($('#mouse_'+mouse['id']).length == 0) {
	    $('body').append('<span class="mouse" id="mouse_'+mouse['id']+'"><span style="display:none;" class="chat"/></span>');
	}
	$('#mouse_'+mouse['id']).css({
	    'left' : (($(window).width() - mouse['w']) / 2 + mouse['x']) + 'px',
	    'top' : mouse['y'] + 'px'
	})
    }
};

function speak(data){
    clearTimeout(timeouts[data['id']]);
    $('#mouse_'+data['id']+' img').remove();
    $('#mouse_'+data['id']).append('<img src="http://www.gravatar.com/avatar/' + data['email'] + '?s=20" />');

    if(data['text'] == '') {
	return $('#mouse_'+data['id']+' .chat').hide();
    }

    $('#mouse_'+data['id']+' .chat').show().html(data['text']);
    timeouts[data['id']] = setTimeout("$('#mouse_"+data['id']+" .chat').hide()", 5000)
};

function preview(data){
    clearTimeout(timeouts[data['preview']]);
    $('#preview img').remove();
    $('#preview').append('<img src="http://www.gravatar.com/avatar/' + data['email'] + '?s=20" />');

    if(data['text'] == '') {
	return $('#preview .chat').hide();
    }

    $('#preview').show();
    $('#preview .chat').show().html(data['text']);
    timeouts['preview'] = setTimeout("$('#preview').hide()", 5000)
};

$(document).ready(function(){
    $('#mouse_toggle a').toggle(function(){
	$('.mouse').hide();
	disabled = true;
	$(this).html('enable');
    }, function(){
	$('.mouse').show();
	disabled = false;
	$(this).html('disable');
    });

    $('form#chat input#email').focus();
    $('form#chat').submit(function(){
	if($('form#chat input#email').val() == '') {
	    return alert('You forgot to fill in your e-mail address.');
	}

	socket.send(JSON.stringify({
	    action: 'speak',
	    email: $('form#chat input#email').val(),
	    text: $('form#chat input#text').val().substring(0, 140)
	}));

	email: $('form#chat input#text').val('')
	return false;
    })

    $('body').append('<span id="preview"><span style="display:none;" class="chat"/></span>');
});

$(document).mousemove(
    ratelimit(function(e){
	socket.send(JSON.stringify({
	    action: 'move',
	    x: e.pageX,
	    y: e.pageY,
	    w: $(window).width(),
	    h: $(window).height()
	}))

	$('#preview').css({
	    'left' : e.pageX + 'px',
	    'top' : e.pageY + 'px'
	})
    }, 40)
);
