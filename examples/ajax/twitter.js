user = 'gkossakowski'
numTweets = 5
appendTo = "#jstwitter"

function loadTweets() {
    $.ajax({
        url: 'http://api.twitter.com/1/statuses/user_timeline.json/',
        type: 'GET',
        dataType: 'jsonp',
        data: {
            screen_name: user,
            include_rts: true,
            count: numTweets,
            include_entities: true
        },
        success: function(data, textStatus, xhr) {
            for (var i=0; i < data.length; i++) {
                $(appendTo).append(
                    '<p>' + data[i].text + '</p>'
                )
            }
        }   
    })
}

$(document).ready(loadTweets)