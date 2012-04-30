function fetchTweets(username, callback) {
  jQuery.ajax({
    url: "http://api.twitter.com/1/
      statuses/user_timeline.json/",
    type: "GET",
    dataType: "jsonp",
    data: {
      screen_name : username,
      include_rts : true,
      count : 5,
      include_entities : true
    },
    success: callback
  })
}
