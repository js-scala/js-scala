val numTweets = 5
val appendTo = "#jstwitter"

for (user <- array("foo", "bar", "baz")) {
  log("fetching " + user)
  val data = ajax.get(
    new JSLiteral {
      val url = "http://api.twitter.com/1/statuses/user_timeline.json",
      val `type` = "GET",
      val dataType = "jsonp",
      val data = new JSLiteral {
        val screen_name = user,
        /* ... */
      }
    }
  )

  val user_div = "user_" + user
  $(appendTo).append("<div id='" + user_div + "'>")
  for (d <- data) {
    $("#" + user_div).append("<p>" + d.text + "</p>")
  }
}
