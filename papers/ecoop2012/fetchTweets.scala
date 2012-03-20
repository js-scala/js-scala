def fetchTweets(username: Rep[String]) = (ajax.get {
  new JSLiteral {
    val url = 
"http://api.twitter.com/1/statuses/user_timeline.json"
    val `type` = "GET"
    val dataType = "jsonp"
    val data = new JSLiteral {
      val screen_name = user
      val include_rts = true
      val count = 5
      val include_entities = true
    }
  }
}).as[TwitterResponse]

type TwitterResponse = Array[JSLiteral {val text: String}]
