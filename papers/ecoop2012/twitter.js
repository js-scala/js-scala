var processed = 0
var users = ["gkossakowski", "odersky",
  "adriaanm"]
users.forEach(function (user) {
  console.log("fetching " + user)
  fetchTweets(user, function(data) {
    console.log("finished fetching " + user)
    data.forEach(function (tweet) {
      console.log("fetched " + tweet.text)
    })
    processed += 1
    if (processed == users.length) {
      console.log("done")
    }
  })
})
