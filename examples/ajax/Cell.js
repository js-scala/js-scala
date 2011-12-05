function spawn(b) {
  console.log("spawning " + b)
  b()
}
function Cell() {
  this.value = undefined
  this.queue = []
  this.get = function (k) {
    if (this.value != undefined) {
      k(this.value)
    } else {
      this.queue.push(k)
    }
  }
  this.set = function (v) {
    if (this.value != undefined) {
      throw "can't set value twice"
    } else {
      this.value = v
      this.queue.forEach(function (f) {
        spawn(function () { f(v) })
      })
    }
  }
}
