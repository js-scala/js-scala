(function () { "use strict";
var Option = function() { }
var Some = function(v) {
	this.v = v;
};
Some.__interfaces__ = [Option];
Some.prototype = {
	bind: function(f) {
		return f(this.v);
	}
}
var None = function() {
};
None.__interfaces__ = [Option];
None.prototype = {
	bind: function(f) {
		return new None();
	}
}
var Benchmark = function() { }
Benchmark.maybe = function(x) {
	return new Some(x + 1);
}
Benchmark.haxe = function() {
	Benchmark.maybe(0).bind(function(a) {
		return Benchmark.maybe(a).bind(function(b) {
			return Benchmark.maybe(b).bind(function(c) {
				return Benchmark.maybe(c).bind(function(d) {
					return new Some(d);
				});
			});
		});
	});
}
$hxExpose(Benchmark.haxe, "Benchmark.haxe");
function $hxExpose(src, path) {
	var o = typeof window != "undefined" ? window : exports;
	var parts = path.split(".");
	for(var ii = 0; ii < parts.length-1; ++ii) {
		var p = parts[ii];
		if(typeof o[p] == "undefined") o[p] = {};
		o = o[p];
	}
	o[parts[parts.length-1]] = src;
}
})();
