
interface Option<A> {
    public function bind<B>(f: A -> Option<B>): Option<B>;
}

class Some<A> implements Option<A> {
    private var v: A;
    public function new(v: A) {
        this.v = v;
    }
    public function bind<B>(f: A -> Option<B>): Option<B> {
        return f(v);
    }
}

class None<A> implements Option<A> {
    public function new() {}
    public function bind<B>(f: A -> Option<B>): Option<B> {
        return new None<B>();
    }
}

class Benchmark {

    static function maybe(x: Int) {
        return new Some(x + 1);
    }

    @:expose public static function haxe() {
        maybe(0).bind(function (a) {
            return maybe(a).bind(function (b) {
                return maybe(b).bind(function (c) {
                    return maybe(c).bind(function (d) {
                        return new Some(d);
                    });
                });
            });
        });
    }
}