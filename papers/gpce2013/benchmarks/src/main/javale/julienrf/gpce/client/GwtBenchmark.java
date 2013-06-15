package julienrf.gpce.client;

public class GwtBenchmark {

    static Option<Integer> maybe(Integer x) {
        return new Option.Some<Integer>(x + 1);
    }

    public static Option<Integer> gwt() {
        return maybe(0).bind(new Function<Integer, Option<Integer>>() {
            @Override
            public Option<Integer> apply(Integer a) {
                return maybe(a).bind(new Function<Integer, Option<Integer>>() {
                    @Override
                    public Option<Integer> apply(Integer b) {
                        return maybe(b).bind(new Function<Integer, Option<Integer>>() {
                            @Override
                            public Option<Integer> apply(Integer c) {
                                return maybe(c).bind(new Function<Integer, Option<Integer>>() {
                                    @Override
                                    public Option<Integer> apply(Integer d) {
                                        return new Option.Some<Integer>(d);
                                    }
                                });
                            }
                        });
                    }
                });
            }
        });
    }

    native public void run()/*-{
        var suite = new $wnd.Benchmark.Suite();
        suite.add(function () { return @julienrf.gpce.client.GwtBenchmark::gwt()() })
                .on('cycle', function (event) { $wnd.console.log(event.target.toString()) })
                .run();
    }-*/;

}
