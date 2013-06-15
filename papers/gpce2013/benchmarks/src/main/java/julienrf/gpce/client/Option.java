package julienrf.gpce.client;

public interface Option<A> {
    <B> Option<B> bind(Function<A, Option<B>> f);

    public static class Some<A> implements Option<A> {

        public final A value;

        public Some(A value) {
            this.value = value;
        }

        @Override
        public <B> Option<B> bind(Function<A, Option<B>> f) {
            return f.apply(value);
        }
    }

    public static class None<A> implements Option<A> {

        @Override
        public <B> Option<B> bind(Function<A, Option<B>> f) {
            return new None<B>();
        }
    }

}
