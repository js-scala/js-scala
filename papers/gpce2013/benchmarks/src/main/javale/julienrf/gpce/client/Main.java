package julienrf.gpce.client;

import com.google.gwt.core.client.EntryPoint;

public class Main implements EntryPoint {
    @Override
    public void onModuleLoad() {
        GwtBenchmark gwtBenchmark = new GwtBenchmark();
        gwtBenchmark.run();
    }
}
