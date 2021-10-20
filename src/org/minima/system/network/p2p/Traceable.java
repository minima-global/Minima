package org.minima.system.network.p2p;

import org.minima.utils.json.JSONObject;

public interface Traceable {
    String getTraceId();
    JSONObject getContent();
    default String getName() {
        return this.getClass().getSimpleName();
    }
}
