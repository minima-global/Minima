package org.minima.system.network.p2p;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public interface Traceable {

    Traceable NEW_TRACEABLE = new Traceable() {
        @Override
        public String getTraceId() {
            return MiniData.getRandomData(8).toString();
        }

        @Override
        public JSONObject getContent() {
            return new JSONObject();
        }
    };

    String getTraceId();
    JSONObject getContent();
    default String getName() {
        return this.getClass().getSimpleName();
    }
}
