package org.minima.system.network.p2p.event;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.minima.utils.json.JSONObject;

import java.util.HashMap;
import java.util.Map;

@RequiredArgsConstructor
@Getter
public class Event {
    private final String traceId1;
    private final String traceId2;
    private final String type;
    private long time = System.currentTimeMillis();


    public JSONObject toJSONObject() {
        Map<String, String> h = new HashMap();
        h.put("traceId1", traceId1);
        h.put("traceId2", traceId2);
        h.put("type", type);
        h.put("time", String.valueOf(time));

        return new JSONObject(h);
    }
}
