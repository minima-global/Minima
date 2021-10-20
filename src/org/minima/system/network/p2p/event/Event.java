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
    private final JSONObject details;
    private long time = System.currentTimeMillis();

    public JSONObject toJSONObject() {
        Map<String, Object> map = new HashMap<>();
        map.put("traceId1", traceId1);
        map.put("traceId2", traceId2);
        map.put("type", type);
        map.put("time", time);
        map.put("details", details);
        return new JSONObject(map);
    }
}
