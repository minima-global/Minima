package org.minima.system.network.p2p.event;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.minima.utils.json.JSONObject;

import java.util.HashMap;
import java.util.Map;

@RequiredArgsConstructor
@Getter
public class Event {
    private final String traceId;
    private final String category;
    private final String type;
    private final JSONObject details;
    private long time = System.currentTimeMillis();

    public JSONObject toJSONObject() {
        Map<String, Object> map = new HashMap<>();
        map.put("traceId", traceId);
        map.put("category", category);
        map.put("type", type);
        map.put("time", time);
        map.put("details", details);
        return new JSONObject(map);
    }
}
