package org.minima.system.network.p2p.util;

import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

public class MapUtils {

    @NoArgsConstructor()
    public static class MapBuilder {
        private Map<String, Object> map = new HashMap<>();

        public static MapBuilder using(String key, Object value) {
            MapBuilder newMap = new MapBuilder();
            newMap.map.put(key, value);
            return newMap;
        }
        public MapBuilder and(String key, Object value) {
            map.put(key, value);
            return this;
        }

        public Map<String, Object> asMap() {
            return map;
        }
    }
}
