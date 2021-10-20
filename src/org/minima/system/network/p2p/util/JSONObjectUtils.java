package org.minima.system.network.p2p.util;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.net.InetSocketAddress;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

public class JSONObjectUtils {
    public static JSONObject from(InetSocketAddress inetSocketAddress) {

        if (inetSocketAddress == null)
            return null;

        Map<String, Object> map = new HashMap<>();
        map.put("host", inetSocketAddress.getHostString());
        map.put("address", inetSocketAddress.getAddress().toString());
        map.put("port", inetSocketAddress.getPort());
        return new JSONObject(map);
    }

    public static JSONArray from(Collection<InetSocketAddress> inetSocketAddresses) {
        return new JSONArray(inetSocketAddresses.stream()
                .map(JSONObjectUtils::from)
                .collect(toList()));
    }
}
