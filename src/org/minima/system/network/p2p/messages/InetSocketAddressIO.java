package org.minima.system.network.p2p.messages;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.net.InetSocketAddress;
import java.util.ArrayList;

public class InetSocketAddressIO {

    public static JSONArray addressesListToJSON(ArrayList<InetSocketAddress> peers) {
        JSONArray array = new JSONArray();
        if (!peers.isEmpty()) {
            for (InetSocketAddress address : peers) {
                JSONObject object = new JSONObject();
                object.put("host", address.getAddress().getHostAddress());
                object.put("port", address.getPort());
                array.add(object);
            }
        }
        return array;
    }

    public static ArrayList<InetSocketAddress> addressesJSONToList(JSONArray jsonArray) {
        ArrayList<InetSocketAddress> peers = new ArrayList<>();
        if (!jsonArray.isEmpty()) {
            for (Object object : jsonArray) {
                JSONObject json = (JSONObject) object;
                peers.add(new InetSocketAddress((String) json.get("host"), safeReadInt(json, "port")));
            }
        }
        return peers;
    }

    public static int safeReadInt(JSONObject jsonObject, String key){
        int ret = 0;
        Object y = jsonObject.getOrDefault(key, 0);

        if (y instanceof Integer) {
            ret = (int) y;
        } else if (y instanceof Long){
            ret = Math.toIntExact((long) y);
        }
        return ret;
    }
}
