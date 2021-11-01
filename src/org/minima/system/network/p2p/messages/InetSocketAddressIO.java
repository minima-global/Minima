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
                peers.add(new InetSocketAddress((String) json.get("host"), Math.toIntExact((long) json.get("port"))));
            }
        }
        return peers;
    }
}
