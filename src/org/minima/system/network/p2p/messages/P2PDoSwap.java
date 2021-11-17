package org.minima.system.network.p2p.messages;

import java.net.InetSocketAddress;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;


public class P2PDoSwap {

    private MiniData secret = MiniData.getRandomData(8);
    private InetSocketAddress swapTarget;
    private String swappingClientUID;


    private static final String SECRET_JSON_KEY = "secret";
    private static final String HOST_JSON_KEY = "swap_target_host";
    private static final String PORT_JSON_KEY = "swap_target_port";

    public P2PDoSwap() {
        //Create and empty DoSwapMsg for reading from json
    }

    public P2PDoSwap(MiniData secret, InetSocketAddress swapTarget, String swappingClientUID) {
        if (swapTarget == null){
            throw new  NullPointerException("swapTarget is null");
        }
        this.secret = secret;
        this.swapTarget = swapTarget;
        this.swappingClientUID = swappingClientUID;
    }

    public static P2PDoSwap readFromJson(JSONObject json) {
        P2PDoSwap data = new P2PDoSwap();
        data.readJson(json);
        return data;

    }

    public String getSwappingClientUID() {
        return swappingClientUID;
    }

    public void setSwappingClientUID(String swappingClientUID) {
        this.swappingClientUID = swappingClientUID;
    }

    public JSONObject toJson() {
        JSONObject contents = new JSONObject();
        contents.put(SECRET_JSON_KEY, secret.toString());
        contents.put(HOST_JSON_KEY, swapTarget.getHostString());
        contents.put(PORT_JSON_KEY, swapTarget.getPort());
        JSONObject main = new JSONObject();
        main.put("do_swap", contents);
        return main;
    }

    public void readJson(JSONObject json) {
        JSONObject contents = (JSONObject) json.get("do_swap");
        if (contents.containsKey(SECRET_JSON_KEY)) {
            this.setSecret(new MiniData((String) contents.get(SECRET_JSON_KEY)));
        }
        if (contents.containsKey(HOST_JSON_KEY) && contents.containsKey(PORT_JSON_KEY)) {
            this.setSwapTarget(new InetSocketAddress((String) contents.get(HOST_JSON_KEY), InetSocketAddressIO.safeReadInt(contents, PORT_JSON_KEY)));
        }
    }

    public MiniData getSecret() {
        return secret;
    }

    public void setSecret(MiniData secret) {
        this.secret = secret;
    }

    public InetSocketAddress getSwapTarget() {
        return swapTarget;
    }

    public void setSwapTarget(InetSocketAddress swapTarget) {
        this.swapTarget = swapTarget;
    }
}
