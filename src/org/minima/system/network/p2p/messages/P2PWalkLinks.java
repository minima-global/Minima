package org.minima.system.network.p2p.messages;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;

import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;


public class P2PWalkLinks {

    public String getTargetUidForNextHop() {
        return targetUidForNextHop;
    }

    public void setTargetUidForNextHop(String targetUidForNextHop) {
        this.targetUidForNextHop = targetUidForNextHop;
    }

    String targetUidForNextHop;
    boolean walkInLinks;
    boolean isJoiningWalk;
    boolean isClientWalk;
    int availableNoneP2PConnectionSlots = 0;
    boolean isReturning = false;
    List<InetSocketAddress> pathTaken = new ArrayList<>();
    private MiniData secret = MiniData.getRandomData(8);


    public P2PWalkLinks() {
    }

    public P2PWalkLinks(boolean walkInLinks, boolean isJoiningWalk, String targetUid) {
        this.walkInLinks = walkInLinks;
        this.isJoiningWalk = isJoiningWalk;
        this.targetUidForNextHop = targetUid;
    }

    public P2PWalkLinks(boolean walkInLinks, boolean isReturning, List<InetSocketAddress> pathTaken) {
        this.walkInLinks = walkInLinks;
        this.isReturning = isReturning;
        this.pathTaken = pathTaken;
    }

    public static P2PWalkLinks readFromJSON(JSONObject json) {
        P2PWalkLinks msgWalkLinks = new P2PWalkLinks();
        msgWalkLinks.fromJson(json);
        return msgWalkLinks;
    }

    public void addHopToPath(InetSocketAddress address) {
        if (address == null){
            throw new NullPointerException("Address is null");
        }
        this.pathTaken.add(address);
    }

    public InetSocketAddress getPreviousNode(InetSocketAddress thisAddress) {
        InetSocketAddress previousNode = null;
        int index = this.pathTaken.indexOf(thisAddress);
        if (index > 0) {
            index--;
            previousNode = this.pathTaken.get(index);
        }
        return previousNode;
    }

    public JSONObject toJson(){
        JSONObject contents = new JSONObject();
        contents.put("secret", secret.toString());
        contents.put("walkInLinks", walkInLinks);
        contents.put("isJoiningWalk", isJoiningWalk);
        contents.put("isClientWalk", isClientWalk);
        contents.put("availableClientSlots", availableNoneP2PConnectionSlots);
        contents.put("isReturning", isReturning);
        contents.put("pathTaken", InetSocketAddressIO.addressesListToJSON(pathTaken));
        JSONObject main = new JSONObject();
        main.put("walk_links", contents);
        return main;
    }

    public void fromJson(JSONObject json) {
        JSONObject contents = (JSONObject) json.get("walk_links");
        if (contents.containsKey("secret")) {
            this.setSecret(new MiniData((String) contents.get("secret")));
        }
        if (contents.containsKey("walkInLinks")) {

            this.setWalkInLinks((boolean) contents.get("walkInLinks"));
        }
        if (contents.containsKey("isJoiningWalk")) {
            this.setJoiningWalk((boolean) contents.get("isJoiningWalk"));
        }
        if (contents.containsKey("isClientWalk")) {
            this.setClientWalk((boolean) contents.get("isClientWalk"));
        }
        if (contents.containsKey("availableClientSlots")) {
            this.setAvailableNoneP2PConnectionSlots(InetSocketAddressIO.safeReadInt(contents, "availableClientSlots"));
        }
        if (contents.containsKey("isReturning")) {
            this.setReturning((boolean) contents.get("isReturning"));
        }
        if (contents.containsKey("pathTaken")) {
            this.setPathTaken(InetSocketAddressIO.addressesJSONToList((JSONArray) contents.get("pathTaken")));
        }

    }

    public MiniData getSecret() {
        return secret;
    }

    public void setSecret(MiniData secret) {
        this.secret = secret;
    }

    public boolean isWalkInLinks() {
        return walkInLinks;
    }

    public void setWalkInLinks(boolean walkInLinks) {
        this.walkInLinks = walkInLinks;
    }

    public boolean isJoiningWalk() {
        return isJoiningWalk;
    }

    public void setJoiningWalk(boolean joiningWalk) {
        isJoiningWalk = joiningWalk;
    }

    public boolean isClientWalk() {
        return isClientWalk;
    }

    public void setClientWalk(boolean clientWalk) {
        isClientWalk = clientWalk;
    }

    public int getAvailableNoneP2PConnectionSlots() {
        return availableNoneP2PConnectionSlots;
    }

    public void setAvailableNoneP2PConnectionSlots(int availableNoneP2PConnectionSlots) {
        this.availableNoneP2PConnectionSlots = availableNoneP2PConnectionSlots;
    }

    public boolean isReturning() {
        return isReturning;
    }

    public void setReturning(boolean returning) {
        isReturning = returning;
    }

    public List<InetSocketAddress> getPathTaken() {
        return pathTaken;
    }

    public void setPathTaken(List<InetSocketAddress> pathTaken) {
        this.pathTaken = pathTaken;
    }
}
