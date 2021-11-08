package org.minima.system.network.p2p;

import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;

import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.network.p2p.params.P2PParams;
import org.minima.utils.JsonDB;
import org.minima.utils.json.JSONArray;

public class P2PDB extends JsonDB {

    public P2PDB() {
        super();
    }

	/**
	 * Loads the peers list from the DB
	 * @return A list of peers or an empty list if there is no data to load
	 */
    public List<InetSocketAddress> getPeersList() {
        ArrayList<InetSocketAddress> peers = new ArrayList<>();
        JSONArray jsonArray = (JSONArray) getAllData().getOrDefault("peers", new JSONArray());
		if (!jsonArray.isEmpty()){
            peers.addAll(InetSocketAddressIO.addressesJSONToList(jsonArray));
		}
        return peers;
    }

	/**
	 * Stores a list of InetSocketAddress peers as json array of strings in the db
	 * @param peers ArrayList of discovery peers host:minimaPort
	 */
    public void setPeersList(List<InetSocketAddress> peers) {
        getAllData().put("peers", InetSocketAddressIO.addressesListToJSON(peers));
    }

    /**
     * Sets the version number for the database using the P2PParams value
     */
    public void setVersion() {
        setString("version", P2PParams.VERSION);
    }

    /**
     * Gets the version number of data saved in the save file.
     * <p>
     * If there is no data then default to current version
     *
     * @return String of P2P version number
     */
    public String getVersion() {
        return getString("version", P2PParams.VERSION);
    }
}
