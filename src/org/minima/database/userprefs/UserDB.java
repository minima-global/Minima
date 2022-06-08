package org.minima.database.userprefs;

import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.system.params.GlobalParams;
import org.minima.utils.JsonDB;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class UserDB extends JsonDB{

	public UserDB() {
		super();
	}
	
	/**
	 * Set your Welcome message
	 */
	public void setWelcome(String zWelcome) {
		setString("welcome", zWelcome);
	}
	
	public String getWelcome() {
		return getString("welcome", "Running Minima "+GlobalParams.MINIMA_VERSION);
	}
	
	/**
	 * Is RPC Enabled on this system..
	 */
	public boolean isRPCEnabled() {
		return getBoolean("rpcenable", false);
	}
	
	public void setRPCEnabled(boolean zEnabled) {
		setBoolean("rpcenable", zEnabled);
	}
	

	/**
	 * The Incentive Cash User
	 */
	public String getIncentiveCashUserID() {
		return getString("uid", "");
	}
	
	public void setIncentiveCashUserID(String zUID) {
		setString("uid", zUID);
	}
	
	/**
	 * SSH Tunnel settings
	 */
	public boolean isSSHTunnelEnabled() {
		return getBoolean("sshenabled", false);
	}
	
	public void setSSHTunnelEnabled(boolean zEnabled) {		
		setBoolean("sshenabled", zEnabled);
	}	
	
	public void setSSHTunnelSettings(JSONObject zSettings) {
		setJSON("sshtunnelsettings", zSettings);
	}
	
	public JSONObject getSSHTunnelSettings() {
		return getJSON("sshtunnelsettings", new JSONObject());
	}

	/**
	 * Web Hooks
	 */
	public void setWebHooks(ArrayList<String> zWebHooks) {
		
		//Create one string..
		JSONArray arr = new JSONArray();
		for(String hook : zWebHooks) {
			arr.add(hook);
		}
		
		//Add this..
		setJSONArray("webhooks", arr);
	}
	
	public ArrayList<String> getWebHooks() {
		ArrayList<String> ret = new ArrayList<>();
		
		JSONArray arr = getJSONArray("webhooks");
		for(Object hook : arr) {
			String hk = (String)hook;
			
			ret.add(hk);
		}
		
		return ret;
	}
	
	
	/**
	 * Load Custom Transactions
	 */
	public MiniData loadCustomTransactions() {
		return getData("custom_transactions", MiniData.ZERO_TXPOWID);
	}
	
	public void saveCustomTransactions(MiniData zCompleteDB) {
		setData("custom_transactions", zCompleteDB);
	}
}
