package org.minima.database.userprefs;

import org.minima.system.params.GlobalParams;
import org.minima.utils.JsonDB;

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
	
}
