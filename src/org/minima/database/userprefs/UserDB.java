package org.minima.database.userprefs;

import org.minima.system.params.GlobalParams;
import org.minima.utils.JsonDB;

public class UserDB extends JsonDB{

	public UserDB() {
		super();
	}
	
	public void setWelcome(String zWelcome) {
		setString("welcome", zWelcome);
	}
	
	public String getWelcome() {
		return getString("welcome", "Running Minima "+GlobalParams.MINIMA_VERSION);
	}
	
	
}
