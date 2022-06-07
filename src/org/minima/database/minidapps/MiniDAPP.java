package org.minima.database.minidapps;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.minima.utils.json.JSONObject;

public class MiniDAPP {

	public String 	mUID;
	public String 	mName;
	public String 	mIcon;
	public String 	mVersion;
	public String 	mDescription;
	
	public MiniDAPP(String zUID, String zName, String zIcon, String zVersion, String zDescription) {
		mUID 			= zUID;
		mName 			= zName;
		mIcon 			= zIcon;
		mVersion		= zVersion;
		mDescription 	= zDescription;
	}
	
	public MiniDAPP(ResultSet zResultSet) throws SQLException {
		mUID 			= zResultSet.getString("uid");
		mName 			= zResultSet.getString("name");
		mIcon 			= zResultSet.getString("icon");
		mVersion 		= zResultSet.getString("version");
		mDescription 	= zResultSet.getString("description");
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("uid", mUID);
		ret.put("name", mName);
		ret.put("icon", mIcon);
		ret.put("version", mVersion);
		ret.put("description", mDescription);
		
		return ret;
	}
}
