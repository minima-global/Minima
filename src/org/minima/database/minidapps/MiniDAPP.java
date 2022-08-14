package org.minima.database.minidapps;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.SqlDB;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.ParseException;

public class MiniDAPP {

	/**
	 * The Random UID of the MiniDAPP
	 */
	private String 	mUID;
	
	/**
	 * The Complete JSON conf file 
	 */
	private JSONObject mConfData = new JSONObject();
	
	
	public MiniDAPP(String zUID, JSONObject zConf) {
		mUID 			= zUID;
		mConfData		= zConf;
	}
	
	public MiniDAPP(ResultSet zResultSet) throws SQLException {
		mUID 			= zResultSet.getString("uid");
		
		//Extra Data is a JSONOBject stored as bytes
		MiniData confbytes = new MiniData(zResultSet.getBytes("confdata")); 
		try {
			mConfData	= SqlDB.convertDataToJSONObject(confbytes);
		} catch (ParseException e) {
			MinimaLogger.log(e);
			
			//Create a default
			mConfData = new JSONObject();
		} 
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		//Make sure browser are poermissions are in there
		if(!mConfData.containsKey("browser")) {
			mConfData.put("browser", getBrowser());
		}
		
		if(!mConfData.containsKey("permission")) {
			mConfData.put("permission", getPermission());
		}
		
		ret.put("uid", mUID);
		ret.put("conf", mConfData);
		
		return ret;
	}
	
	public String getUID() {
		return mUID;
	}
	
	public JSONObject getConfData() {
		return mConfData;
	}
	
	public String getName() {
		return mConfData.getString("name", "no_name");
	}
	
	public String getDescription() {
		return mConfData.getString("description", "no_description");
	}
	
	public String getIcon() {
		return mConfData.getString("icon", "");
	}
	
	public String getVersion() {
		return mConfData.getString("version", "no_version");
	}
	
	public String getBrowser() {
		return mConfData.getString("browser", "internal");
	}
	
	public String getPermission() {
		return mConfData.getString("permission", "read");
	}
	
	public void setPermission(String zPermission) {
		mConfData.put("permission", zPermission);
	}
}
