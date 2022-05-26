package org.minima.database.maxima;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class MaximaContact {

	/**
	 * Unique database ID
	 */
	public int 		mUID = 0;
	
	/**
	 * Extra data can be stored with the contact as a aJSON
	 */
	public JSONObject mExtraData;
	
	/**
	 * The actual MAIN public Key of the Contact
	 */
	public String mPublicKey;
	
	/**
	 * Where you contact them
	 */
	public String 	mCurrentAddress;
	
	/**
	 * Where they contact you
	 */
	public String 	mMyCurrentAddress;
	
	/**
	 * Last Seen
	 */
	long mLastSeen = System.currentTimeMillis();
	
	public MaximaContact(String zPublicKey) {
		mPublicKey	= zPublicKey;
		mExtraData 	= new JSONObject();
		setname("noname");
		setMinimaAddress("Mx00");
		setBlockDetails(MiniNumber.ZERO, MiniNumber.ZERO, MiniData.ZERO_TXPOWID);
	}
	
	public MaximaContact(ResultSet zSQLResult) throws SQLException {
		mUID			= zSQLResult.getInt("id");
		mPublicKey		= zSQLResult.getString("publickey");
		mCurrentAddress	= zSQLResult.getString("currentaddress");
		mMyCurrentAddress	= zSQLResult.getString("myaddress");
		mLastSeen		= zSQLResult.getLong("lastseen");
		
		//Extra Data is a JSONOBject stored as bytes
		MiniData extrabytes = new MiniData(zSQLResult.getBytes("extradata")); 
		try {
			mExtraData	= convertDataToJSONObject(extrabytes);
		} catch (ParseException e) {
			MinimaLogger.log(e);
			
			//Create a default
			mExtraData = new JSONObject();
			setname("noname");
			setMinimaAddress("Mx00");
			setBlockDetails(MiniNumber.ZERO, MiniNumber.ZERO, MiniData.ZERO_TXPOWID);
		} 
	}
	
	public void setExtraData(JSONObject zExtra){
		mExtraData = zExtra;
	}
	
	public void setCurrentAddress(String zAddress) {
		mCurrentAddress = zAddress;
	}
	
	public void setMyAddress(String zMyAddress) {
		mMyCurrentAddress = zMyAddress;
	}
	
	public int getUID() {
		return mUID;
	}
	
	public void setMinimaAddress(String zMxAddress) {
		mExtraData.put("minimaaddress", zMxAddress);
	}
	
	public String getMinimaAddress() {
		return (String)mExtraData.get("minimaaddress");
	}
	
	public void setname(String zName) {
		mExtraData.put("name", zName);
	}
	
	public String getName() {
		return (String)mExtraData.get("name");
	}
	
	public JSONObject getExtraData() {
		return mExtraData;
	}
	
	public String getPublicKey() {
		return mPublicKey;
	}
	
	public String getCurrentAddress() {
		return mCurrentAddress;
	}
	
	public String getMyAddress() {
		return mMyCurrentAddress;
	}
	
	public void setLastSeen(long zLastSeen) {
		mLastSeen = zLastSeen;
	}
	
	public long getLastSeen() {
		return mLastSeen;
	}
	
	public void setBlockDetails(MiniNumber zTipBlock, MiniNumber zTipBlock50, MiniData zT50Hash) {
		mExtraData.put("topblock", zTipBlock.toString());
		mExtraData.put("checkblock", zTipBlock50.toString());
		mExtraData.put("checkhash", zT50Hash.to0xString());
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("id", mUID);
		json.put("publickey", mPublicKey);
		json.put("currentaddress", mCurrentAddress);
		json.put("myaddress", mMyCurrentAddress);
		json.put("lastseen", mLastSeen);
		json.put("date", new Date(mLastSeen).toString());
		json.put("extradata", mExtraData);
		
		return json;
	}
	
	public static JSONObject convertDataToJSONObject(MiniData zData) throws ParseException {
		
		//First convert the Data back into a String
		MiniString str = new MiniString(zData.getBytes());
		
		//And now convert that String into a JSONOBject
		JSONObject json = (JSONObject) new JSONParser().parse(str.toString());
		
		return json;
	}
	
	public static MiniData convertJSONObjectToData(JSONObject zJSON) {
		
		//First convert the Data back into a String
		MiniString str = new MiniString(zJSON.toString());
		
		//And now convert that String into a MiniData
		MiniData data = new MiniData(str.getData());
		
		return data;
	}
}
