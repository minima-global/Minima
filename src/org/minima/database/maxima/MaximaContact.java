package org.minima.database.maxima;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.json.JSONObject;

public class MaximaContact {

	public int 		mUID = 0;
	
	public String 	mName;
	
	public MiniData mExtraData;
	
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
	long mLastSeen;
	
	/**
	 * Block values to check you are on the same chain
	 */
	MiniNumber 	mTopBlock 	= MiniNumber.ZERO;
	MiniNumber 	mCheckBlock	= MiniNumber.ZERO;
	MiniData 	mCheckHash	= MiniData.ZERO_TXPOWID;
	
	
	public MaximaContact(String zName, String zPublicKey) {
		mName 		= zName;
		mPublicKey	= zPublicKey;
	}
	
	public MaximaContact(ResultSet zSQLResult) throws SQLException {
		mUID			= zSQLResult.getInt("id");
		mName			= zSQLResult.getString("name");
		mExtraData		= new MiniData(zSQLResult.getBytes("extradata"));
		mPublicKey		= zSQLResult.getString("publickey");
		mCurrentAddress	= zSQLResult.getString("currentaddress");
		mMyCurrentAddress	= zSQLResult.getString("myaddress");
		mLastSeen		= zSQLResult.getLong("lastseen");
	}
	
	public MaximaContact(MaximaContact zContact) {
		mUID				= zContact.getUID();
		mName				= zContact.getName();
		mExtraData			= zContact.getExtraData();
		mPublicKey			= zContact.getPublicKey();
		mCurrentAddress		= zContact.getCurrentAddress();
		mMyCurrentAddress	= zContact.getMyAddress();
		mLastSeen			= zContact.getLastSeen();
	}
	
	public void setExtraData(MiniData zExtra){
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
	
	public String getName() {
		return mName;
	}
	
	public MiniData getExtraData() {
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
	
	public long getLastSeen() {
		return mLastSeen;
	}
	
	public void setBlockDetails(MiniNumber zTipBlock, MiniNumber zTipBlock50, MiniData zT50Hash) {
		mTopBlock 		= zTipBlock;
		mCheckBlock 	= zTipBlock50;
		mCheckHash		= zT50Hash;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("id", mUID);
		json.put("name", mName);
		json.put("publickey", mPublicKey);
		json.put("currentaddress", mCurrentAddress);
		json.put("myaddress", mMyCurrentAddress);
		
//		json.put("tipblock", mCurrentBlock.toString());
//		json.put("checkblock", mCurrentBlock50.toString());
//		json.put("checkhash", mCurrentBlock50Hash.to0xString());
		
		json.put("lastseen", mLastSeen);
		json.put("date", new Date(mLastSeen).toString());
		
		return json;
	}
}
