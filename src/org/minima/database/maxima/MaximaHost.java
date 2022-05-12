package org.minima.database.maxima;

import java.security.KeyPair;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.json.JSONObject;

public class MaximaHost {

	/**
	 * The Host
	 */
	public String mHost;
	
	/**
	 * RSA Keys used with this host
	 */
	MiniData mPublic;
	MiniData mPrivate;
	
	long mLastSeen = System.currentTimeMillis();
	
	int mConnected;
	
	public MaximaHost() {}
	
	public MaximaHost(String zHost) {
		mHost = zHost;
	}
	
	public MaximaHost(ResultSet zSQLResult) throws SQLException {
		mHost 		= zSQLResult.getString("host");
		mPublic		= new MiniData(zSQLResult.getBytes("publickey"));
		mPrivate 	= new MiniData(zSQLResult.getBytes("privatekey"));
		mLastSeen	= zSQLResult.getLong("lastseen");
		mConnected	= zSQLResult.getInt("connected");
	}
	
	public MaximaHost(String zHost, MiniData zPublic, MiniData zPrivate) {
		mHost 		= zHost;
		mPublic		= zPublic;
		mPrivate 	= zPrivate;
	}
	
	public void createKeys() throws Exception {
		//Create a new new maxima ident..
		KeyPair generateKeyPair = GenerateKey.generateKeyPair();
		
		byte[] publicKey 		= generateKeyPair.getPublic().getEncoded();
		mPublic 				= new MiniData(publicKey);
		
		byte[] privateKey	 	= generateKeyPair.getPrivate().getEncoded();
		mPrivate 				= new MiniData(privateKey);
	}
	
	public void setKeys(MiniData zPrivate, MiniData zPublic) {
		mPrivate 	= zPrivate;
		mPublic		= zPublic;
	}
	
	public void updateLastSeen() {
		mLastSeen = System.currentTimeMillis();
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("host", mHost);
//		ret.put("private", mPrivate.to0xString());
		ret.put("public", mPublic.to0xString());
		ret.put("lastseen", new Date(mLastSeen));
		ret.put("connected", isConnected());
		ret.put("address", getMaximaAddress());
		
		return ret;
	}
	
	public String getHost() {
		return mHost;
	}
	
	public MiniData getPublicKey() {
		return mPublic;
	}
	
	public MiniData getPrivateKey() {
		return mPrivate;
	}
	
	public long getLastSeen() {
		return mLastSeen;
	}

	public void setConnected(int zConnected) {
		updateLastSeen();
		mConnected = zConnected;
	}
	
	public int getConnected() {
		return mConnected;
	}
	
	public boolean isConnected() {
		return mConnected != 0;
	}
	
	public String getMaximaAddress() {
		return Address.makeMinimaAddress(getPublicKey())+"@"+mHost;
	}
}
