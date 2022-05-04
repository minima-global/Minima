package org.minima.database.maxima;

import java.security.KeyPair;

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
	
	public MaximaHost() {}
	
	public MaximaHost(String zHost) {
		mHost = zHost;
	}
	
	public MaximaHost(String zHost, MiniData zPublic, MiniData zPrivate) {
		mHost 		= zHost;
		mPublic		= zPublic;
		mPrivate 	= zPrivate;
	}
	
	public MaximaHost(JSONObject zDetails) {
		mHost 		= (String) zDetails.get("host");
		mPrivate	= new MiniData( (String) zDetails.get("private") );
		mPublic		= new MiniData( (String) zDetails.get("public") );
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
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("host", mHost);
		ret.put("private", mPrivate.to0xString());
		ret.put("public", mPublic.to0xString());
		
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
}
