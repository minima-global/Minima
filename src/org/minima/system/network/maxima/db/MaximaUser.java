package org.minima.system.network.maxima.db;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class MaximaUser implements Streamable {

	public JSONObject mData;
		
	public MaximaUser() {}

	public MaximaUser(String zPubkey, String zHost) {
		mData = new JSONObject();
		
		setPublicKey(zPubkey);
		setHost(zHost);
		setTimeStamp(System.currentTimeMillis());
		setRSAPubKeyHex("0x00");
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("publickey", getPublicKey());
		json.put("host",getHost());
		json.put("RSA",getRSAPubKeyHex());
		json.put("timestamp", new Date(getTimeStamp()).toString());
		
		return json;
	}
	
	public String getPublicKey() {
		return (String) mData.get("publickey");
	}
	
	public void setPublicKey(String zPubKey) {
		mData.put("publickey", zPubKey);
	}
	
	public String getHost() {
		return (String) mData.get("host");
	}
	
	public void setHost(String zHost) {
		mData.put("host", zHost);
	}
	
	public String getCompleteAddress() {
		return getPublicKey()+"@"+getHost();
	}
	
	public long getTimeStamp() {
		String ts = (String) mData.get("timestamp");
		return Long.parseLong(ts);
	}
	
	public void setTimeStamp(long zTimeStamp) {
		mData.put("timestamp", ""+zTimeStamp);
	}

	public void setRSAPubKeyHex(String zPubKey) {
		mData.put("RSA",zPubKey);
	}
	
	public String getRSAPubKeyHex() {
		return (String) mData.get("RSA");
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniString str = new MiniString(mData.toString());
		str.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		MiniString ds = MiniString.ReadFromStream(zIn);
		try {
			mData = (JSONObject)(new JSONParser().parse(ds.toString()));
		} catch (ParseException e) {
			MinimaLogger.log(e);
			mData = new JSONObject();
		}
	}
	
	
}
