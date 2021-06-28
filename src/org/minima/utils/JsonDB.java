package org.minima.utils;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class JsonDB implements Streamable{

	/**
	 * Simple parameter JSON
	 */
	private JSONObject mParams;
	
	public JsonDB() {
		mParams = new JSONObject();
	}
	
	public JSONObject getAllData() {
		return mParams;
	}

	public boolean exists(String zName) {
		return mParams.get(zName) != null;
	}
	
	/**
	 * Boolean functions
	 */
	public boolean getBoolean(String zName, boolean zDefault) {
		if(mParams.get(zName) == null) {
			return zDefault;
		}
		
		return (boolean)mParams.get(zName);
	}
	
	public void setBoolean(String zName, boolean zData) {
		mParams.put(zName, new Boolean(zData));
	}
	
	/**
	 * Number functions
	 */
	public MiniNumber getNumber(String zName, MiniNumber zDefault) {
		if(mParams.get(zName) == null) {
			return zDefault;
		}
		
		String number = (String) mParams.get(zName);
		
		return new MiniNumber(number);
	}
	
	public void setNumber(String zName, MiniNumber zNumber) {
		mParams.put(zName, zNumber.toString());
	}
	
	/**
	 * HEX Data functions
	 */
	public MiniData getHexData(String zName, MiniData zDefault) {
		if(mParams.get(zName) == null) {
			return zDefault;
		}
		
		String data = (String) mParams.get(zName);
		
		return new MiniData(data);
	}
	
	public void setHexData(String zName, MiniData zData) {
		mParams.put(zName, zData.toString());
	}
	
	
	/**
	 * String functions
	 */
	public String getString(String zName, String zDefault) {
		if(mParams.get(zName) == null) {
			return zDefault;
		}
		
		return (String)mParams.get(zName);
	}
	
	public void setString(String zName, String zData) {
		mParams.put(zName, zData);
	}
	
	/**
	 * Wipe the DB
	 */
	public void clean() {
		mParams.clear();
	}
	
	/**
	 * Load and Save
	 */
	public void saveDB(File zFile) {
		try {
			MiniFile.writeObjectToFile(zFile, this);
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
	}
	
	public void loadDB(File zFile) {
		//Does the File exist
		if(!zFile.exists()) {
			MinimaLogger.log("JSONDB file does not exist : "+zFile);
			return;
		}
		
		try {
			FileInputStream fis = new FileInputStream(zFile);
			DataInputStream dis = new DataInputStream(fis);
			
			readDataStream(dis);
			
			dis.close();
			fis.close();
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
	}
	

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniString data = new MiniString(mParams.toString());
		data.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		MiniString data = MiniString.ReadFromStream(zIn);
		try {
			mParams = (JSONObject)(new JSONParser().parse(data.toString()));
		} catch (ParseException e) {
			MinimaLogger.log(e);
			mParams = new JSONObject();
		}
	}

}
