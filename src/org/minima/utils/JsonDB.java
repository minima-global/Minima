package org.minima.utils;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class JsonDB implements Streamable{

	/**
	 * Simple parameter JSON
	 */
	protected JSONObject mParams;
	
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
		mParams.put(zName, zData);
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
	public MiniData getData(String zName, MiniData zDefault) {
		if(mParams.get(zName) == null) {
			return zDefault;
		}
		
		String data = (String) mParams.get(zName);
		
		return new MiniData(data);
	}
	
	public void setData(String zName, MiniData zData) {
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
	 * JSONObject
	 */
	public void setJSON(String zName, JSONObject zJSON) {
		mParams.put(zName, zJSON);
	}
	
	public JSONObject getJSON(String zName, JSONObject zDefault) {
		if(mParams.get(zName) == null) {
			return zDefault;
		}
		
		return (JSONObject)mParams.get(zName);
	}
	
	/**
	 * JSONArray
	 */
	public void setJSONArray(String zName, JSONArray zJSONArray) {
		mParams.put(zName, zJSONArray);
	}
	
	public JSONArray getJSONArray(String zName ) {
		if(mParams.get(zName) == null) {
			return new JSONArray();
		}
		
		return (JSONArray)mParams.get(zName);
	}
	
	/**
	 * Load and Save
	 */
	public void loadDB(File zFile) {
		MiniFile.loadObject(zFile, this);
	}
	
	public void saveDB(File zFile) {
		MiniFile.saveObject(zFile, this);
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
