package org.minima.utils;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

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
	
	public String getString(String zName) {
		return (String)mParams.get(zName);
	}
	
	public void setString(String zName, String zData) {
		mParams.put(zName, zData);
	}
	
	public void clean() {
		mParams = new JSONObject();
	}
	
	public void saveDB(File zFile) {
		try {
			MiniFile.writeObjectToFile(zFile, this);
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
	}
	
	public void loadDB(File zFile) {
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
