package org.minima.system.network.sshtunnel;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class TunnelDB implements Streamable{

	/**
	 * Simple parameter JSON
	 */
	JSONObject mParams;
	
	
	public TunnelDB() {
		mParams = new JSONObject();
	}

	public JSONObject getParams() {
		return mParams;
	}

	public void clean() {
		mParams = new JSONObject();
	}
	
	public void saveDB(File zFile) {
		//Save the MaximaDB
		try {
			FileOutputStream fos = new FileOutputStream(zFile);
			DataOutputStream dos = new DataOutputStream(fos);
			
			writeDataStream(dos);
			dos.flush();
			
			dos.close();
			fos.close();
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
	}
	
	public void loadDB(File zFile) {
		//Get the MaximaDB
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
