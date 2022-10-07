package org.minima.utils;


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;

import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

public class RPCClient {

	public static String USER_AGENT = "Minima/1.0";
	
	public static String sendGET(String zHost) throws IOException {
		//Create the URL
		URL obj = new URL(zHost);
		
		//Open her up
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setRequestMethod("GET");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		
		int responseCode = con.getResponseCode();
		StringBuffer response = new StringBuffer();
		
		if (responseCode == HttpURLConnection.HTTP_OK) { // success
			InputStream is = con.getInputStream();
			
			BufferedReader in = new BufferedReader(new InputStreamReader(is));
			String inputLine;
			
			while ((inputLine = in.readLine()) != null) {
				response.append(inputLine);
			}
			
			in.close();
			is.close();
			
		} else {
			System.out.println("GET request not HTTP_OK resp:"+responseCode+" @ "+zHost);
		}
			
		return response.toString(); 
	}
	
	public static String sendPUT(String zHost) throws IOException {
		//Create the URL
		URL obj = new URL(zHost);
		
		//Open her up
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setRequestMethod("PUT");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		
		int responseCode = con.getResponseCode();
		StringBuffer response = new StringBuffer();
		
		if (responseCode == HttpURLConnection.HTTP_OK) { // success
			InputStream is = con.getInputStream();
			
			BufferedReader in = new BufferedReader(new InputStreamReader(is));
			String inputLine;
			
			while ((inputLine = in.readLine()) != null) {
				response.append(inputLine);
			}
			
			in.close();
			is.close();
			
		} else {
			System.out.println("PUT request not HTTP_OK resp:"+responseCode+" @ "+zHost);
		}
			
		return response.toString(); 
	}

	public static String sendPOST(String zHost, String zParams) throws IOException {
		return sendPOST(zHost, zParams, null);
	}
	
	public static String sendPOST(String zHost, String zParams, String zType) throws IOException {
		URL obj = new URL(zHost);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setRequestMethod("POST");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		
		//Type specified..
		if(zType != null) {
			con.setRequestProperty("Content-Type", zType);
		}
		
		// For POST only - START
		con.setDoOutput(true);
		OutputStream os = con.getOutputStream();
		os.write(zParams.getBytes(MiniString.MINIMA_CHARSET));
		os.flush();
		os.close();
		// For POST only - END

		int responseCode = con.getResponseCode();
		
		if (responseCode == HttpURLConnection.HTTP_OK) { //success
			BufferedReader in 		= new BufferedReader(new InputStreamReader(con.getInputStream(),MiniString.MINIMA_CHARSET));
			StringBuffer response 	= new StringBuffer();

			String inputLine;
			while ((inputLine = in.readLine()) != null) {
				response.append(inputLine);
			}
			in.close();

			return response.toString();
		} 
		
		throw new IOException("POST request not HTTP_OK resp:"+responseCode+" @ "+zHost+" params "+zParams);
	}

	
	public static void main(String[] zArgs) throws IOException {		

	    String totalline = "";
		for(String arg : zArgs) {
//			System.out.println("ARG : " + arg);
			totalline += arg+" ";
		}
		
		totalline = URLEncoder.encode(totalline, MiniString.MINIMA_CHARSET);
		
		try {
			//Now run this function..
			String result = sendGET("http://127.0.0.1:9002/"+totalline);
			
			//Create a JSON
			JSONObject json = (JSONObject) new JSONParser().parse(result);
			
			//Output the result..
			System.out.println(MiniFormat.JSONPretty(json));
			
		}catch(Exception exc) {
			MinimaLogger.log("ERROR CMDHANDLER : "+totalline+" "+exc);
		}
	}
}
