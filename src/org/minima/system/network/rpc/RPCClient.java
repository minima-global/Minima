package org.minima.system.network.rpc;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;

import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class RPCClient {

	public static String USER_AGENT = "Minima/1.0";
	
	public static String sendGET(String zHost) throws IOException {
		//Create the URL
		URL obj = new URL(zHost);
		
		//Open her up
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
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
			MinimaLogger.log("GET request FAIL "+zHost);
		}
			
		return response.toString(); 
	}

	public static String sendPOST(String zHost, String zParams) throws IOException {
		URL obj = new URL(zHost);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
		//con.setConnectTimeout(20000);
		con.setRequestMethod("POST");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		
		// For POST only - START
		con.setDoOutput(true);
		OutputStream os = con.getOutputStream();
		os.write(zParams.getBytes());
		os.flush();
		os.close();
		// For POST only - END

		int responseCode = con.getResponseCode();
		//System.out.println("POST Response Code :: " + responseCode);

		if (responseCode == HttpURLConnection.HTTP_OK) { //success
			BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
			String inputLine;
			StringBuffer response = new StringBuffer();

			while ((inputLine = in.readLine()) != null) {
				response.append(inputLine);
			}
			in.close();

			// print result
			return response.toString();
		
		} else {
			MinimaLogger.log("POST request not worked "+zHost+" "+zParams);
		}
		
		return "ERROR";
	}

	
	public static void main(String[] zArgs) throws IOException {		

	    
//		String host = "127.0.0.1";
//		int port    = 9005;
//		String request = "status";
//		
//		try {			
//			JSONObject msg = new JSONObject();
//			msg.put("from", "Paddy@127.0.0.1:9005");
//			msg.put("to", "SPartacus@127.0.0.1:7005");
//			msg.put("msg", "Hello You!!");
//			msg.put("signature", "0x73465873658347568345");
//			
//			//Encode..
//			String enc = URLEncoder.encode(new String(msg.toString()),"UTF-8").trim();
//			
//			//Now try a POST
//			String res = sendPOST("http://127.0.0.1:9005/", enc);
//			
//			System.out.println("POST : " + res);
//			
//			
//		} catch (Exception e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
		
		//String url = "https://incentivedb.minima.global/items/directus_users?filter={ \"email\": { \"_eq\": \"'+this.username.value+'\" }}'";
		String url = "https://www.google.com";
		
		String ret = sendGET(url);
		
		System.out.println(ret);
	}
}
