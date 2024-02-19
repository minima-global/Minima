package org.minima.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;

import org.minima.objects.base.MiniString;

public class RPCClient {
	
	public static String USER_AGENT = "Minima/1.0";
	
	public static String sendGET(String zHost) throws IOException {
		if(zHost.startsWith("https")) {
			return sendGETHTTPS(zHost);
		}else {
			return sendGETBasicAuth(zHost, "", "");
		}
	}
	
	public static String sendGETBasicAuth(String zHost, String zUser, String zPassword) throws IOException {
		//Create the URL
		URL obj = new URL(zHost);
		
		//Open her up
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setRequestMethod("GET");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		
		//Create the Authorisation header
		if(!zPassword.equals("")) {
			String userpass = zUser + ":" + zPassword;
			String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userpass.getBytes()));
			con.setRequestProperty ("Authorization", basicAuth);
		}
		
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
	
	public static String sendGETHTTPS(String zHost) throws IOException {
		//Create the URL
		URL obj = new URL(zHost);
		
		//Open her up
		HttpsURLConnection con = (HttpsURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setRequestMethod("GET");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		
		int responseCode = con.getResponseCode();
		StringBuffer response = new StringBuffer();
		
		if (responseCode == HttpsURLConnection.HTTP_OK) { // success
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
	
	public static String sendGETSSL(String zHost) throws IOException {
		return sendGETBasicAuthSSL(zHost, "", "", null);
	}
	
	public static String sendGETBasicAuthSSL(String zHost, String zUser, String zPassword, SSLContext zSSLContext) throws IOException {
		//Create the URL
		URL obj = new URL(zHost);
		
		//Open her up
		HttpsURLConnection con = (HttpsURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setRequestMethod("GET");
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		con.setSSLSocketFactory(zSSLContext.getSocketFactory());
		
		//Create the Authorisation header
		if(!zPassword.equals("")) {
			String userpass = zUser + ":" + zPassword;
			String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userpass.getBytes()));
			con.setRequestProperty ("Authorization", basicAuth);
		}
		
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
		if(zHost.startsWith("https")) {
			return sendPOSTHTTPS(zHost, zParams, null);
		}else {
			return sendPOST(zHost, zParams, null);
		}
	}
	
	public static String sendPOST(String zHost, String zParams, String zType) throws IOException {
		URL obj = new URL(zHost);
		HttpURLConnection con = (HttpURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setInstanceFollowRedirects(true);
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
	
	public static String sendPOSTHTTPS(String zHost, String zParams, String zType) throws IOException {
		URL obj = new URL(zHost);
		HttpsURLConnection con = (HttpsURLConnection) obj.openConnection();
		con.setConnectTimeout(10000);
		con.setInstanceFollowRedirects(true);
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
		
		if (responseCode == HttpsURLConnection.HTTP_OK) { //success
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
	
	/**
	 * Send GET request with an AUTH token
	 */
	public static String sendGETAuth(String zHost, String zAuthToken) throws IOException {
		
		//Create the URL
		URL obj = new URL(zHost);
		
		//MinimaLogger.log("GETAUTH : "+zHost+" "+zAuthToken);
		
		//Open her up
		HttpURLConnection con = null;
		if(zHost.startsWith("https")) {
			con = (HttpsURLConnection) obj.openConnection();
		}else {
			con = (HttpURLConnection) obj.openConnection();
		}
		
		//Set the Connection details
		con.setConnectTimeout(10000);
		con.setRequestMethod("GET");
		con.setInstanceFollowRedirects(true);
		con.setRequestProperty("User-Agent", USER_AGENT);
		con.setRequestProperty("Connection", "close");
		con.setRequestProperty ("Authorization", "Basic "+zAuthToken);
		
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
		
		String res = response.toString();
				
		//MinimaLogger.log("GETAUTH RESULT : "+res);
		
		return res; 
	}
}
