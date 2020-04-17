package org.minima.system.network.minidapps;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.utils.MinimaLogger;

public class DAPPServer implements Runnable{

	ServerSocket mServerSocket;
	DAPPManager mDAPPMAnager;
	
	int mPort;
	String mHost;
	
	boolean mRunning = true;
	
	public DAPPServer(int zPort, DAPPManager zDAPPMAnager) {
		mPort = zPort;
		mHost = "127.0.0.1";
		mDAPPMAnager = zDAPPMAnager;
		
		System.out.println("DAPP Server started on "+mHost+":"+mPort);
	}
	
	public int getPort() {
		return mPort;
	}
	
	public String getHost() {
		return mHost;
	}
		
	public void stop() {
		mRunning = false;
		
		try {
			if(mServerSocket != null) {
				mServerSocket.close();
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Override
	public void run() {
		try {
			//Start a server Socket..
			mServerSocket = new ServerSocket(mPort);
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new RPC Handler ..
				DAPPHandler rpc = new DAPPHandler(clientsock,mDAPPMAnager);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(rpc);
				rpcthread.start();
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("DAPPServer : Port "+mPort+" already in use!.. restart required..");
			
		} catch (SocketException e) {
			if(mRunning) {
				//Socket shut down..
				MinimaLogger.log("DAPPServer : Socket Shutdown.. "+e);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		System.out.println("DAPP Server stopped");
	}
	
	
	
	public static void main(String[] zArgs) {
		
//		//Start the DAPP Server
//		DAPPServer sever  = new DAPPServer(21000);
//		Thread tt = new Thread(sever);
//		tt.start();
//		
//		try {
//			//Listen for input
//		    InputStreamReader is    = new InputStreamReader(System.in);
//		    BufferedReader bis      = new BufferedReader(is);
//	
//		    //Loop until finished..
//		    while(true){
//	            //Get a line of input
//	            String input = bis.readLine().trim();
//	            
//	            if(input.equals("quit")) {
//	            	sever.stop();
//	            	break;
//	            }else if(!input.equals("")) {
//		            System.out.println("Unknown command : "+input);
//	            }
//		    }
//		}catch(Exception exc) {
//			System.out.println(exc);	
//		}
		
		System.out.println("Finished..");
	}
}
