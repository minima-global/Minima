package org.minima.system.network.rpc;

import java.io.IOException;
import java.net.BindException;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.Enumeration;

import org.minima.system.input.InputHandler;
import org.minima.utils.MinimaLogger;

public class RPCServer implements Runnable{

	InputHandler mInputHandler;
	
	ServerSocket mServerSocket;
	int mPort;
	static String mHost;
	
	boolean mRunning = true;
	
	public RPCServer(InputHandler zInput, int zPort) {
		mInputHandler = zInput;
		mPort = zPort;
		
		mHost = "127.0.0.1";
		boolean found = false;
	    try {
		    Enumeration<NetworkInterface> interfaces = NetworkInterface.getNetworkInterfaces();
	        while (!found && interfaces.hasMoreElements()) {
	            NetworkInterface iface = interfaces.nextElement();
	            // filters out 127.0.0.1 and inactive interfaces
	            if (iface.isLoopback() || !iface.isUp())
	                continue;

	            Enumeration<InetAddress> addresses = iface.getInetAddresses();
	            while(!found && addresses.hasMoreElements()) {
	                InetAddress addr = addresses.nextElement();
	                String ip   = addr.getHostAddress();
	                String name = iface.getDisplayName();
	                
	                //Only get the IPv4
	                if(!ip.contains(":")) {
	                	mHost = ip;
	                	
	                	if(name.startsWith("wl")) {
	                		found = true;
	                		break;
	                	}
	                }
	            }
	        }
	    } catch (SocketException e) {
	        System.out.println("RPCSERVER : "+e);
	    }
	    
	    System.out.println("RPC Server started on "+mHost+":"+mPort);
	}
	
	public int getPort() {
		return mPort;
	}
	
	public static String getHost() {
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
				RPCHandler rpc = new RPCHandler(clientsock, mInputHandler);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(rpc);
				rpcthread.start();
			}
			
		} catch (BindException e) {
			//Socket shut down..
			MinimaLogger.log("RPCServer : Port "+mPort+" already in use!.. restart required..");
			
		} catch (SocketException e) {
			if(mRunning) {
				//Socket shut down..
				MinimaLogger.log("RPCServer : Socket Shutdown.. "+e);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
