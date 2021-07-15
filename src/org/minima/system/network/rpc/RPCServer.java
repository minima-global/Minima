package org.minima.system.network.rpc;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import javax.net.ServerSocketFactory;
import javax.net.ssl.SSLServerSocketFactory;

import org.minima.system.Main;
import org.minima.utils.MinimaLogger;

public class RPCServer implements Runnable{

	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public RPCServer(int zPort) {
		mPort = zPort;
		
	    MinimaLogger.log("RPC Server started on port : "+mPort);
	}
	
	public int getPort() {
		return mPort;
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
			//SSL ?
			if(Main.getMainHandler().getNetworkHandler().isSSLEnabled()) {
				//Start a SSL server Socket..
				mServerSocket = Main.getMainHandler().getNetworkHandler().getSSLServerFactory().createServerSocket(mPort);
				
			}else {
				//Start a server Socket..
				mServerSocket = new ServerSocket(mPort);
			}
			
			//Keep listening..
			while(mRunning) {
				//Listen in for connections
				Socket clientsock = mServerSocket.accept();
				
				//create a new RPC Handler ..
				RPCHandler rpc = new RPCHandler(clientsock);
				
				//Run in a new Thread
				Thread rpcthread = new Thread(rpc, "RPC Client");
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
	
	public static void main(String[] zRags) {
		ServerSocketFactory ssocketFactory = SSLServerSocketFactory.getDefault();
        try {
            
        	Runnable tester = new Runnable() {
				@Override
				public void run() {
					
					try {
						Thread.sleep(1000);
						
						System.out.println("Attempt to connect..");
			            
						//Try and connect..
						String resp = RPCClient.sendGET("https://127.0.0.1:2305/hello");
						
						System.out.println(resp);
			            		
					} catch (Exception e) {
						System.out.println("Get connection "+e);
						// TODO Auto-generated catch block
						//e.printStackTrace();
					}
					
				}
			};
			Thread tt = new Thread(tester);
			tt.start();
        	
        	
            /* Create a SSL server socket */
            ServerSocket serverSocket = ssocketFactory.createServerSocket(2305);
            
            System.out.println("waiting for client..");
            
            /* Accepting a client */
            Socket socket = serverSocket.accept();
            
            /* input and output stream buffer */
            BufferedReader inFromClient = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            DataOutputStream outToClient = new DataOutputStream(socket.getOutputStream());
            
            /* read line, ends with \n from client */
            String data = inFromClient.readLine();
            
            System.out.println("Data : " + data);
            
            /* after receive data from client, server reply */
            outToClient.writeBytes("Confirmed!\r\n");
            
            /* close all object */
            outToClient.close();
            inFromClient.close();
            socket.close();
            
        } catch (IOException ex) {
//            ex.printStackTrace();
            System.out.println("Server "+ex);
			
        }
		
	}
}
