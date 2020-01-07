package org.minima.system.network.rpc;

import java.io.IOException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

import org.minima.system.input.InputHandler;
import org.minima.utils.MinimaLogger;

public class RPCServer implements Runnable{

	InputHandler mInputHandler;
	
	ServerSocket mServerSocket;
	int mPort;
	
	boolean mRunning = true;
	
	public RPCServer(InputHandler zInput, int zPort) {
		mInputHandler = zInput;
		mPort = zPort;
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
			MinimaLogger.log("RPCServer : Port "+mPort+" allready in use!.. restart required..");
			
		} catch (SocketException e) {
			if(!mRunning) {
				//Socket shut down..
				MinimaLogger.log("RPCServer : Socket Shutdown..");
			}else {
				e.printStackTrace();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
