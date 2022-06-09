package org.minima.system.network.rpc;

import java.io.FileInputStream;
import java.io.IOException;
import java.security.KeyStore;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.TrustManager;
import javax.net.ssl.TrustManagerFactory;

import org.minima.utils.MinimaLogger;
 
public class HTTPSServer implements Runnable {
    
	public static int TYPE_FILE = 0;
	public static int TYPE_RPC 	= 1;
	
	private int mPort 			= 9999;
    private boolean mShutdown 	= false;
    
    SSLServerSocket mSSLServerSocket = null;
    
    int mServerType;
    
    public HTTPSServer(int port, int zType){
        //Port and type
    	mPort 		= port;
        mServerType = zType;
        
        //Run it..
		Thread tt = new Thread(this);
		tt.start();
    }
    
    public int getPort() {
    	return mPort;
    }
    
    public void shutdown() {
    	try {
    		mShutdown = true;
			
    		if(mSSLServerSocket != null) {
    			mSSLServerSocket.close();
    		}
		} catch (IOException e) {
			e.printStackTrace();
		}
    }
    
    // Create the and initialize the SSLContext
    private SSLContext createSSLContext(){
        try{
            KeyStore keyStore = KeyStore.getInstance("JKS");
            keyStore.load(new FileInputStream("testkey.jks"),"password".toCharArray());
             
            // Create key manager
            KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
            keyManagerFactory.init(keyStore, "password".toCharArray());
            KeyManager[] km = keyManagerFactory.getKeyManagers();
             
            // Create trust manager
            TrustManagerFactory trustManagerFactory = TrustManagerFactory.getInstance("SunX509");
            trustManagerFactory.init(keyStore);
            TrustManager[] tm = trustManagerFactory.getTrustManagers();
             
            // Initialize SSLContext
            SSLContext sslContext = SSLContext.getInstance("TLSv1.1");
            sslContext.init(km,  tm, null);
             
            return sslContext;
        } catch (Exception ex){
            ex.printStackTrace();
        }
         
        return null;
    }
     
    @Override
    public void run(){
        SSLContext sslContext = this.createSSLContext();
         
        try{
            // Create server socket factory
            SSLServerSocketFactory sslServerSocketFactory = sslContext.getServerSocketFactory();
             
            // Create server socket
            mSSLServerSocket = (SSLServerSocket) sslServerSocketFactory.createServerSocket(this.mPort);
             
            MinimaLogger.log("SSL server started on port "+mPort);
            while(!mShutdown){
                
            	//Get the socket
            	SSLSocket sslSocket = (SSLSocket) mSSLServerSocket.accept();
                
//            	if(mServerType == TYPE_FILE) {
//            		FileHandler handler = new FileHandler(sslSocket);
//                    Thread runner = new Thread(handler);
//                    runner.start();
//            	}else{
//            		CommandHandler handler = new CommandHandler(sslSocket);
//                    Thread runner = new Thread(handler);
//                    runner.start();
//            	}
            }
        } catch (Exception ex){
        	if(!mShutdown) {
        		MinimaLogger.log(ex);
        	}
        }
    }
}