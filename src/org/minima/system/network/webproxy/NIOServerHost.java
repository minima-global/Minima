package org.minima.system.network.webproxy;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.concurrent.ConcurrentHashMap;

import org.minima.system.network.WebProxyManager;
import org.minima.utils.messages.Message;

public class NIOServerHost implements Runnable {

	/**
	 * The Main selector
	 */
	Selector mSelector;
	
	/**
	 * Main Channel
	 */
	ServerSocketChannel mServerSockerChannel;
	
	/**
	 * The server socket
	 */
	ServerSocket mServerSocket;
	
	/**
	 * Hashcode link to the socket channel
	 */
	public ConcurrentHashMap<Integer, SocketChannel> mChannels = new ConcurrentHashMap<Integer, SocketChannel>();
    
	/**
	 * The Port
	 */
	int mPort;
	
	boolean mRunning;
	
	MainProxyHandler mHandler;
	
	public NIOServerHost(int zPort,MainProxyHandler zHandler) {
		mPort = zPort;
		mHandler = zHandler;
		
		//Run it
    	Thread tt = new Thread(this);
    	tt.start();
	}
	
	public boolean isRunning() {
		return mRunning;
	}
	
	public void stop() {
		mRunning = false;
	}
	
	public void sendToUser(String request, Integer zUserID) {
		try {
			//Get the Channel..
			SocketChannel sc = mChannels.get(zUserID);
		
			System.out.println("Sending message "+request+" to "+zUserID+" sc:"+sc);
			
			//Wrap up the message..
			String sendstring = request+"\n";
			ByteBuffer buf = ByteBuffer.wrap(sendstring.getBytes());
			
			//And pump it..
			sc.write(buf);
			
		}catch(Exception exc) {
			exc.printStackTrace();
		}
	}
	
	@Override
	public void run() {
		//Now running
		mRunning = true;
		
		//Create a buffer
		int maxbuffer = 1024;
		ByteBuffer readBuffer = ByteBuffer.allocate(maxbuffer);
		byte[] byteBuffer     = new byte[maxbuffer];
		
		try {
			// Create a new selector
	    	mSelector = Selector.open();
	        
	    	// Open a listener on each port, and register each one
	        mServerSockerChannel = ServerSocketChannel.open();
	        mServerSockerChannel.configureBlocking(false);
	        
	        //Get the Socket
	        mServerSocket = mServerSockerChannel.socket();            
	        mServerSocket.bind(new InetSocketAddress(mPort));
	        
	        //registers ACCEPT
	        mServerSockerChannel.register(mSelector, SelectionKey.OP_ACCEPT);
	    
	        //Message
	        System.out.println(getClass().getName()+" Going to listen on " + mPort);
		
	        while (mRunning) {
	        	//Start selecting
	            mSelector.select(1000);
	            
	            //Whats happening..
	            Set<SelectionKey> selectedKeys = mSelector.selectedKeys();
	            Iterator<SelectionKey> it = selectedKeys.iterator();            
	            
	            //Cycle through
	            while (it.hasNext()) {
	                SelectionKey key = (SelectionKey) it.next();
	                
	                if ((key.readyOps() & SelectionKey.OP_ACCEPT) == SelectionKey.OP_ACCEPT) {
	                	// Accept the new connection
	                    ServerSocketChannel sscNew = (ServerSocketChannel) key.channel();
	                    SocketChannel sc = sscNew.accept();
	                    sc.configureBlocking(false);
	                    
	                    //Get a UID for the user
	                    Integer user = new Integer(sc.hashCode());
	                    
	                    // Add the new connection to the selector                    
	                    sc.register(mSelector, SelectionKey.OP_READ, user );
	                    
	                    // Add the socket channel to the list
	                    mChannels.put(user, sc);
	                    
	                    System.out.println("Accept Channel from "+user +" "+sc);
	                	
	                } else if ((key.readyOps() & SelectionKey.OP_READ) == SelectionKey.OP_READ) {
	                    //WHO
	                	Integer user = (Integer)key.attachment();
	                	
	                	// Read the data
	                    SocketChannel sc = (SocketChannel) key.channel();            
	                    
	                    //Clear the input buffer
	                    readBuffer.clear();
	                    
	                    int readlen = 0;
	                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
	                    
	                    while ((readlen = sc.read(readBuffer)) > 0) {
	                    	int pos = readBuffer.position();
	                    	readBuffer.flip();
	                    	readBuffer.get(byteBuffer,0,pos);
	                    	
	                    	baos.write(byteBuffer,0,pos);
	                    	
	                    	//Now compact
	                        readBuffer.compact();
	                    }
	                    
	                    //And convert.
	                    String msg = new String(baos.toByteArray()).trim();
		                
	                    System.out.println("Complete Message "+msg);
	                    
	                    //Close the channel
	                    if (readlen == -1) {
	                    	//Remove from the set
	                    	mChannels.remove(user);
	                    	
                        	//Close the channel
	                        sc.close();
	                        
	                        Message usermsg = new Message(MainProxyHandler.PROXY_USER_CLOSE);
                        	usermsg.addInt("userid", user.intValue());
                        	
	                    	//Say it..
                        	mHandler.PostMessage(usermsg);
	                        
	                        //say it..
	                        System.out.println("Channel closed :"+user);
                        }else{
	                    	if(msg.length()>0) {
	                        	//All messages end in WEBPROXY_MESSAGE_END
	                    		StringTokenizer strtok = new StringTokenizer(msg,WebProxyManager.WEBPROXY_MESSAGE_END);
	                    		while(strtok.hasMoreElements()) {
	                    			String msgtok = strtok.nextToken().trim();
	                    			
	                    			//Create a message
		                        	Message usermsg = new Message(MainProxyHandler.PROXY_USER_MESSAGE);
		                        	usermsg.addInt("userid", user.intValue());
		                        	usermsg.addString("data", msgtok);
		                        	
			                    	//Say it..
		                        	mHandler.PostMessage(usermsg);
	                    		}
	                    		
//	                    		//Create a message
//	                        	Message usermsg = new Message(MainProxyHandler.PROXY_USER_MESSAGE);
//	                        	usermsg.addInt("userid", user.intValue());
//	                        	usermsg.addString("data", msg);
//	                        	
//		                    	//Say it..
//	                        	mHandler.PostMessage(usermsg);
	                    	}
	                    }
	                    
	                } 
	                
	                //Remove the key
	                it.remove();
	            }
	        }
	        
	        System.out.println("Shutting down..");
	        
	        //Shut down..
	        mServerSocket.close();
	        mServerSockerChannel.close();
	        mSelector.close();
	        
		}catch(Exception exc) {
			exc.printStackTrace();
        }
	}
	
	/**
     * This method sends messages to a random outPutStream
     */
    private void sendMsgsToRandomClients() {
        new Thread("Send-to-Clients") {
            public void run() {
                try {
                	int num = 0;
                    while (true) {
                        Random generator = new Random();
                        if(mChannels.keySet().size()>0){
                            Integer randomKey = new ArrayList<Integer>(mChannels.keySet()).get(generator.nextInt(mChannels.keySet().size()));
                            SocketChannel sc = mChannels.get(randomKey);
                            
                            try {
                            	num++;                                
                                ByteBuffer buf = ByteBuffer.wrap(("From server to Client msg nº - "+ num + "\n").getBytes());
                                sc.write(buf);
                            } catch (IOException e) {
                                e.printStackTrace();
                                mChannels.remove(randomKey);
                            }                            
                        }
                        
                        Thread.sleep(1000);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        }.start();
    }
	
}
