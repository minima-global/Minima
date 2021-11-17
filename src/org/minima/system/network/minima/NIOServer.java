package org.minima.system.network.minima;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.StandardSocketOptions;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.concurrent.ConcurrentHashMap;

import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class NIOServer implements Runnable {

	public static boolean mTraceON = false;
	
	NIOManager mNIOManager;
	
	int mPort;
	
	Selector mSelector;
	
	boolean mShutDown;
	
	ConcurrentHashMap<String, NIOClient> mClients = new ConcurrentHashMap<>();
	
	ArrayList<SocketChannel> mRegisterChannels;
	
	ArrayList<String> mDisconnectChannels;
	
	public NIOServer(int zPort, NIOManager zNIOManager) {
		mPort 				= zPort;
		mNIOManager 		= zNIOManager;
		mRegisterChannels	= new ArrayList<>();
		mDisconnectChannels	= new ArrayList<>();
	}
	
	public void shutdown() {
		mShutDown = true;
		if(mSelector != null) {
			mSelector.wakeup();
		}
	}
	
	public int getNetClientSize() {
		return mClients.size();
	}
	
	public ArrayList<NIOClient> getAllNIOClients(){
		ArrayList<NIOClient> allclients = new ArrayList<>();
		
		Enumeration<NIOClient> clients = mClients.elements();
		while(clients.hasMoreElements()) {
			allclients.add(clients.nextElement());
		}
		
		return allclients;
	}
	
	public void regsiterNewSocket(SocketChannel zChannel) {
		synchronized (mRegisterChannels) {
			mRegisterChannels.add(zChannel);
		}
		
		mSelector.wakeup();
	}
	
	public void disconnect(String zUID) {
		synchronized (mDisconnectChannels) {
			mDisconnectChannels.add(zUID);
		}
		
		mSelector.wakeup();
	}
	
	public NIOClient getClient(String zUID) {
		return mClients.get(zUID);
	}
	
	public void sendMessage(String zUID, MiniData zData) {
		NIOClient client =  mClients.get(zUID);
		if(client != null) {
			client.sendData(zData);
		}
	}
	
	public void sendMessageAll(MiniData zData) {
		Enumeration<NIOClient> clients = mClients.elements();
		while(clients.hasMoreElements()) {
			clients.nextElement().sendData(zData);
		}
	}
	
	@Override
	public void run() {
	
		try {
			// Bind to 0.0.0.0 address which is the local network stack
	        InetAddress addr = InetAddress.getByName("0.0.0.0");
	
	        // Open a new ServerSocketChannel so we can listen for connections
	        ServerSocketChannel serversocket = ServerSocketChannel.open();
	
	        // Configure the socket to be non-blocking as part of the new-IO library (NIO)
	        serversocket.configureBlocking(false);
	
	        // Bind our socket to the local port
	        serversocket.socket().bind(new InetSocketAddress(addr.getHostName(), mPort));
	
	        // Reuse the address so more than one connection can come in
	        serversocket.socket().setReuseAddress(true);
	
	        // Open our selector channel
	        mSelector = Selector.open(); // selector is open here
	
	        // Register an "Accept" event on our selector service which will let us know when sockets connect to our channel
	        SelectionKey acceptKey = serversocket.register(mSelector, SelectionKey.OP_ACCEPT);
	
	        // Set our key's interest OPs to "Accept"
	        acceptKey.interestOps(SelectionKey.OP_ACCEPT);
	
	        //Ok - we are up and running..
	        mNIOManager.PostMessage(NIOManager.NIO_SERVERSTARTED);
	        
	        // This is the main loop
	        while (!mShutDown) {
	        	
	        	//Logs..
	        	if(mTraceON) {
	        		MinimaLogger.log("[NIOSERVER] Waiting for selection..");
	        	}
	        	
	        	//Select something.. 
	        	mSelector.select(30000);
	        	
	        	//Are there any Channels to add..
	        	synchronized (mRegisterChannels) {
        			if(mRegisterChannels.size()>0) {
		        		for(SocketChannel chann : mRegisterChannels) {
		        			addChannel(false,chann);
		        		}
		        		
	        			//And clear..
	        			mRegisterChannels.clear();
        			}
	        	}
	        	
	        	//Are there any Channels to disconnect..
	        	synchronized (mDisconnectChannels) {
        			if(mDisconnectChannels.size()>0) {
		        		for(String uid: mDisconnectChannels) {
		        			NIOClient client = mClients.get(uid);
		        			if(client != null) {
		        				//Close client and invalidate key
		        				client.disconnect();
		        				
		        				//Remove from the list..
			                    mClients.remove(uid);
			                    
			                    //Tell the Network Manager
			                    Message newclient = new Message(NIOManager.NIO_DISCONNECTED)
			                    		.addObject("client", client)
			                    		.addBoolean("reconnect", false);
			                    
			                    mNIOManager.PostMessage(newclient);
		        			}
		        		}
		        		
	        			//And clear..
		        		mDisconnectChannels.clear();
        			}
	        	}
	        	
	        	//Loop through the current keys
	            Iterator<SelectionKey> iterator = mSelector.selectedKeys().iterator();
	            while (iterator.hasNext()) {
	            	
	                //Get  and remove the next key
	            	SelectionKey key = (SelectionKey) iterator.next();
	                iterator.remove();
	
	                // Get a reference to one of our custom objects
	                NIOClient client = (NIOClient) key.attachment();
	                
	                // skip any invalid / cancelled keys
	                if (!key.isValid()) {
	                	continue;
	                }
	                
	                try {
	                	if (key.isAcceptable()) {
	                		// Accept the socket's connection
	                        SocketChannel socket = serversocket.accept();
	                		
	                        //And add to our selector..
	                        addChannel(true, socket);
	                    }
	
	                    if (key.isReadable()) {
	                    	client.handleRead();
	                    }
	                    
	                    if (key.isWritable()) {
	                    	client.handleWrite();
	                    }
	                    
	                } catch (Exception e) {
	                	
	                    // Disconnect the user
	                    client.disconnect();
	                    
	                    //Remove from the list..
	                    mClients.remove(client.getUID());
	
	                    //Small Log..
	                    if(mTraceON) {
	                    	MinimaLogger.log("[NIOSERVER] NIOClient:"+client.getUID()+" "+e+" total:"+mClients.size());
	                    }
	                    
	                    //Tell the Network Manager
	                    Message newclient = new Message(NIOManager.NIO_DISCONNECTED)
	                    		.addObject("client", client)
	                    		.addBoolean("reconnect", !client.isIncoming());
	                    
	                    //Tell the manager
	                    mNIOManager.PostMessage(newclient);
	                }
	            }
	        }
	        
	        //Notify..
	        if(mTraceON) {
	        	MinimaLogger.log("[NIOServer] SHUTDOWN");
	        }
            
            //Shut down the socket..
            serversocket.close();
	        
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
	}

	private void addChannel(boolean zIncoming, SocketChannel zSocketChannel) throws IOException {
		// You can get the IPV6  Address (if available) of the connected user like so:
        String ipAddress = zSocketChannel.socket().getInetAddress().getHostAddress();
        
        // We also want this socket to be non-blocking so we don't need to follow the thread-per-socket model
        zSocketChannel.configureBlocking(false);
        zSocketChannel.setOption(StandardSocketOptions.TCP_NODELAY, true);
        zSocketChannel.setOption(StandardSocketOptions.SO_KEEPALIVE, true);

        // Let's also register this socket to our selector:
        SelectionKey selectionkey = zSocketChannel.register(mSelector, SelectionKey.OP_READ | SelectionKey.OP_WRITE);
        
        // Initially - We are only interested in events for reads for our selector.
        selectionkey.interestOps(SelectionKey.OP_READ);

        //What Port..
        InetSocketAddress remote = (InetSocketAddress)zSocketChannel.getRemoteAddress();
        InetSocketAddress local  = (InetSocketAddress )zSocketChannel.getLocalAddress();
        
        int port = remote.getPort();
        
        //Create a new NIOCLient
        NIOClient  nioc = new NIOClient(zIncoming, ipAddress, port, zSocketChannel, selectionkey);
        
        // register with key
        selectionkey.attach(nioc);
        
        //Add to the total list..
        mClients.put(nioc.getUID(), nioc);
        
        //log..
        if(mTraceON) {
        	MinimaLogger.log("[NIOSERVER] NEW NIOClient:"+nioc.getUID()+" total:"+mClients.size());
        }
        
        //Post about it..
        Message newclient = new Message(NIOManager.NIO_NEWCONNECTION).addObject("client", nioc);
        mNIOManager.PostMessage(newclient);
	}
	
}
