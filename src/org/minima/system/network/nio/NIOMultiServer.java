package org.minima.system.network.nio;

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class NIOMultiServer extends MessageProcessor {

	public static final String NIOSERVER_INIT    = "NIOSERVER_INIT";
	public static final String NIOSERVER_CLOSE   = "NIOSERVER_CLOSE";
	
	public static final String NIOSERVER_ACCEPT  = "NIOSERVER_ACCEPT";
	public static final String NIOSERVER_READ    = "NIOSERVER_READ";
	public static final String NIOSERVER_WRITE   = "NIOSERVER_WRITE";
	
	
    private ServerSocketChannel mSockChan;
    
    private Selector     mSelector;
    
    private ByteBuffer mReadBuffer;
    private ByteBuffer mWriteBuffer;
   
    ArrayList<NIOClient> mClients;
    
	public NIOMultiServer() {
		super("NIOServer");
		
		mClients = new ArrayList<>();
		
		mReadBuffer  = ByteBuffer.allocateDirect(16384);
		mWriteBuffer = ByteBuffer.allocateDirect(16384);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(NIOSERVER_INIT)) {
			
			// open a non-blocking server socket channel
		    mSockChan = ServerSocketChannel.open();
		    mSockChan.configureBlocking(false);

		    // bind to localhost on designated port
		    InetAddress addr = InetAddress.getLocalHost();
		    mSockChan.socket().bind(new InetSocketAddress(addr, 9000));

		    // get a selector for multiplexing the client channels
		    mSelector = Selector.open();
			
		    //Start the ball rolling
//			TimerMessage(100, NIOSERVER_ACCEPT);
			
		}else if(zMessage.isMessageType(NIOSERVER_CLOSE)) {
			
			//Close all the clients..
			
			//Close the server
			mSockChan.close();
			
			
		}else if(zMessage.isMessageType(NIOSERVER_ACCEPT)) {
			
			//Check for new connections..
			SocketChannel clientChannel;
			
	    	// since this is non-blocking, this will return immediately regardless of whether there is a connection available
		    while ((clientChannel = mSockChan.accept()) != null) {
		    	//Non blocking
		    	clientChannel.configureBlocking( false);
			    
		    	//Create a new NIO client..
		    	NIOClient nio = new NIOClient(clientChannel);
			    
		    	//Register for reading - and add the client as the attachment to the selection key
		    	SelectionKey readKey = clientChannel.register(mSelector, SelectionKey.OP_READ, nio);
			    
		    	//Add to our list..
		    	mClients.add(nio);
		    }
			
			//Set the next one..
			PostMessage(NIOSERVER_READ);
		
		}else if(zMessage.isMessageType(NIOSERVER_READ)) {
			
			// non-blocking select, returns immediately regardless of how many keys are ready
		    mSelector.selectNow();
		    
		    // run through the keys and process
		    Iterator keys = mSelector.selectedKeys().iterator();
		    while (keys.hasNext()) {
				SelectionKey key = (SelectionKey) keys.next();
				keys.remove();
				
				//Get the Channel
				SocketChannel channel = (SocketChannel) key.channel();
				
				//Get the NIOClient..
				NIOClient nio = (NIOClient) key.attachment();
				
				//Get ready to read..
				mReadBuffer.clear();
			
				// read from the channel into our buffer
				long nbytes = channel.read(mReadBuffer);
			
				if(nbytes == -1) {
					//Channel Close..
					
				}else {
					//Send to the client..
					mReadBuffer.flip();
					
					//Add to the NIO..
					//
					
					//Clear the Buffer
					mReadBuffer.clear();
				}
			}
		    
			//Set the next one..
//			TimerMessage(100, NIOSERVER_ACCEPT);
		
		}else if(zMessage.isMessageType(NIOSERVER_WRITE)) {
			//Get the Client..
			NIOClient cient = (NIOClient) zMessage.getObject("nioclient");
			
			byte[] data = null;
			
			//Send a message to a client..	
			mWriteBuffer.clear();
			mWriteBuffer.put(data);
			mWriteBuffer.flip();
			
			
			
		}
		
		
	}

}
