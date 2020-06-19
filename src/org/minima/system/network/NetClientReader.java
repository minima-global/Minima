package org.minima.system.network;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.net.SocketException;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.Greeting;
import org.minima.objects.greet.HashNumber;
import org.minima.objects.greet.TxPoWList;
import org.minima.system.backup.SyncPackage;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.ConsensusNet;
import org.minima.system.brains.ConsensusPrint;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ProtocolException;
import org.minima.utils.messages.Message;

public class NetClientReader implements Runnable {
	
	/**
	 * Maximum Message sizes..
	 */
	
	//20 MB MAX INTRO / Greeting / and TxPoW List
	public static final int MAX_INTRO = 1024 * 1000 * 20;
	
	//20 KB MAX MESSAGE
	public static final int MAX_TXPOW = 1024 * 20;
			
	//The Length of a TxPoWID message 64 + 4 byte int
	public static final int TXPOWID_LEN = Crypto.MINIMA_DEFAULT_MAX_HASH_LENGTH + 4;
	
	//The Max length of the greeting message..
	public static final int MAX_TXPOW_LIST_REQ = 128;
		
	/**
	 * If the peers don;t intersect a complete Sync Package is sent in this
	 */
	public static final MiniByte NETMESSAGE_INTRO			= new MiniByte(0);
	
	/**
	 * The peer has a new TXPOW. This is the ID. If the peer 
	 * doesn't have it already he will request it 
	 */
	public static final MiniByte NETMESSAGE_TXPOWID			= new MiniByte(1);
	
	/**
	 * Request the full details of a TXPOW. Either the complete 
	 * txpow, or just the MMR proofs and updates.
	 */
	public static final MiniByte NETMESSAGE_TXPOW_REQUEST	= new MiniByte(2);
	
	/**
	 * Complete TXPOW. Only sent if the peer has requested it
	 */
	public static final MiniByte NETMESSAGE_TXPOW			= new MiniByte(3);
	
	/**
	 * Request the full details of a list of TxPow. You only send the top TxPoW 
	 * and a number for the parents required
	 */
	public static final MiniByte NETMESSAGE_TXPOWLIST_REQUEST = new MiniByte(4);
	
	/**
	 * A list of TxPoW details
	 */
	public static final MiniByte NETMESSAGE_TXPOWLIST	      = new MiniByte(5);
	
	/**
	 * Greeting message that tells what Net Protocol this peer speaks, and a complete block chain header list. Any Blocks 
	 * the peer doesn't have he can request. Both peers send this to each other when they connect.
	 */
	public static final MiniByte NETMESSAGE_GREETING		= new MiniByte(6);
	
	/**
	 * PING PONG
	 */
	public static final MiniByte NETMESSAGE_PING		    = new MiniByte(7);
	
	/**
	 * Netclient owner
	 */
	NetClient 		mNetClient;
	
	/**
	 * Constructor
	 * 
	 * @param zNetClient
	 */
	public NetClientReader(NetClient zNetClient) {
		mNetClient 		= zNetClient;
	}

	@Override
	public void run() {
		try {
			//Create an input stream
			DataInputStream mInput = new DataInputStream(new BufferedInputStream(mNetClient.getSocket().getInputStream()));
			
			//The message type
			MiniByte msgtype = new MiniByte();
			
			//The Consensus
			ConsensusHandler consensus = mNetClient.getNetworkHandler().getMainHandler().getConsensusHandler();
			
			while(true) {
				//What message type
				msgtype = MiniByte.ReadFromStream(mInput);
				
				//What length..
				int len = MiniNumber.ReadFromStream(mInput).getAsInt();
				
				//Check within acceptable parameters - this should be set in TxPoW header.. for now fixed
				if( msgtype.isEqual(NETMESSAGE_TXPOWID) || 
					msgtype.isEqual(NETMESSAGE_TXPOW_REQUEST)) {
					if(len > TXPOWID_LEN) {
						throw new ProtocolException("Receive Invalid Message length for TXPOWID type:"+msgtype+" len:"+len);
					}
				}else if(msgtype.isEqual(NETMESSAGE_INTRO) || 
						 msgtype.isEqual(NETMESSAGE_GREETING) || 
						 msgtype.isEqual(NETMESSAGE_TXPOWLIST)) {
					if(len > MAX_INTRO) {
						throw new ProtocolException("Receive Invalid Message length for TXPOW_INTRO type:"+msgtype+" len:"+len);
					}
				}else if(msgtype.isEqual(NETMESSAGE_TXPOW)) {
					if(len > MAX_TXPOW) {
						throw new ProtocolException("Receive Invalid Message length for TXPOW type:"+msgtype+" len:"+len);
					}
				}else if(msgtype.isEqual(NETMESSAGE_TXPOWLIST_REQUEST)) {
					if(len > MAX_TXPOW_LIST_REQ) {
						throw new ProtocolException("Receive Invalid Message length for MAX_TXPOW_LIST_REQ type:"+msgtype+" len:"+len);
					}
				}else if(msgtype.isEqual(NETMESSAGE_PING)) {
					if(len > 1) {
						throw new ProtocolException("Receive Invalid Message length for PING message type:"+msgtype+" len:"+len);
					}
				}
			
				//Now read in the full message
				MiniData fullmsg = MiniData.ReadFromStream(mInput, len);
				
				//Now convert to an 
				ByteArrayInputStream bais   = new ByteArrayInputStream(fullmsg.getData());
				DataInputStream inputstream = new DataInputStream(bais);
				
				//New Message received
				Message rec = new Message(ConsensusNet.CONSENSUS_PREFIX+"NET_MESSAGE_"+msgtype);
				
				//Always add the client
				rec.addObject("netclient", mNetClient);
				
				//What kind of message is it..
				if(msgtype.isEqual(NETMESSAGE_INTRO)) {
					//tell us how big the sync was..
					MinimaLogger.log("Initial Sync Message : "+ConsensusPrint.formatSize(len));
					
					//Read in the SyncPackage
					SyncPackage sp = new SyncPackage();
					sp.readDataStream(inputstream);
					
					//Add and send
					rec.addObject("sync", sp);
					
				}else if(msgtype.isEqual(NETMESSAGE_TXPOWID)) {
					//Peer now has this TXPOW - if you don't you can request the full version
					MiniData hash  = MiniData.ReadFromStream(inputstream);
					
					//Add this ID
					rec.addObject("txpowid", hash);
					
				}else if(msgtype.isEqual(NETMESSAGE_TXPOW)) {
					//A complete TxPOW
					TxPoW tx = new TxPoW();
					tx.readDataStream(inputstream);
					
					//Add this ID
					rec.addObject("txpow", tx);
					
				}else if(msgtype.isEqual(NETMESSAGE_TXPOW_REQUEST)) {
					//Requesting a TxPOW
					MiniData hash  = MiniData.ReadFromStream(inputstream);
					
					//Add this ID
					rec.addObject("txpowid", hash);
				
				}else if(msgtype.isEqual(NETMESSAGE_GREETING)) {
					//Get the Greeting
					Greeting greet = Greeting.ReadFromStream(inputstream);
					
					//Add this ID
					rec.addObject("greeting", greet);
				
				}else if(msgtype.isEqual(NETMESSAGE_TXPOWLIST_REQUEST)) {
					//A list of Required TxPoW messages..
					HashNumber hashnum = HashNumber.ReadFromStream(inputstream);
					
					//Add this ID
					rec.addObject("hashnumber", hashnum);
					
				}else if(msgtype.isEqual(NETMESSAGE_TXPOWLIST)) {
					//tell us how big the sync was..
					MinimaLogger.log("Initial Sync Message : "+ConsensusPrint.formatSize(len));
					
					TxPoWList txplist = new TxPoWList();
					txplist.readDataStream(inputstream);
					
					//Add this ID
					rec.addObject("txpowlist", txplist);
					
				}else if(msgtype.isEqual(NETMESSAGE_PING)) {
					MiniByte mb = MiniByte.ReadFromStream(inputstream);
					
					//Add this ID
					rec.addObject("sent", mb);
					
				}else {
					throw new Exception("Invalid message on network : "+rec);
				}
				
				//Check there is nothing left..
				int left = inputstream.available();
				if(inputstream.available()>0) {
					//Something gone wrong..
					throw new ProtocolException("Data left in inputstream when reading.. "+left);
				}
				
				//Clean up..
				inputstream.close();
				bais.close();
				
				//Post it..
				consensus.PostMessage(rec);
			}
		
		}catch(SocketException exc) {
			//Network error.. reset and reconnect..
		}catch(IOException exc) {
			//Network error.. reset and reconnect..
//			MinimaLogger.log("IOEXC.. "+exc);
//			exc.printStackTrace();
		}catch(ProtocolException exc) {
			MinimaLogger.log("PROTOCOL ERROR.. "+exc);
			exc.printStackTrace();
			
		}catch(OutOfMemoryError exc) {
			MinimaLogger.log("MEMORY ERROR.. "+exc);
			exc.printStackTrace();
			
			//DRASTIC ACTION.. Use ONLY if bash script in place to restart on Exit
			//System.exit(99);
			
		}catch(Exception exc) {
			//General Exception	
			MinimaLogger.log("NETCLIENTREADER ERROR.. "+exc);
			exc.printStackTrace();
		
		}
		
		//Tell the network Handler
		mNetClient.getNetworkHandler().PostMessage(new Message(NetworkHandler.NETWORK_CLIENTERROR).addObject("client", mNetClient));
	}
}

