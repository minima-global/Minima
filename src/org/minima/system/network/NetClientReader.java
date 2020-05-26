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
import org.minima.system.backup.SyncPackage;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.ConsensusNet;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ProtocolException;
import org.minima.utils.messages.Message;

public class NetClientReader implements Runnable {
	
	/**
	 * Temporary Maximum Message sizes..
	 */
	
	//10 MB MAX INTRO
	public static final int MAX_INTRO = 1024 * 1000 * 10;
	
	//20 KB MAX MESSAGE
	public static final int MAX_TXPOW = 1024 * 20;
			
	//The Length of a TxPoWID message 64 +4 byte int
	public static final int TXPOWID_LEN = Crypto.MINIMA_DEFAULT_MAX_HASH_LENGTH + 4;
	
	/**
	 * Greeting message that tells what Net Protocol this peer speaks, and a complete block chain header list. Any Blocks 
	 * the peer doesn't have he can request. Both peers send this to each other when they connect.
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
				if(msgtype.isEqual(NETMESSAGE_TXPOWID) || msgtype.isEqual(NETMESSAGE_TXPOW_REQUEST)) {
					if(len != TXPOWID_LEN) {
						throw new ProtocolException("Receive Invalid Message length for TXPOWID "+len);
					}
				}else if(msgtype.isEqual(NETMESSAGE_INTRO)) {
					if(len > MAX_INTRO) {
						throw new ProtocolException("Receive Invalid Message length for TXPOW_INTRO "+len);
					}
				}else if(msgtype.isEqual(NETMESSAGE_TXPOW)) {
					if(len > MAX_TXPOW) {
						throw new ProtocolException("Receive Invalid Message length for TXPOW "+len);
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
					//Read in the SyncPackage
					SyncPackage sp = new SyncPackage();
					sp.readDataStream(inputstream);
					
					//Add and send
					rec.addObject("sync", sp);
					
				}else if(msgtype.isEqual(NETMESSAGE_TXPOWID)) {
					//Peer now has this TXPOW - if you don't you can request the full version
					MiniData hash  = new MiniData();
					hash.readDataStream(inputstream);
					
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
					MiniData hash  = new MiniData();
					hash.readDataStream(inputstream);
					
					//Add this ID
					rec.addObject("txpowid", hash);
				
				}else {
					throw new ProtocolException("Invalid message on network : "+rec);
				}
				
				//Check there is nothing left..
				int left = inputstream.available();
				if(inputstream.available()>0) {
					//Something gone wrong..
					throw new ProtocolException("Data left in inputstream when reading.. "+left);
				}
				
				//Clean up..
				try {
					inputstream.close();
					bais.close();
				}catch(Exception exc) {}
				
				//Post it..
				consensus.PostMessage(rec);
			}
		
		}catch(SocketException exc) {
			//Network error.. reset and reconnect..
		}catch(IOException exc) {
			//Network error.. reset and reconnect..
		}catch(ProtocolException exc) {
			//Full Stack
			exc.printStackTrace();
			
			//This more serious error.. print it..
			MinimaLogger.log("NetClientReader closed UID "+mNetClient.getUID()+" exc:"+exc);
		
		}catch(Exception exc) {
			//General Exception	
		}
		
		//Tell the network Handler
		mNetClient.getNetworkHandler().PostMessage(new Message(NetworkHandler.NETWORK_CLIENTERROR).addObject("client", mNetClient));
	}
}

