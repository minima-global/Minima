package org.minima.system.network.minima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Greeting;
import org.minima.objects.IBD;
import org.minima.objects.Pulse;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWChecker;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.network.maxima.MaximaCTRLMessage;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaxTxPoW;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.network.p2p.messages.InetSocketAddressIO;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class NIOMessage implements Runnable {

	/**
	 * What was the last sync block requested..
	 */
	public static Hashtable<String, MiniNumber> mlastSyncReq = new Hashtable<>(); 
	
	/**
	 * Base Message types sent over the network
	 */
	public static final MiniByte MSG_GREETING 		= new MiniByte(0);
	public static final MiniByte MSG_IBD 			= new MiniByte(1); // initial blockchain download
	public static final MiniByte MSG_TXPOWID 		= new MiniByte(2);
	public static final MiniByte MSG_TXPOWREQ 		= new MiniByte(3);
	public static final MiniByte MSG_TXPOW 			= new MiniByte(4);
	public static final MiniByte MSG_GENMESSAGE 	= new MiniByte(5);
	public static final MiniByte MSG_PULSE 			= new MiniByte(6);
	public static final MiniByte MSG_P2P 			= new MiniByte(7);
	public static final MiniByte MSG_PING 			= new MiniByte(8);
	
	public static final MiniByte MSG_MAXIMA_CTRL	= new MiniByte(9);
	public static final MiniByte MSG_MAXIMA_TXPOW 	= new MiniByte(10);
	
	public static final MiniByte MSG_SINGLE_PING 	= new MiniByte(11);
	public static final MiniByte MSG_SINGLE_PONG 	= new MiniByte(12);
	
	public static final MiniByte MSG_IBD_REQ 		= new MiniByte(13);
	public static final MiniByte MSG_IBD_RESP 		= new MiniByte(14);
	
	public static final MiniByte MSG_ARCHIVE_REQ 		= new MiniByte(15);
	public static final MiniByte MSG_ARCHIVE_DATA 		= new MiniByte(16);
	public static final MiniByte MSG_ARCHIVE_SINGLE_REQ = new MiniByte(17);
	
	public static final MiniByte MSG_TXBLOCKID 			= new MiniByte(18);
	public static final MiniByte MSG_TXBLOCKREQ 		= new MiniByte(19);
	public static final MiniByte MSG_TXBLOCK 			= new MiniByte(20);
	public static final MiniByte MSG_TXBLOCKMINE 		= new MiniByte(21);
	
	/**
	 * Helper function that converts to String 
	 */
	public static String convertMessageType(MiniByte zType) {
		if(zType.isEqual(MSG_GREETING)) {
			return "GREETING";
		}else if(zType.isEqual(MSG_IBD)) {
			return "IBD";
		}else if(zType.isEqual(MSG_TXPOWID)) {
			return "TXPOWID";
		}else if(zType.isEqual(MSG_TXPOWREQ)) {
			return "TXPOWREQ";
		}else if(zType.isEqual(MSG_TXPOW)) {
			return "TXPOW";
		}else if(zType.isEqual(MSG_GENMESSAGE)) {
			return "GENMESSAGE";
		}else if(zType.isEqual(MSG_PULSE)) {
			return "PULSE";
		}else if(zType.isEqual(MSG_P2P)) {
			return "P2P";
		}else if(zType.isEqual(MSG_PING)) {
			return "PING";
		}else if(zType.isEqual(MSG_MAXIMA_CTRL)) {
			return "MAXIMA_CTRL";
		}else if(zType.isEqual(MSG_MAXIMA_TXPOW)) {
			return "MAXIMA";
		
		}else if(zType.isEqual(MSG_IBD_REQ)) {
			return "MSG_IBD_REQ";
		}else if(zType.isEqual(MSG_IBD_RESP)) {
			return "MSG_IBD_RESP";
		
		}else if(zType.isEqual(MSG_TXBLOCKID)) {
			return "TXBLOCKID";
		}else if(zType.isEqual(MSG_TXBLOCKREQ)) {
			return "TXBLOCKREQ";
		}else if(zType.isEqual(MSG_TXBLOCK)) {
			return "TXBLOCK";
		}else if(zType.isEqual(MSG_TXBLOCKMINE)) {
			return "TXBLOCKMINE";
		}
		
		return "UNKNOWN";
	}
	
	/**
	 * Who sent the message
	 */
	String mClientUID;
	
	/**
	 * The Data packet sent
	 */
	MiniData mData;
	
	/**
	 * Is Trace on 
	 */
	boolean mTrace = false;
	String mFilter  = "";
 	
	public NIOMessage(String zClientUID, MiniData zData) {
		mClientUID 	= zClientUID;
		mData 		= zData;
	}
	
	public void setTrace(boolean zTrace, String zFilter) {
		mTrace = zTrace;
		mFilter = zFilter;
	}
	
	@Override
	public void run() {
		//Convert the MiniData into a valid net message
		byte[] data = mData.getBytes();
		
		//Are we shutting down
		if(Main.getInstance().isShuttongDownOrRestoring()) {
			//MinimaLogger.log("Minima Shutting down - no new NIOMessages");
			return;
		}
		
		//Convert..
		ByteArrayInputStream bais 	= new ByteArrayInputStream(data);
		DataInputStream dis 		= new DataInputStream(bais);
		
		//What type of message is it..
		try {
			//What Type..
			MiniByte type = MiniByte.ReadFromStream(dis);
			
			//Are we syncing an IBD
			if(Main.getInstance().isSyncIBD()) {
				if(type.isEqual(MSG_TXPOWID) || type.isEqual(MSG_TXBLOCKID) || type.isEqual(MSG_PULSE)) {
					//Ignore until finished..
					MinimaLogger.log("Ignoring NIOmessage during IBD Sync.. type:"+convertMessageType(type));
					return;
				}
			}
			
			//Are we a TxBlock node..
			if(GeneralParams.TXBLOCK_NODE) {
				if( type.isEqual(MSG_TXPOWID)) {
					//Ignore these..
					return;
				}
				
//				//Random message lost
//				if( type.isEqual(MSG_TXBLOCKID)) {
//					if(new Random().nextInt(100) < 80) {
//						MinimaLogger.log("RANDOM LOSE TXBLOCK ID MESSAGE");
//						return;
//					}	
//				}
			}
			
			//Output some info
			String tracemsg = "[NIOMessage] uid:"+mClientUID+" type:"+convertMessageType(type)+" size:"+MiniFormat.formatSize(data.length);
			if(mTrace && tracemsg.contains(mFilter)) {
				MinimaLogger.log(tracemsg,false);
			}
			
			if(true) {
				MinimaLogger.log(tracemsg,false);
			}
			
			//Now find the right message
			if(type.isEqual(MSG_GREETING)) {
				//Get the client.. unless an internal message
				NIOClient nioclient = Main.getInstance().getNIOManager().getNIOServer().getClient(mClientUID);
				if(nioclient == null) {
					MinimaLogger.log(mClientUID+" Error null client on Greeting NIOMessage..");
					return;
				}
				
				//We have received a greeting message
				Greeting greet = Greeting.ReadFromStream(dis);
				
				//What version..
				boolean testcheck = true;
				String greetstr = greet.getVersion().toString();
				if(GeneralParams.TEST_PARAMS && !greetstr.contains("TEST")) {
					testcheck = false;
				}else if(!GeneralParams.TEST_PARAMS && greetstr.contains("TEST")) {
					testcheck = false;
				} 
				
				if(!testcheck || !greetstr.startsWith(GlobalParams.MINIMA_BASE_VERSION)) {
					
					MinimaLogger.log("Greeting with Incompatible Version! "+greet.getVersion().toString()+" .. we are "+GlobalParams.MINIMA_VERSION+" from "+nioclient.getFullAddress()+" incoming:"+nioclient.isIncoming());
					
					//Add to our Invalid Peers list
					P2PFunctions.addInvalidPeer(nioclient.getFullAddress());
					
					//Tell the P2P..
					Message newconn = new Message(P2PFunctions.P2P_NOCONNECT);
					newconn.addObject("client", nioclient);
					newconn.addString("uid", nioclient.getUID());
					Main.getInstance().getNetworkManager().getP2PManager().PostMessage(newconn);
					
					//Are we incoming..
					if(nioclient.isIncoming() && !nioclient.haveSentGreeting()) {
						nioclient.setSentGreeting(true);
						
						//Send them a greeting so THEY disconnect
						Greeting greetout = new Greeting().createGreeting();
						
						//And send it..
						NIOManager.sendNetworkMessage(nioclient.getUID(), NIOMessage.MSG_GREETING, greetout);
						
						//Disconnect in with a small pause
						TimerMessage msg = new TimerMessage(2000,NIOManager.NIO_DISCONNECT);
						msg.addString("uid", mClientUID);
						Main.getInstance().getNIOManager().PostTimerMessage(msg);
					
					}else{
					
						//Just disconnect, you've already sent the Greeting - permanently ( no reconnect )
						Main.getInstance().getNIOManager().disconnect(mClientUID);
					}
					
					return;
				}
				
				//Is this a port foprwrad address
				if(nioclient.getHost().equals("127.0.0.1")) {
					if(greet.getExtraData().containsKey("host")) {
						MinimaLogger.log("Greeting from SSH Port with HOST set : "+greet.getExtraDataValue("host"));
					}
				}
				
				//Get the Host / Port..
				if(greet.getExtraData().containsKey("host")) {
					nioclient.overrideHost(greet.getExtraDataValue("host"));
				}
				if(greet.getExtraData().containsKey("port")) {
					nioclient.setMinimaPort(Integer.parseInt(greet.getExtraDataValue("port")));
				}
				
//				//Is there a maxima mls..
//				if(greet.getExtraData().containsKey("maximamls")) {
//					String mls = greet.getExtraData().getString("maximamls"); 
//					MinimaLogger.log("MLS rec : "+mls);
//					nioclient.setMaximaMLS(mls+"@"+nioclient.getFullAddress());
//				}
				
				//Get the welcome message..
				nioclient.setWelcomeMessage("Minima v"+greet.getVersion());
				nioclient.setValidGreeting(true);
				
				//Tell the P2P..
				Message newconn = new Message(P2PFunctions.P2P_CONNECTED);
				newconn.addString("uid", nioclient.getUID());
				newconn.addBoolean("incoming", nioclient.isIncoming());
				newconn.addObject("client", nioclient);
				Main.getInstance().getNetworkManager().getP2PManager().PostMessage(newconn);
				
				//Tell MAXIMA
				Message maxconn = new Message(MaximaManager.MAXIMA_CONNECTED);
				maxconn.addObject("nioclient", nioclient);
				maxconn.addString("uid", nioclient.getUID());
				maxconn.addBoolean("incoming", nioclient.isIncoming());
				Main.getInstance().getMaxima().PostMessage(maxconn);
				
				//Is this an incoming connection.. send a greeting!
				if(nioclient.isIncoming()) {
					
					//Only Send this ONCE!
					if(!nioclient.haveSentGreeting()) {
						nioclient.setSentGreeting(true);	
						
						//Send a greeting..
						Greeting greetout = new Greeting().createGreeting();
						
						//And send it..
						NIOManager.sendNetworkMessage(nioclient.getUID(), NIOMessage.MSG_GREETING, greetout);
					}
				}
				
//				String welcome = (String) greet.getExtraData().get("welcome");
//				if(welcome != null) {
//					//Tell the NIOServer
//					Main.getInstance().getNIOManager().getNIOServer().setWelcome(mClientUID, welcome);
//				}
				
				//Create an IBD response to that Greeting..
				IBD ibd = new IBD();
				ibd.createIBD(greet);
				
				//Send it
				NIOManager.sendNetworkMessage(mClientUID, MSG_IBD, ibd);
				
			}else if(type.isEqual(MSG_IBD)) {
				
				//Log it..
				MinimaLogger.log("Received IBD size:"+MiniFormat.formatSize(data.length));
				
				//IBD received..
				IBD ibd = IBD.ReadFromStream(dis);
				
				//Log it..
				MinimaLogger.log("Received IBD blocks:"+ibd.getTxBlocks().size());
				
				//Check Seems Valid..
				if(!ibd.checkValidData()) {
					
					//Disconnect
					Main.getInstance().getNIOManager().disconnect(mClientUID);
					
					return;
				}
				
				//Log it..
				MinimaLogger.log("Received IBD is valid..");
				
				//Is it a complete IBD even though we have a cascade
				if(MinimaDB.getDB().getCascade().getLength()>0 && ibd.hasCascadeWithBlocks()) {
					
					boolean heavier = IBD.checkOurChainHeavier(ibd);
					
					if(!heavier) {
						MinimaLogger.log("[!] CONNECTED TO HEAVIER CHAIN.. from "+mClientUID);
					}
					
					return;
				}
								
				//A small message..
				MinimaLogger.log("[+] Connected to the blockchain Initial Block Download received. size:"+MiniFormat.formatSize(data.length)+" blocks:"+ibd.getTxBlocks().size());
				
				//Send to the Processor
				Main.getInstance().getTxPoWProcessor().postProcessIBD(ibd, mClientUID);
				
			}else if(type.isEqual(MSG_TXPOWID)) {
				
				//Read in the txpowid
				MiniData txpowid = MiniData.ReadFromStream(dis);
				
				//Do we have it..
				boolean exists = MinimaDB.getDB().getTxPoWDB().exists(txpowid.to0xString());
				
				//If not request it..
				if(!exists) {
					//request it..
					NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txpowid);
				}
				
			}else if(type.isEqual(MSG_TXPOWREQ)) {
				
				//Read in the txpowid
				MiniData txpowid = MiniData.ReadFromStream(dis);
				
				//Load it.. (but don't keep in RAM)
				TxPoW txpow = MinimaDB.getDB().getTxPoWDB().getTxPoW(txpowid.to0xString());
				 
				//And send it on..
				if(txpow != null) {
					//request it..
					NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOW, txpow);
				}else {
					MinimaLogger.log("TxPoW requested from "+mClientUID+" that we don't have.. "+txpowid.to0xString());
				}
			
			}else if(type.isEqual(MSG_TXPOW)) {
				//Read the TxPoW
				TxPoW txpow = TxPoW.ReadFromStream(dis);
				
				//Do we have it..
				boolean exists = MinimaDB.getDB().getTxPoWDB().exists(txpow.getTxPoWID());
				if(exists) {
					return;
				}
				
				//Now get the current tip details
				TxPoWTreeNode tip 		= MinimaDB.getDB().getTxPoWTree().getTip();
				TxPoWTreeNode cascade 	= MinimaDB.getDB().getTxPoWTree().getRoot();
				
				//Have we got any blocks at all yet..
				if(tip == null) {
					return;
				}
				
				//The block and cascade block
				MiniNumber cascadeblock = cascade.getBlockNumber();
				MiniNumber block 		= txpow.getBlockNumber();
				
				//Check if is a block and within range of our current tip
				BigDecimal tipdec 		= new BigDecimal(tip.getTxPoW().getBlockDifficulty().getDataValue());
				BigDecimal blockdec 	= new BigDecimal(txpow.getBlockDifficulty().getDataValue());
				double blockdiffratio 	= tipdec.divide(blockdec, MathContext.DECIMAL32).doubleValue();
				
				//Start a timer..
				long timestart = System.currentTimeMillis();
				
				//Some BASIC checks that MUST pass..
				boolean disconnectpeer = false;
				
				//NONE of these should fail
				if(!txpow.getChainID().isEqual(TxPoWChecker.CURRENT_NETWORK)) {
					//Check ChainID
					MinimaLogger.log("Wrong Block ChainID! from "+mClientUID+" "+txpow.getChainID()+" "+txpow.getTxPoWID());
					disconnectpeer = true;
				
				}else if(!TxPoWChecker.checkTxPoWBasic(txpow)) {
					//Basic checks for valid TxPoW
					MinimaLogger.log("TxPoW FAILS Basic checks from "+mClientUID+" "+txpow.getTxPoWID());
					disconnectpeer = true;
				
				}else if(!TxPoWChecker.checkSignatures(txpow)) {
					//Check the Signatures
					MinimaLogger.log("Invalid signatures on txpow from "+mClientUID+" "+txpow.getTxPoWID());
					disconnectpeer = true;
				}
				
				//Do we disconnect yet.. 
				if(disconnectpeer) {
					Main.getInstance().getNIOManager().disconnect(mClientUID);
					return;
				}
				
				//More CHECKS.. if ALL these pass will forward otherwise may be a branch txpow that we requested
				boolean fullyvalid = true;
				
				//Interesting info.. check this.. probably a timing issue
				if(txpow.isBlock() && blockdiffratio < 0.01) {
					//Block difficulty too low..
					MinimaLogger.log("Received txpow block with low block difficulty.. "+blockdiffratio+" "+txpow.getBlockNumber()+" "+txpow.getTxPoWID());
					fullyvalid = false;
				}
				
				boolean beforecascade = false;
				if(block.isLessEqual(cascadeblock)) {
					//Block before cascade
					MinimaLogger.log("Received block before cascade.. "+block+" / "+cascadeblock+" difficulty:"+blockdiffratio+" from "+mClientUID);
					fullyvalid 		= false;
					beforecascade 	= true;
				}
				
				//Check the Scripts - could fail.. 
				if(!TxPoWChecker.checkTxPoWScripts(tip.getMMR(), txpow, tip.getTxPoW())) {
					//Monotonic txn MUST pass the script check or is INVALID - since will never pass..
					if(txpow.isMonotonic()) {
						MinimaLogger.log("Error Monotonic TxPoW failed script check from Client:"+mClientUID+" "+txpow.getTxPoWID());
						return;
					}else {
						MinimaLogger.log("NON-Monotonic TxPoW failed script check from Client:"+mClientUID+" "+txpow.getTxPoWID());
					}
					
					//Could be block related
					fullyvalid = false;
				}
				
				//Max time in the future.. 2 hours.. could be OUR clock..
				if(txpow.isBlock()) {
					MiniNumber maxtime = new MiniNumber(System.currentTimeMillis() + (1000 * 60 * 120));
					if(txpow.getTimeMilli().isMore(maxtime)) {
						MinimaLogger.log("TxPoW block received with millitime MORE than 2 hours in future "+new Date(txpow.getTimeMilli().getAsLong())+" "+txpow.getTxPoWID());
						fullyvalid = false;
					}
				}
				
				//Check size
				long size = txpow.getSizeinBytesWithoutBlockTxns();
				if(size > tip.getTxPoW().getMagic().getMaxTxPoWSize().getAsLong()) {
					MinimaLogger.log("TxPoW received size too large.. "+size+" "+txpow.getTxPoWID());
					fullyvalid = false;
				}
				
				//Check for mempool coins..
				if(TxPoWChecker.checkMemPoolCoins(txpow)) {
					//Same coins in different transaction - could have been requested by us from branch
					//MinimaLogger.log("TxPoW with existing mempoolcoins from client : "+mClientUID+" "+txpow.getTxPoWID());
					fullyvalid = false;
				}
				
				//Check the MMR - could be in a separate branch / or a future txn..
				if(!TxPoWChecker.checkMMR(tip.getMMR(), txpow, false)) {
					fullyvalid = false;
				}
				
				//Is the MEMPOOL Full
				if(TxPoWGenerator.isMempoolFull()) {
					
					//What is the Burn
					MiniNumber burn = txpow.getBurn();
					
					//Check the Burn
					if(burn.isLessEqual(TxPoWGenerator.getMinMempoolBurn())) {
						MinimaLogger.log("Received TxPoW with low burn when MEMPOOL full "+burn);
						fullyvalid=false;
					}
				}
				
				//How long did all that take..
				long timefinish = System.currentTimeMillis();
				long timediff 	= timefinish - timestart;
				if(timediff > 20000) {
					MinimaLogger.log("Message took a long time ("+timediff+"ms) to process @ txpowid:"+txpow.getTxPoWID());
					fullyvalid = false;
				}
				
				//Ok - let's add to our database and process..
				Main.getInstance().getTxPoWProcessor().postProcessTxPoW(txpow);
				
				//ONLY if it's FULLY OK.. forward the TxPoWID to the rest of the network..
				if(fullyvalid) {
					//Forward to the network
					NIOManager.sendNetworkMessageAll(MSG_TXPOWID, txpow.getTxPoWIDData());
				}
				
				//Check all the Transactions.. if it's a block
				if(!GeneralParams.TXBLOCK_NODE && txpow.isBlock() && !beforecascade) {
					ArrayList<MiniData> txns = txpow.getBlockTransactions();
					for(MiniData txn : txns) {
						exists = MinimaDB.getDB().getTxPoWDB().exists(txn.to0xString());
						if(!exists) {
							//request it.. with a slight delay - as may be in process stack
							NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txn);
						}
					}
					
					//Scan through all the blocks to see if we have everything..
					TxPoWDB txpdb 				= MinimaDB.getDB().getTxPoWDB();
					TxPoW current 				= txpow;
					
					int counter = 0;
					while(counter<512) {
						
						//What height are we at
						if(current.getBlockNumber().isLessEqual(cascadeblock)) {
							//Far enough
							break;
						}
						
						//What is the parent
						MiniData parentid = current.getParentID();
						
						//Is this onchain already
						TxPoWTreeNode node = TxPoWSearcher.searchChainForTxPoWBlock(parentid);
						if(node!=null) {
							//we'll search the tree next
							break;
						}
						
						//Get the parent
						TxPoW parent = txpdb.getTxPoW(current.getParentID().to0xString());
						if(parent == null) {
							//Send a message for it and break..
							NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, current.getParentID());
							break;
						}
						
						//Check all the transactions in the block..
						ArrayList<MiniData> ptxns = parent.getBlockTransactions();
						for(MiniData txn : ptxns) {
							exists = MinimaDB.getDB().getTxPoWDB().exists(txn.to0xString());
							if(!exists) {
								//request it.. 
								NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txn);
							}
						}
						
						//And make the parent current
						current = parent;
						counter++;
					}
					
					//Now scan the whole tree - unless you already have per block
					TxPoWTreeNode tipblock = MinimaDB.getDB().getTxPoWTree().getTip();
					while(tipblock != null) {
						
						//Do we have all the txns in this block
						boolean haveall = tipblock.checkFullTxns(txpdb);
						
						if(!haveall) {
							ArrayList<MiniData> ptxns = tipblock.getTxPoW().getBlockTransactions();
							for(MiniData txn : ptxns) {
								exists = MinimaDB.getDB().getTxPoWDB().exists(txn.to0xString());
								if(!exists) {
									//request it.. 
									NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txn);
								}
							}
						}
						
						//Get the parent
						tipblock = tipblock.getParent();
					}
				}
				
			}else if(type.isEqual(MSG_GENMESSAGE)) {
				//Read in the message
				MiniString msg = MiniString.ReadFromStream(dis);
				
				//Foe now..
				MinimaLogger.log(mClientUID+":"+msg.toString());
			
			}else if(type.isEqual(MSG_PING)) {
				//Read in a txpow unit.. currently does nothing.. could be 1000's of connections..
				MiniData txpowid = MiniData.ReadFromStream(dis);
			
			}else if(type.isEqual(MSG_P2P)) {
				
				//P2P message..
				MiniString msg = MiniString.ReadFromStream(dis);
				
				//Should not be receiving these..
				if(!GeneralParams.P2P_ENABLED) {
					return;
				}
				
				//Get the Client
				NIOClient nioclient = Main.getInstance().getNIOManager().getNIOServer().getClient(mClientUID);
				if(nioclient == null) {
					MinimaLogger.log(mClientUID+" Error null client on P2P NIOMessage..");
					return;
				}
				NIOClientInfo clientInfo = new NIOClientInfo(nioclient, true);
				
				//Convert to JSON
				JSONObject json = (JSONObject) new JSONParser().parse(msg.toString());
				
				//Have we received a p2p greeting..?
				P2PManager p2pmanager = (P2PManager)Main.getInstance().getNetworkManager().getP2PManager();
				
				if(!nioclient.hasReceivedP2PGreeting()) {
//					MinimaLogger.log("RECEIVED P2P MSG BEFORE GREETING.. DELAYING BY 10s.. "+json.toJSONString());
					
					//Post with delay
					TimerMessage p2p = new TimerMessage(10000, P2PFunctions.P2P_MESSAGE);
					p2p.addString("uid", mClientUID);
					p2p.addObject("message", json);
					p2pmanager.PostTimerMessage(p2p);
					
				}else {
					//Post directly
					Message p2p = new Message(P2PFunctions.P2P_MESSAGE);
					p2p.addString("uid", mClientUID);
					p2p.addObject("message", json);
					p2pmanager.PostMessage(p2p);
					
				}
				
			}else if(type.isEqual(MSG_PULSE)) {
				
				//Read in the Pulse..
				Pulse pulse 	= Pulse.ReadFromStream(dis);
				TxPoWDB txpdb 	= MinimaDB.getDB().getTxPoWDB();
				
				//Now check this list against your ownn..
				ArrayList<MiniData> mylist 		= MinimaDB.getDB().getTxPoWTree().getPulseList();
				ArrayList<MiniData> requestlist = new ArrayList<>();
				
				HashSet<String> fullist = new HashSet<>();
				for(MiniData block : mylist) {
					fullist.add(block.to0xString());
				}
				
				long timestart = System.currentTimeMillis();
				
				//Now check for intersection
				boolean found = false;
				ArrayList<MiniData> pulsemsg = pulse.getBlockList();
				int counter=0;
				for(MiniData block : pulsemsg) {
					
					//Is it one of ours already
					TxPoWTreeNode node = TxPoWSearcher.searchChainForTxPoWBlock(block);
					if(node!=null) {
						found = true;
						//We search this every block
						break;
					}
					
					counter++;
					String blockstr = block.to0xString();
					
					//Do we have the block
					TxPoW check = txpdb.getTxPoW(blockstr);
					if(check == null) {
						//Ask for it
						requestlist.add(0, block);
					}else {
						if(!GeneralParams.TXBLOCK_NODE) {
							//Check all the transactions..
							ArrayList<MiniData> txns = check.getBlockTransactions();
							for(MiniData txn : txns) {
								if(!txpdb.exists(txn.to0xString())) {
									requestlist.add(0, txn);
								}
							}
						}
					}
					
					//Is there a crossover
					if(fullist.contains(blockstr)) {
						found = true;
						//break;
					}
				}
				
				long timediff = System.currentTimeMillis() - timestart;
				if(counter>0) {
					MinimaLogger.log("PULSE("+counter+"/"+pulsemsg.size()+") from:"+mClientUID+" TIME:"+timediff+"ms req:"+requestlist.size()+" crossover:"+found);
				}
				
				//Did we find a crossover..
				if(found) {
					if(!GeneralParams.TXBLOCK_NODE) {
					
						//Request all the blocks.. in the correct order
						for(MiniData block : requestlist) {
							NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, block);
						}
					}else {
						
						//Request all the blocks.. in the correct order
						for(MiniData block : requestlist) {
							NIOManager.sendNetworkMessage(mClientUID, MSG_TXBLOCKREQ, block);
						}
					}
					
				}else{
					NIOClient nioclient = Main.getInstance().getNIOManager().getNIOServer().getClient(mClientUID);
					int port = nioclient.getPort();
					if (nioclient.getMinimaPort() == -1){
						port = nioclient.getMinimaPort();
					}
					
					//Hmm something funny..
					MinimaLogger.log("[!] No Crossover found whilst syncing with new node. They are on a different chain. Please check you are on the correct chain.. disconnecting from "+ nioclient.getHost() + ":" + port);
					Main.getInstance().getNIOManager().disconnect(mClientUID);
				}
				
			}else if(type.isEqual(MSG_MAXIMA_CTRL)) {
				
				//Make sure acceptable length
				if(mData.getLength() > 65535) {
					MinimaLogger.log("Maxima CTRL message too Large! from "+mClientUID);
					return;
				}
				
				//Get the message
				MaximaCTRLMessage msg = MaximaCTRLMessage.ReadFromStream(dis);
				
				//Get the client
				NIOClient nioclient = Main.getInstance().getNIOManager().getNIOServer().getClient(mClientUID);
				
				//Check not null
				if(nioclient != null) {
					
					//And post it to the Maxima Manager..
					Message maxmsg = new Message(MaximaManager.MAXIMA_CTRLMESSAGE);
					maxmsg.addObject("nioclient", nioclient);
					maxmsg.addObject("maximactrl", msg);
					
					Main.getInstance().getMaxima().PostMessage(maxmsg);
				}
				
			}else if(type.isEqual(MSG_MAXIMA_TXPOW)) {
				
				//Convert to a MaxTxPOW
				MaxTxPoW mxtxpow = MaxTxPoW.ReadFromStream(dis);
				
				//Check the TxPoW unit is correct
				if(!mxtxpow.checkValidTxPoW()) {
					MinimaLogger.log("Invalid Maxima message : Incorrect TxPoW Hash from "+mClientUID);

					//Tell them it's a fail!
					NIOManager.sendNetworkMessage(mClientUID, MSG_PING, MaximaManager.MAXIMA_WRONGHASH);
					
					return;
				}
								
				//How large is the Maxima Package
				MiniData mp = MiniData.getMiniDataVersion(mxtxpow.getMaximaPackage());
				
				//Make sure acceptable length - 256K
				if(mp.getLength() > 262144) {
					MinimaLogger.log("Maxima message too Large! from "+mClientUID+" "+MiniFormat.formatSize(mp.getLength()));
					
					//Tell them it's a fail!
					NIOManager.sendNetworkMessage(mClientUID, MSG_PING, MaximaManager.MAXIMA_TOOBIG);
					
					return;
				}
				
				//Get the client
				NIOClient nioclient = Main.getInstance().getNIOManager().getNIOServer().getClient(mClientUID);
				
				//Are we still connected..
				if(nioclient == null) {
					//Already disconnected
					return;
				}
				
				//And send it on to Maxima..
				Message maxmsg = new Message(MaximaManager.MAXIMA_RECMESSAGE);
				maxmsg.addObject("nioclient", nioclient);
				maxmsg.addObject("maxtxpow", mxtxpow);
				
				//Send to the Maxima Manager
				Main.getInstance().getMaxima().PostMessage(maxmsg);
				
				//Is it a block or a transaction..
				TxPoW txpow = mxtxpow.getTxPoW();
				if(txpow.isTransaction() || txpow.isBlock()) {
					
					//And Now post the TxPoW on the stack..
					MiniData niodata = NIOManager.createNIOMessage(NIOMessage.MSG_TXPOW, txpow);

					//And post on out stack
					Message newniomsg = new Message(NIOManager.NIO_INCOMINGMSG);
					newniomsg.addString("uid", "0x01");
					newniomsg.addObject("data", niodata);

					//Post to the NIOManager - which will check it and forward if correct
					Main.getInstance().getNetworkManager().getNIOManager().PostMessage(newniomsg);
				}
				
			}else if(type.isEqual(MSG_SINGLE_PING)) {
				
				//Get the Data Object..
				MiniData datapacket = MiniData.ReadFromStream(dis);
				
				//Now send some useful info back 
				Greeting pinggreet = new Greeting();
				pinggreet.getExtraData().put("welcome", "hi there!");
				
				TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				
				if(tip != null) {
					pinggreet.getExtraData().put("topblock", tip.getBlockNumber().toString());
					pinggreet.getExtraData().put("tophash", tip.getTxPoW().getTxPoWID());
					
					TxPoWTreeNode tip50 = tip.getParent(100);
					pinggreet.getExtraData().put("50block", tip50.getBlockNumber().toString());
					pinggreet.getExtraData().put("50hash", tip50.getTxPoW().getTxPoWID());
				}else {
					pinggreet.getExtraData().put("topblock", "0");
					pinggreet.getExtraData().put("tophash", "0x00");
					pinggreet.getExtraData().put("50block", "0");
					pinggreet.getExtraData().put("50hash", "0x00");
				}
				
				//Is the P2P Enable..
				if(GeneralParams.P2P_ENABLED) {
					
					//Get the peers list
					P2PManager p2PManager 	= (P2PManager) Main.getInstance().getNetworkManager().getP2PManager();
					JSONArray peers 		= InetSocketAddressIO.addressesListToJSONArray(p2PManager.getPeersCopy());
					pinggreet.getExtraData().put("peers-list", peers);
					pinggreet.getExtraData().put("clients", p2PManager.getClients());
					
				}else {
					//No peers..
					pinggreet.getExtraData().put("peers-list", new JSONArray());
					pinggreet.getExtraData().put("clients", 0);
				}
				
				//Send this back to them.. 
				NIOManager.sendNetworkMessage(mClientUID, MSG_SINGLE_PONG, pinggreet);
			
			}else if(type.isEqual(MSG_IBD_REQ)) {
				
				//Get the Hash of the Block
				TxPoW lastblock = TxPoW.ReadFromStream(dis);
				
				//And post this on..
				//MinimaLogger.log("[+] Received Sync IBD Request from "+mClientUID+" @ "+lastblock.getBlockNumber());
				
				//Are we limiting this..
				if(GeneralParams.ARCHIVESYNC_LIMIT_BANDWIDTH) {
					
					//How much have we used..
					long total 		= Main.getInstance().getNIOManager().getTrafficListener().getTotalWrite();
					String current 	= MiniFormat.formatSize(total);
					
					if(total > NIOManager.MAX_ARCHIVE_WRITE) {
						MinimaLogger.log("MAX Bandwith used already ("+current+") - no more archive sync for 24hours..");
						return;
					}
				}
				
				//What was the last request from this user
				MiniNumber lastreq = mlastSyncReq.get(mClientUID);
				if(lastreq != null) {
					if(lastreq.isEqual(lastblock.getBlockNumber())) {
						MinimaLogger.log("[+] Received SAME Sync IBD Request from "+mClientUID+" @ "+lastblock.getBlockNumber()+" IGNORING");
						return;
					}
				}
				mlastSyncReq.put(mClientUID, lastblock.getBlockNumber());
				
				//Create an IBD of the blocks we hjave before this one..
				IBD syncibd = new IBD();
				syncibd.createSyncIBD(lastblock);
				
				//And send it..
				NIOManager.sendNetworkMessage(mClientUID, MSG_IBD_RESP, syncibd);
				
			}else if(type.isEqual(MSG_IBD_RESP)) {
				
				//Load the IBD..
				IBD syncibd = IBD.ReadFromStream(dis);
				
				//Are there any blocks..
				if(syncibd.getTxBlocks().size() > 0) {
				
					//Top block
					MiniNumber top = syncibd.getTxBlocks().get(0).getTxPoW().getBlockNumber(); 
					
					//And post this on..
					MinimaLogger.log("[+] Received Sync IBD. size:"+MiniFormat.formatSize(data.length)+" blocks:"+syncibd.getTxBlocks().size()+" top:"+top);
					
					//Send to the Processor
					Main.getInstance().getTxPoWProcessor().postProcessSyncIBD(syncibd, mClientUID);
				}
			
			}else if(type.isEqual(MSG_ARCHIVE_REQ)) {
				
				//What block are we starting from..
				MiniNumber firstblock 	= MiniNumber.ReadFromStream(dis);
				
				IBD ibd = new IBD();
				
				//Is this a test connect
				if(firstblock.isEqual(MiniNumber.MINUSONE)) {
					MinimaLogger.log("Archive IBD connection test..");
					
					//Send it.. empty just testing the connection
					NIOManager.sendNetworkMessage(mClientUID, MSG_ARCHIVE_DATA, ibd);
					
				}else {
					MinimaLogger.log("Archive IBD request start @ "+firstblock);
					ibd.createArchiveIBD(firstblock);
					
					//Send it..
					NIOManager.sendNetworkMessage(mClientUID, MSG_ARCHIVE_DATA, ibd);
				}
			
			}else if(type.isEqual(MSG_ARCHIVE_SINGLE_REQ)) {
				
//				//Do we support archive data
//				if(!MinimaDB.getDB().getArchive().isStoreMySQL()) {
//					MinimaLogger.log("Archive single request we do not saupport.. from "+mClientUID);
//					return;
//				}
//				
//				//What block do they want
//				MiniNumber blocknum 	= MiniNumber.ReadFromStream(dis);
//				
//				//Get that block
//				TxBlock block = MinimaDB.getDB().getArchive().getMySQLCOnnect().loadBlockFromNum(blocknum.getAsLong());
//				if(block != null) {
//					//Send it to them..
//					NIOManager.sendNetworkMessage(mClientUID, MSG_ARCHIVE_DATA, block);
//				}
				
			}else if(type.isEqual(MSG_ARCHIVE_DATA)) {
			
				//Messages are handled in the archive command - not here yet
				MinimaLogger.log("Received MSG_ARCHIVE_DATA msg.. ignoring.. from "+mClientUID);
				
//				//It's an IBD structure
//				IBD archibd = IBD.ReadFromStream(dis);
//				
//				//Send this to the main processor
//				Main.getInstance().getTxPoWProcessor().postProcessArchiveIBD(archibd, mClientUID);
								
			
			}else if(type.isEqual(MSG_TXBLOCKID)) {
				
				//Are we running this type of node..
				if(!GeneralParams.TXBLOCK_NODE) {
					return;
				}
				
				//Read in the txpowid
				MiniData txpowid = MiniData.ReadFromStream(dis);
				String txid 	 = txpowid.to0xString();
				
				//Do we have it..
				TxBlock txb = MinimaDB.getDB().getTxBlockDB().findTxBlock(txid);
				
				//Do we have it in TxPoWTree
				if(txb == null) {
					TxPoWTreeNode node = MinimaDB.getDB().getTxPoWTree().findNode(txid);
					if(node!=null) {
						txb = node.getTxBlock();
					}
				}
				
				//If not request it..
				if(txb==null) {
					//request it..
					NIOManager.sendNetworkMessage(mClientUID, MSG_TXBLOCKREQ, txpowid);
				} 
			
			}else if(type.isEqual(MSG_TXBLOCKREQ)) {
				
				//Read in the txpowid
				MiniData txpowid 	= MiniData.ReadFromStream(dis);
				String txid 		= txpowid.to0xString();
				
				//Do we have it.. in RAM DB
				TxBlock txb = MinimaDB.getDB().getTxBlockDB().findTxBlock(txid);
				
				//Do we have it in TxPoWTree
				if(txb == null) {
					TxPoWTreeNode node = MinimaDB.getDB().getTxPoWTree().findNode(txid);
					if(node!=null) {
						txb = node.getTxBlock();
					}
				}

				//Do we have it in Archive..
				if(txb == null) {
					txb = MinimaDB.getDB().getArchive().loadBlock(txid);
				}
				
				//Send it to them
				if(txb!=null) {
					NIOManager.sendNetworkMessage(mClientUID, MSG_TXBLOCK, txb);
				}else {
					MinimaLogger.log("Request for TxBlock we don't have : "+txpowid.to0xString());
				}
			
			}else if(type.isEqual(MSG_TXBLOCK)) {
				
				//Are we running this type of node..
				if(!GeneralParams.TXBLOCK_NODE) {
					return;
				}
				
				//Get the TxBlock
				TxBlock txblock = TxBlock.ReadFromStream(dis);
				
				//And process..
				Main.getInstance().getTxPoWProcessor().postProcessTxBlock(txblock);
				
				//Is the parent above the cascade.
				TxPoWTreeNode cascade = MinimaDB.getDB().getTxPoWTree().getRoot();
				if(txblock.getTxPoW().getBlockNumber().isMoreEqual(cascade.getBlockNumber())) {
				
					//Do we have the parent..
					MiniData parent = txblock.getTxPoW().getParentID();
					String txid 	= parent.to0xString();
				
					//Do we have it.. in RAM DB
					TxBlock txb = MinimaDB.getDB().getTxBlockDB().findTxBlock(txid);
					
					//Do we have it in TxPoWTree
					if(txb == null) {
						TxPoWTreeNode node = MinimaDB.getDB().getTxPoWTree().findNode(txid);
						if(node!=null) {
							txb = node.getTxBlock();
						}
					}
					
					//If not request it..
					if(txb==null) {
						//request it..
						MinimaLogger.log("Request Parent TxBlock.. @ "+txblock.getTxPoW().getBlockNumber());
						NIOManager.sendNetworkMessage(mClientUID, MSG_TXBLOCKREQ, parent);
					}
				}
				
			}else if(type.isEqual(MSG_TXBLOCKMINE)) {
				
				//Are we running this type of node..
				if(!GeneralParams.TXBLOCK_NODE) {
					return;
				}
				
				//Get the unmined txpow
				TxPoW txp = TxPoW.ReadFromStream(dis);
				
				//Reset the RandomID - so everyone mines a different block
				txp.getTxBody().resetRandomPRNG();
				
				//Mine it..
				Main.getInstance().getTxPoWMiner().mineTxPoWAsync(txp);
				
			}else {
				
				//UNKNOWN MESSAGE..
				MinimaLogger.log("Unknown Message type received from "+mClientUID+" type:"+type+" size:"+data.length);
			}
			
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
	}
}
