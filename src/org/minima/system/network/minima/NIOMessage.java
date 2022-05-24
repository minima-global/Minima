package org.minima.system.network.minima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Greeting;
import org.minima.objects.IBD;
import org.minima.objects.Pulse;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWChecker;
import org.minima.system.network.maxima.MaximaCTRLMessage;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaxTxPoW;
import org.minima.system.network.maxima.message.MaximaPackage;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.p2p.P2PManager;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.ListCheck;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class NIOMessage implements Runnable {

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
	public static final MiniByte MSG_MAXIMA_TXPOW 		= new MiniByte(10);
	
	public static final MiniByte MSG_SINGLE_PING 	= new MiniByte(11);
	public static final MiniByte MSG_SINGLE_PONG 	= new MiniByte(12);
	
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
	
	public NIOMessage(String zClientUID, MiniData zData) {
		mClientUID 	= zClientUID;
		mData 		= zData;
	}
	
	public void setTrace(boolean zTrace) {
		mTrace = zTrace;
	}
	
	@Override
	public void run() {
		//Convert the MiniData into a valid net message
		byte[] data = mData.getBytes();
		
		//Convert..
		ByteArrayInputStream bais 	= new ByteArrayInputStream(data);
		DataInputStream dis 		= new DataInputStream(bais);
		
		//What type of message is it..
		try {
			//What Type..
			MiniByte type = MiniByte.ReadFromStream(dis);
			
			//Output some info
			if(mTrace) {
				MinimaLogger.log("[NIOMessage] uid:"+mClientUID+" type:"+convertMessageType(type)+" size:"+MiniFormat.formatSize(data.length));
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
				
				if(!testcheck || !greetstr.startsWith("0.102")) {
					
					MinimaLogger.log("Greeting with Incompatible Version! "+greet.getVersion().toString()+" .. we are "+GlobalParams.MINIMA_VERSION);
					
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
				//IBD received..
				IBD ibd = IBD.ReadFromStream(dis);
				
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
					MinimaLogger.log("Wrong Block ChainID! "+txpow.getChainID()+" "+txpow.getTxPoWID());
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
				if(blockdiffratio < 0.1) {
					//Block difficulty too low..
					MinimaLogger.log("Received txpow with low block difficulty.. "+blockdiffratio+" "+txpow.getBlockNumber()+" "+txpow.getTxPoWID());
					fullyvalid = false;
				}
				
				if(block.isLessEqual(cascadeblock)) {
					//Block before cascade
					MinimaLogger.log("Received block before cascade.. "+block+" / "+cascadeblock+" difficulty:"+blockdiffratio);
					fullyvalid = true;
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
				
				//Check for mempool coins..
				if(TxPoWChecker.checkMemPoolCoins(txpow)) {
					//Same coins in different transaction - could have been requested by us from branch
					fullyvalid = false;
				}
				
				//Check the MMR - could be in a separate branch
				if(!TxPoWChecker.checkMMR(tip.getMMR(), txpow)) {
					fullyvalid = false;
				}
				
				//How long did all that take..
				long timefinish = System.currentTimeMillis();
				long timediff 	= timefinish - timestart;
				if(timediff > 1000) {
					MinimaLogger.log("Message took a long time ("+timediff+"ms) to process @ "+txpow.getTxPoWID());
					fullyvalid = false;
				}
				
				//Ok - let's add to our database and process..
				Main.getInstance().getTxPoWProcessor().postProcessTxPoW(txpow);
				
				//Since it's OK.. forward the TxPoWID to the rest of the network..
				if(fullyvalid) {
					//Forward to the network
					NIOManager.sendNetworkMessageAll(MSG_TXPOWID, txpow.getTxPoWIDData());
				}
				
				//Check all the Transactions.. if it's a block
				if(txpow.isBlock()) {
					ArrayList<MiniData> txns = txpow.getBlockTransactions();
					for(MiniData txn : txns) {
						exists = MinimaDB.getDB().getTxPoWDB().exists(txn.to0xString());
						if(!exists) {
							//request it.. with a slight delay - as may be in process stack
							NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txpow.getTxPoWIDData());
						}
					}
					
					//Get the parent if we don't have it..
					exists = MinimaDB.getDB().getTxPoWDB().exists(txpow.getParentID().to0xString());
					if(!exists) {
						NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txpow.getParentID());
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
				
				//Now check for intersection
				boolean found = false;
				ArrayList<MiniData> pulsemsg = pulse.getBlockList();
				for(MiniData block : pulsemsg) {
					if(!ListCheck.MiniDataListContains(mylist, block)) {
						TxPoW check = txpdb.getTxPoW(block.to0xString());
						if(check == null) {
							requestlist.add(0, block);
						}else {
							ArrayList<MiniData> txns = check.getBlockTransactions();
							for(MiniData txn : txns) {
								if(!txpdb.exists(txn.to0xString())) {
									requestlist.add(0, txn);
								}
							}
						}
					}else {
						found = true;
						break;
					}
				}
				
				//Did we find a crossover..
				if(found) {
					
					//Request all the blocks.. in the correct order
					for(MiniData block : requestlist) {
						NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, block);
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
				
				//And post it to the Maxima Manager..
				Message maxmsg = new Message(MaximaManager.MAXIMA_CTRLMESSAGE);
				maxmsg.addObject("nioclient", nioclient);
				maxmsg.addObject("maximactrl", msg);
				
				Main.getInstance().getMaxima().PostMessage(maxmsg);
				
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

					//And send
					Message newniomsg = new Message(NIOManager.NIO_INCOMINGMSG);
					newniomsg.addString("uid", mClientUID);
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
				
				pinggreet.getExtraData().put("topblock", tip.getBlockNumber().toString());
				pinggreet.getExtraData().put("tophash", tip.getTxPoW().getTxPoWID());
				
				TxPoWTreeNode tip50 = MinimaDB.getDB().getTxPoWTree().getTip().getParent(50);
				pinggreet.getExtraData().put("50block", tip50.getBlockNumber().toString());
				pinggreet.getExtraData().put("50hash", tip50.getTxPoW().getTxPoWID());
				
				//Send this back to them.. 
				NIOManager.sendNetworkMessage(mClientUID, MSG_SINGLE_PONG, pinggreet);
			
			}else {
				
				//UNKNOWN MESSAGE..
				MinimaLogger.log("Unknown Message type received from "+mClientUID+" type:"+type+" size:"+data.length);
			}
			
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
	}
}
