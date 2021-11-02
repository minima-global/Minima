package org.minima.system.network.minima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.objects.Greeting;
import org.minima.objects.IBD;
import org.minima.objects.Pulse;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWChecker;
import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.params.GeneralParams;
import org.minima.utils.ListCheck;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;

public class NIOMessage implements Runnable {

	/**
	 * Base Message types sent over the network
	 */
	public static final MiniByte MSG_GREETING 	= new MiniByte(0);
	public static final MiniByte MSG_IBD 		= new MiniByte(1);
	public static final MiniByte MSG_TXPOWID 	= new MiniByte(2);
	public static final MiniByte MSG_TXPOWREQ 	= new MiniByte(3);
	public static final MiniByte MSG_TXPOW 		= new MiniByte(4);
	public static final MiniByte MSG_GENMESSAGE = new MiniByte(5);
	public static final MiniByte MSG_PULSE 		= new MiniByte(6);
	public static final MiniByte MSG_P2P 		= new MiniByte(7);
	
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
				//We have received a greeting message
				Greeting greet = Greeting.ReadFromStream(dis);
				
				//Message
				MinimaLogger.log("Greeting received from "+mClientUID+" "+MiniFormat.formatSize(mData.getLength()));
				
				//Get the welcome message..
				String welcome = (String) greet.getExtraData().get("welcome");
				if(welcome != null) {
					//Tell the NIOServer
					Main.getInstance().getNIOManager().getNIOServer().setWelcome(mClientUID, welcome);
				}
				
				//Create an IBD response to that Greeting..
				IBD ibd = new IBD();
				ibd.createIBD(greet);
				
				//Send it
				NIOManager.sendNetworkMessage(mClientUID, MSG_IBD, ibd);
				
			}else if(type.isEqual(MSG_IBD)) {
				//IBD received..
				IBD ibd = IBD.ReadFromStream(dis);
				
				//Message
				MinimaLogger.log("IBD received from "+mClientUID+" "+MiniFormat.formatSize(mData.getLength()));
				
				//Do some checking!
//				//Sort the Sync blocks - low to high - they should be in the correct order but just in case..
//				Collections.sort(ibd.getSyncBlocks(), new Comparator<TxBlock>() {
//					@Override
//					public int compare(TxBlock o1, TxBlock o2) {
//						// TODO Auto-generated method stub
//						return 0;
//					}
//				});
				
				//Check all the syncblocks are there with none missng..
				//..
				boolean valid = true;
				
				//Valid..
				if(ibd.hasCascade() && !ibd.getCascade().checkCascade()) {
					MinimaLogger.log("ERROR Invalid Cascade sent from "+mClientUID);
					
					//Disconnect..Something fishy..
					Main.getInstance().getNIOManager().disconnect(mClientUID);
					
					return;
				}
				
				//Send to the Processor - with the client in case is invalid.. in which case disconnect
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
				}
			
			}else if(type.isEqual(MSG_TXPOW)) {
				//Read the TxPoW
				TxPoW txpow = TxPoW.ReadFromStream(dis);
				
				//DEBUG HACK
				if(GeneralParams.DEBUGFUNC) {
					MinimaLogger.log("DEBUGFUNC active : ignoring this TXPOW @ "+txpow.getBlockNumber()+" "+txpow.getTxPoWID());
					return;
				}
				
				//Do we have it..
				boolean exists = MinimaDB.getDB().getTxPoWDB().exists(txpow.getTxPoWID());
				if(exists) {
					return;
				}
				
				//OK - Some basic checks..
				boolean checksigs = TxPoWChecker.checkSignatures(txpow); 
				if(!checksigs) {
					MinimaLogger.log("SERIOUS ERROR : Invalid signatures on txpow from "+mClientUID+" "+txpow.getTxPoWID());
					return;
				}
				
				//More CHECKS.. will ignore if sigs fail, too big. not enough PoW or critical error..
				//TODO
				boolean valid = true;
				
				//Since it's OK.. forward the TxPoWID to the rest of the network..
				if(valid) {
					
					//And send to the TxPoWProcessor Processor.. 
					Main.getInstance().getTxPoWProcessor().postProcessTxPoW(txpow);
					
					//Forward to the network
					NIOManager.sendNetworkMessage("", MSG_TXPOWID, txpow.getTxPoWIDData());
					
					//Check all the Transactions..
					ArrayList<MiniData> txns = txpow.getBlockTransactions();
					for(MiniData txn : txns) {
						exists = MinimaDB.getDB().getTxPoWDB().exists(txn.to0xString());
						if(!exists) {
							//request it..
							NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txn);
						}
					}
					
					//Get the parent if we don't have it..
					exists = MinimaDB.getDB().getTxPoWDB().exists(txpow.getParentID().to0xString());
					if(!exists) {
						//request it..
						NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txpow.getParentID());
					}
					
				}else {
					//An invalid txpow.. hmm..
					MinimaLogger.log("INVALID TxPoW sent from "+mClientUID+" "+txpow.getTxPoWID());
				}
				
			}else if(type.isEqual(MSG_GENMESSAGE)) {
				//Read in the message
				MiniString msg = MiniString.ReadFromStream(dis);
				
				//Foe now..
				MinimaLogger.log(mClientUID+":"+msg.toString());
			
			}else if(type.isEqual(MSG_P2P)) {
				
				//P2P message..
				MiniString msg = MiniString.ReadFromStream(dis);
				
				//Convert to JSON
				JSONObject json = (JSONObject) new JSONParser().parse(msg.toString());
				
				//Create the message
				Message p2p = new Message(P2PFunctions.P2P_MESSAGE);
				p2p.addString("uid", mClientUID);
				p2p.addObject("message", json);
				
				//And forward to thew P2P
				Main.getInstance().getNetworkManager().getP2PManager().PostMessage(p2p);
				
			}else if(type.isEqual(MSG_PULSE)) {
				
				//Read in the Pulse..
				Pulse pulse = Pulse.ReadFromStream(dis);
				
				//We'll need this
				TxPoWDB txpdb = MinimaDB.getDB().getTxPoWDB();
				
				//Message
				MinimaLogger.log("PULSE received from "+mClientUID+" "+MiniFormat.formatSize(mData.getLength())+" "+pulse.getBlockList().size());
				
				GeneralParams.DEBUGFUNC = false;
				
				//Now check this list against your ownn..
				ArrayList<MiniData> mylist 		= MinimaDB.getDB().getTxPoWTree().getPulseList();
				ArrayList<MiniData> requestlist = new ArrayList<>();
				
				//Now check for intersection
				int counter=0;
				boolean found = false;
				ArrayList<MiniData> pulsemsg = pulse.getBlockList();
				for(MiniData block : pulsemsg) {
					if(!ListCheck.MiniDataListContains(mylist, block)) {
						//Do we have it..!
						TxPoW check = txpdb.getTxPoW(block.to0xString());
						
						if(check == null) {
							//We don't have it..
							MinimaLogger.log("REQuESTING PULSE BLOCK.. "+block.to0xString());
							requestlist.add(0, block);
							
						}else {
							//Do we have all the transactions..
							ArrayList<MiniData> txns = check.getBlockTransactions();
							for(MiniData txn : txns) {
								
								TxPoW blocktrans = txpdb.getTxPoW(txn.to0xString());
								
								if(blocktrans == null) {
									//We don't have it..
									MinimaLogger.log("REQuESTING PULSE BLOCK TXN.. "+txn.to0xString());
									requestlist.add(0, txn);
									
									NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, txn);
								}
							}
						}
					}else {
						//Crossover found!
						MinimaLogger.log("PULSE CROSSOVER BLOCK FOUND.. "+counter);
						found = true;
						break;
					}
					
					counter++;
				}
				
				//Did we find a crossover..
				if(found) {
					//Request all the blocks.. that are in the correct order
					for(MiniData block : requestlist) {
						NIOManager.sendNetworkMessage(mClientUID, MSG_TXPOWREQ, block);
					}
					
				}else {
					MinimaLogger.log("NO CROSSOVER BLOCK FOUND from "+mClientUID);
					//DISCONNECT!
				}
				
			}else {
				//UNKNOWN MESSAGE..
				MinimaLogger.log("Unknown Message type received from "+mClientUID+" type:"+type+" size:"+data.length);
				return;
			}
			
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
	}
}
