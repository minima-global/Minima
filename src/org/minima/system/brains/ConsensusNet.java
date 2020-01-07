package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetClientReader;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;

public class ConsensusNet {

	/**
	 * Used for the custom Transactions
	 */
	public static final String CONSENSUS_PREFIX 			= "CONSENSUSNET_";
	
	public static final String CONSENSUS_NET_INITIALISE 	= CONSENSUS_PREFIX+"NET_INITIALISE";
	public static final String CONSENSUS_NET_INTRO 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_INTRO.getValue();
	public static final String CONSENSUS_NET_TXPOWID 		= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOWID.getValue();
	public static final String CONSENSUS_NET_TXPOWREQUEST	= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOW_REQUEST.getValue();
	public static final String CONSENSUS_NET_TXPOW 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOW.getValue();
	
	
	MinimaDB mDB;
	ConsensusHandler mHandler;
	
	public ConsensusNet(MinimaDB zDB, ConsensusHandler zHandler) {
		mDB = zDB;
		mHandler = zHandler;
	}
	
	private MinimaDB getMainDB() {
		return mDB;
	}
	 
	public void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CONSENSUS_NET_INITIALISE)) {
			//Lets create a sync package
			ArrayList<BlockTreeNode> nodes = getMainDB().getMainTree().getAsList();
			
			//Do we have any info.. ?
			if(nodes.size()==0) {
				return;
			}
			
			MiniNumber casc = getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber();
			SyncPackage sp = new SyncPackage();
			sp.setCascadeNode(casc);
			
			//Cycle through it all..
			for(BlockTreeNode node : nodes) {
				MiniNumber block = node.getTxPow().getBlockNumber();
//				sp.getAllNodes().add(0,new SyncPacket(node, block.isLessEqual(casc)));
				sp.getAllNodes().add(0,new SyncPacket(node));
			}
			
			//Now send that on..
			sendNetMessage(zMessage, NetClientReader.NETMESSAGE_INTRO, sp);
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_INTRO)) {
			//Get the Sync Package..
			SyncPackage sp = (SyncPackage) zMessage.getObject("sync");
			
			//Find a crossover.. and request valid txpow you don't have..
			boolean hardhack = false;
//			MiniNumber cross = MiniNumber.ZERO;
//			
//			//Do we have anything
//			if(getMainDB().getMainTree().getAsList().size()>0) {
//				//Is there a cross over
//				cross = checkCrossover(sp);
//				
//				//Do we intersect.. 
//				if(cross.isEqual(MiniNumber.ZERO)) {
//					SimpleLogger.log("IRREGULAR POW INTRO CHAIN.NO CROSSOVER BLOCK.. RESETTING TO NEW CHAIN!"+sp);
//				
//					//Which chain has the most POW
//					hardhack = true;
//				
//				}
//			}

//			SimpleLogger.log("Check Weights!!");
//			sp.calculateWeight();
			
			//FRESH? - Initial User no previous cascade.
			if(hardhack || getMainDB().getMainTree().getAsList().size()==0) {
				//Clear the database..
				getMainDB().getTxPowDB().ClearDB();
				
				//Drill down 
				ArrayList<SyncPacket> packets = sp.getAllNodes();
				for(SyncPacket spack : packets) {
					TxPOW txpow = spack.getTxPOW();
					MMRSet mmr  = spack.getMMRSet();
					boolean cascade = spack.isCascade();
					
					//Add it to the DB..
					BlockTreeNode node = getMainDB().hardAddTxPOWBlock(txpow, mmr, cascade);
					
					//Is this the cascade block
					if(txpow.getBlockNumber().isEqual(sp.getCascadeNode())) {
						getMainDB().hardSetCascadeNode(node);
					}
				}
				
				//Reset weights
				getMainDB().hardResetChain();
			
				//FOR NOW
				MinimaLogger.log("Sync Complete.. Current block : "+getMainDB().getMainTree().getChainTip());
				
			}else {
				//Check the chain Parents!
				//..TODO
				
				//Find a crossover.. and request valid txpow you don't have..
				MiniNumber cross = checkCrossover(sp);
				
				//Do we intersect.. 
				if(cross.isEqual(MiniNumber.ZERO)) {
					MinimaLogger.log("IRREGULAR POW INTRO CHAIN. NO CROSSOVER BLOCK.. "+sp);
					return;
				}
				
				//Otherwise.. 
				ArrayList<SyncPacket> intro = sp.getAllNodes();
				boolean requeston= false;
				int totalreq = 0;
				for(SyncPacket spack : intro) {
					//Is this the crossover..
					if(spack.getTxPOW().getBlockNumber().isMoreEqual(cross)) {
						//OK - from here on we ASK for the TXPOW..!
						requeston = true;
					}
					
					if(requeston) {
						//Request all the TXPOW required.. txn first then block..
						TxPOW txpow = spack.getTxPOW();
						
						//Txns
						ArrayList<MiniData32> txns = txpow.getBlockTxns();
						for(MiniData32 txn : txns) {
							if(!getMainDB().isTxPOWFound(txn)) {
								//Request it!
								sendNetMessage(zMessage, NetClientReader.NETMESSAGE_TXPOW_REQUEST, txn);
								totalreq++;
							}
						}
						
						//Main Txpow
						if(!getMainDB().isTxPOWFound(txpow.getTxPowID())) {
							//Request it!
							sendNetMessage(zMessage, NetClientReader.NETMESSAGE_TXPOW_REQUEST, txpow.getTxPowID());
							totalreq++;
						}
					}
				}
				
				MinimaLogger.log("Sync complete. "+totalreq+" requests made.. ");
			}
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWID)) {
			//Get the ID
			MiniData32 txpowid = (MiniData32) zMessage.getObject("txpowid");
			
			//Do we have it..?
			TxPOW txpow = getMainDB().getTxPOW(txpowid);
			if(txpow == null) {
				//We don't have it, get it..
				sendNetMessage(zMessage, NetClientReader.NETMESSAGE_TXPOW_REQUEST, txpowid);
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_NET_TXPOWREQUEST)) {
			//Request for a previously sent txpowid
			MiniData32 txpowid = (MiniData32) zMessage.getObject("txpowid");
			
			//Get it..
			TxPOW txpow = getMainDB().getTxPOW(txpowid);
			if(txpow == null) {
				//This is odd.. we should have a requested txpowid.. someone has it wrong
				//OR look deeper.. filesystem.. could be an old one.. sync up.
				MinimaLogger.log("TXPOWREQUEST OF MISSING TXPOW "+txpowid);
			
			}else {
				//Bit Special..Get the NetClient...
				NetClient client = (NetClient) zMessage.getObject("netclient");
				
				//Send it to him..
				Message tx = new Message(NetClient.NETCLIENT_SENDTXPOW).addObject("txpow", txpow);
				client.PostMessage(tx);
			}
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_TXPOW)) {
			//Forward - the internal function
			TxPOW txpow = (TxPOW)zMessage.getObject("txpow");
			
			//Is it even a valid TxPOW.. not enough POW ? - FIRST CHECK
			if(!txpow.isBlock() && !txpow.isTransaction()) {
				MinimaLogger.log("ERROR FAKE - not enough POW : "+txpow);
				//Fake ?
				return;
			}
			
			//Do we have it.. now check DB
			if(getMainDB().getTxPOW(txpow.getTxPowID()) != null) {
				//WE HAVE IT..
				return;
			}
			
			//Check the Sigs.. just the once..
			boolean sigsok = TxPOWChecker.checkSigs(txpow);
			if(!sigsok) {
				//Reject
				MinimaLogger.log("Invalid Signatures with TXPOW : "+txpow); 
				return;
			}
			
			//Now check the Transaction is Valid As of now ?
			boolean trxok = TxPOWChecker.checkTransactionMMR(txpow, getMainDB());
			if(!trxok) {
				//Reject
				MinimaLogger.log("NET TXPOW NOT OK : "+txpow); 
				return;
			}
			
			//Add it to the database.. Do this here as there may be other messages in the queue. 
			//Can't wait for ConsensusHandler to catch up. 
			TxPOWDBRow row = getMainDB().addNewTxPow(txpow);
			
			//Now check the parent.. (Whether or not it is a block we may be out of alignment..)
			if(getMainDB().getTxPOW(txpow.getParentID())==null) {
				//We don't have it, get it..
				sendNetMessage(zMessage, NetClientReader.NETMESSAGE_TXPOW_REQUEST, txpow.getParentID());
			}

			//And now check the Txn list.. basically a mempool sync
			ArrayList<MiniData32> txns = txpow.getBlockTxns();
			for(MiniData32 txn : txns) {
				if(getMainDB().getTxPOW(txn) == null) {
					//We don't have it, get it..
					sendNetMessage(zMessage, NetClientReader.NETMESSAGE_TXPOW_REQUEST, txn);
				}
			}
			
			//Now - Process the TxPOW
			Message newtxpow = new Message(ConsensusHandler.CONSENSUS_PRE_PROCESSTXPOW).addObject("txpow", txpow);
			
			//Post it
			mHandler.PostMessage(newtxpow);
		}
	}
	
	/**
	 * Send a network message to the sender of this message
	 * 
	 * This _should also send out a timerMessage that checks if we got it and if not to
	 * send a TXPOW_REQUEST to everyone.. 
	 */
	private void sendNetMessage(Message zFromMessage, MiniByte zMessageType, Streamable zObject) {
		//Get the NetClient...
		NetClient client = (NetClient) zFromMessage.getObject("netclient");
		
		//Send the message
		Message msg = new Message(NetClient.NETCLIENT_SENDOBJECT);
		msg.addObject("type", zMessageType);
		
		//Object can be null
		if(zObject != null) {
			msg.addObject("object", zObject);
		}
		
		//Post it..
		client.PostMessage(msg);
		
		//Create a timer message to check.. 
		//.. 
	}

	/**
	 * Find a crossover node.. Check 2 chains and find where they intersect.
	 */
	public MiniNumber checkCrossover(SyncPackage zIntro) {
		//Our Chain.. FROM root onwards..
		ArrayList<BlockTreeNode> chain = getMainDB().getMainTree().getAsList();
		MiniNumber maincascade = getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber();
		
		//The incoming chain
		ArrayList<SyncPacket> introchain = zIntro.getAllNodes();
		
		boolean found        = false;
		MiniNumber crossover = MiniNumber.ZERO;
		
		//Cycle..
		for(BlockTreeNode block : chain) {
			MiniNumber bnum = block.getTxPow().getBlockNumber();
			//only use nodes after our cascade..
			if(bnum.isMore(maincascade)) {
				//Run through the intro chain..
				for(SyncPacket spack : introchain) {
					if(spack.getTxPOW().getBlockNumber().isEqual(bnum)) {
						//Crossover!
						found = true;
						crossover = bnum;
						break;
					}
				}
			}
		
			if(found) {
				break;
			}
		}
		
		//no Hit..
		return crossover;
	}
}
