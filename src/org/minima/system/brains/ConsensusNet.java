package org.minima.system.brains;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.backup.BackupManager;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetClientReader;
import org.minima.system.txpow.TxPoWChecker;
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
	
	boolean mHardResetAllowed = true;
	
	public ConsensusNet(MinimaDB zDB, ConsensusHandler zHandler) {
		mDB = zDB;
		mHandler = zHandler;
	}
	
	private MinimaDB getMainDB() {
		return mDB;
	}
	 
	public void setHardResest(boolean zHardResetAllowed) {
		mHardResetAllowed = zHardResetAllowed;
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CONSENSUS_NET_INITIALISE)) {
			//Lets create a sync package
			ArrayList<BlockTreeNode> nodes = getMainDB().getMainTree().getAsList();
			
			//Do we have any info.. ?
			if(nodes.size()==0) {
				return;
			}
			
			//Get the complete sync package
			SyncPackage sp = getMainDB().getSyncPackage();
			
//			MiniNumber casc = getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber();
//			SyncPackage sp = new SyncPackage();
//			sp.setCascadeNode(casc);
//			//Cycle through it all..
//			for(BlockTreeNode node : nodes) {
//				sp.getAllNodes().add(0,new SyncPacket(node));
//			}
			
			//Now send that on..
			sendNetMessage(zMessage, NetClientReader.NETMESSAGE_INTRO, sp);
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_INTRO)) {
			//Get the Sync Package..
			SyncPackage sp = (SyncPackage) zMessage.getObject("sync");
			
			boolean hardreset = false;
			MiniNumber cross = MiniNumber.MINUSONE;
			
			//How much POW do you currently have
			BigInteger myweight = BigInteger.ZERO;
			if(getMainDB().getMainTree().getAsList().size()!=0) {
				myweight = getMainDB().getMainTree().getChainRoot().getTotalWeight();
			}
			
			//Are we fresh ?
			if(myweight.compareTo(BigInteger.ZERO)<=0) {
				//Refresh completely.. nothing else you can do..
				hardreset = true;
				
			}else{
				//What weight is this chain.. TODO
				//THIS WRONG.. Needs to compare both chains.. 
				//Only uses the bits after the first crossover..
				
				//FOR NOW..
				BigInteger netweight = sp.calculateWeight();
				
				//Is there a cross over - doesn't check before the cscade
				cross = checkCrossover(sp);
				
				if(cross.isEqual(MiniNumber.MINUSONE)) {
					MinimaLogger.log("IRREGULAR POW INTRO CHAIN. NO CROSSOVER BLOCK.. !");
					
					if(netweight.compareTo(myweight)>0) {
						MinimaLogger.log("INTRO CHAIN HEAVIER.. ");
					}else {
						MinimaLogger.log("YOUR CHAIN HEAVIER.. NO CHANGE REQUIRED");
						return;
					}
					
					if(mHardResetAllowed) {
						hardreset = true;
						MinimaLogger.log("HARD RESETTING.. ");
					}else {
						MinimaLogger.log("NO HARD RESET ALLOWED.. ");
						hardreset = false;
						return;
					}
				}
			}
			
			//We'll be storing the received txpow messages
			BackupManager backup = mHandler.getMainHandler().getBackupManager();
			
			//Complete Refresh..
			if(hardreset) {
				//Clear the database..
				getMainDB().getMainTree().clearTree();
				getMainDB().getCoinDB().clearDB();
				getMainDB().getTxPowDB().ClearDB();
				
				//Wipe the txpow folder..
				File txfolder = backup.getBackUpFolder(); 
				BackupManager.deleteFileOrFolder(txfolder);
				
				//Drill down 
				ArrayList<SyncPacket> packets = sp.getAllNodes();
				for(SyncPacket spack : packets) {
					TxPOW txpow = spack.getTxPOW();
					
					//Store it..
					backup.backupTxpow(txpow);
					
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
				//Some crossover was found..
				MinimaLogger.log("CROSSOVER BLOCK FOUND.. @ "+cross);
				
				//Otherwise.. 
				ArrayList<SyncPacket> intro = sp.getAllNodes();
				int totalreq = 0;
				for(SyncPacket spack : intro) {
					if(spack.getTxPOW().getBlockNumber().isMore(cross)) {
						//Just repost it..
						TxPOW txpow = spack.getTxPOW();
						
						//Get the NetClient...
						NetClient client = (NetClient) zMessage.getObject("netclient");
						
						//Post it as a normal TxPOW..
						Message msg = new Message(CONSENSUS_NET_TXPOW);
						msg.addObject("txpow", txpow);
						msg.addObject("netclient", client);
						
						mHandler.PostMessage(msg);
						
						totalreq++;
					}
				}
				
				MinimaLogger.log("Sync complete. "+totalreq+" blocks added.. ");
			}
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWID)) {
			//Get the ID
			MiniData txpowid = (MiniData) zMessage.getObject("txpowid");
			
			//Do we have it..
			if(getMainDB().getTxPOW(txpowid) == null) {
				//We don't have it, get it..
				sendNetMessage(zMessage, NetClientReader.NETMESSAGE_TXPOW_REQUEST, txpowid);
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_NET_TXPOWREQUEST)) {
			//Request for a previously sent txpowid
			MiniData txpowid = (MiniData) zMessage.getObject("txpowid");
			
			//Get it..
			TxPOW txpow = getMainDB().getTxPOW(txpowid);
			if(txpow == null) {
				//This is odd.. we should have a requested txpowid.. someone has it wrong
				//OR look deeper.. filesystem.. could be an old one.. sync up.
				MinimaLogger.log("NET TXPOWREQUEST OF MISSING TXPOW "+txpowid);
			
			}else {
				//Bit Special..Get the NetClient...
				NetClient client = (NetClient) zMessage.getObject("netclient");
				
				//Send it to him..
				Message tx = new Message(NetClient.NETCLIENT_SENDTXPOW).addObject("txpow", txpow);
				client.PostMessage(tx);
			}
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_TXPOW)) {
			/**
			 * The SINGLE entry point into the system for NEW TXPOW messages..
			 */
			
			//Forward - the internal function
			TxPOW txpow = (TxPOW)zMessage.getObject("txpow");
			
			//Do we have it.. now check DB - hmmm..
			if(getMainDB().getTxPOW(txpow.getTxPowID()) != null) {
				//WE HAVE IT..
				MinimaLogger.log("NET Transaction we already have.. "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				return;
			}
			
			//Is it even a valid TxPOW.. not enough POW ? - FIRST CHECK
			if(!txpow.isBlock() && !txpow.isTransaction()) {
				MinimaLogger.log("ERROR NET FAKE - not transaction not block : "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				//Fake ?
				return;
			}
			
			//Check the Sigs.. just the once..
			boolean sigsok = TxPoWChecker.checkSigs(txpow);
			if(!sigsok) {
				//Reject
				MinimaLogger.log("ERROR NET Invalid Signatures with TXPOW : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
				return;
			}
			
			//Now check the Transaction is Valid As of now ?
			boolean trxok = TxPoWChecker.checkTransactionMMR(txpow, getMainDB());
			if(!trxok) {
				//Reject
				MinimaLogger.log("ERROR NET TXPOW FAILS CHECK MMR: "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
				return;
			}
			
			/**
			 * Add it to the database.. Do this HERE as there may be other messages in the queue. 
			 * Can't wait for ConsensusHandler to catch up.
			 */
			getMainDB().addNewTxPow(txpow);
			
			//Now check the parent.. (Whether or not it is a block we may be out of alignment..)
			MiniData parentID = txpow.getParentID();
			if(getMainDB().getTxPOW(parentID)==null) {
				//We don't have it, get it..
				MinimaLogger.log("Request Parent TxPoW @ "+txpow.getBlockNumber()+" parent:"+parentID); 
				sendNetMessage(zMessage, NetClientReader.NETMESSAGE_TXPOW_REQUEST, parentID);
			}

			//And now check the Txn list.. basically a mempool sync
			ArrayList<MiniData> txns = txpow.getBlockTransactions();
			for(MiniData txn : txns) {
				if(getMainDB().getTxPOW(txn) == null ) {
					MinimaLogger.log("Request missing TxPoW in block "+txpow.getBlockNumber()+" "+txn);
					
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
	}

	/**
	 * Find a crossover node.. Check 2 chains and find where they FIRST intersect.
	 */
	public MiniNumber checkCrossover(SyncPackage zIntro){
		//Our Chain.. FROM TIP backwards..
		ArrayList<BlockTreeNode> chain = getMainDB().getMainTree().getAsList();
				
		//Our cascade node..
		MiniNumber maintip     = getMainDB().getMainTree().getChainTip().getTxPow().getBlockNumber();
		MiniNumber maincascade = getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber();
		
		//The incoming chain
		ArrayList<SyncPacket> introchain = zIntro.getAllNodes();
		int len = introchain.size();
		SyncPacket tip = introchain.get(len-1);
		
		//The Intro cascade node..
		MiniNumber introtip     = tip.getTxPOW().getBlockNumber();
		MiniNumber introcascade = zIntro.getCascadeNode();
	
		MinimaLogger.log("CROSSOVER mytip:"+maintip+" mycasc:"+maincascade);
		MinimaLogger.log("CROSSOVER introtip:"+introtip+" introcasc:"+introcascade);
		
		//Simple check first..
		boolean tipgood  = maintip.isLessEqual(introtip) && maintip.isMoreEqual(introcascade);
		boolean cascgood = maincascade.isLessEqual(introtip) && maincascade.isMoreEqual(introcascade);
		
		boolean found        = false;
		MiniNumber crossover = MiniNumber.MINUSONE;
		
		//No chance of a crossover..
		if(!tipgood && !cascgood) {
			return crossover;	
		}
		
		//Cycle..
		for(BlockTreeNode block : chain) {
			//BLock number and hash.. BOTH have to match
			MiniNumber bnum  = block.getTxPow().getBlockNumber();
			MiniData txpowid = block.getTxPowID();
			
			//only use nodes after our cascade..
			if(bnum.isMore(maincascade)) {
				
				//Run through the intro chain..
				for(SyncPacket spack : introchain) {
					MiniNumber snum  = spack.getTxPOW().getBlockNumber();
					
					//Only use nodes after intro cascade
					if(snum.isMore(introcascade)) {
						if(spack.getTxPOW().getBlockNumber().isEqual(bnum)) {
							//Check the TxPOWID..
							if(spack.getTxPOW().getTxPowID().isEqual(txpowid)) {
								//Crossover!
								found     = true;
								crossover = bnum;
								break;
							}
						}
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
