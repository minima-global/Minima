package org.minima.system.brains;

import java.io.File;
import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.backup.BackupManager;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetClientReader;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.messages.Message;

public class ConsensusNet extends ConsensusProcessor {

	/**
	 * Used for the custom Transactions
	 */
	public static final String CONSENSUS_PREFIX 			= "CONSENSUSNET_";
	
	public static final String CONSENSUS_NET_INITIALISE 	= CONSENSUS_PREFIX+"NET_INITIALISE";
	public static final String CONSENSUS_NET_INTRO 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_INTRO.getValue();
	public static final String CONSENSUS_NET_TXPOWID 		= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOWID.getValue();
	public static final String CONSENSUS_NET_TXPOWREQUEST	= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOW_REQUEST.getValue();
	public static final String CONSENSUS_NET_TXPOW 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOW.getValue();
	
	/**
	 * Will we switch to a heavier chain - DEBUG mode for -private
	 */
	boolean mHardResetAllowed = true;
	
	boolean mFullSyncOnInit = true;
	
	/**
	 * Has the initial Sync been done..
	 */
	boolean mInitialSync;
	
	public ConsensusNet(MinimaDB zDB, ConsensusHandler zHandler) {
		super(zDB, zHandler);
		
		mInitialSync = false;
	}
	 
	public void setHardResest(boolean zHardResetAllowed) {
		mHardResetAllowed = zHardResetAllowed;
	
		if(!mHardResetAllowed) {
			mInitialSync = true;
		}
	}
	
	public void setFullSyncOnInit(boolean zFull) {
		mFullSyncOnInit = zFull;
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CONSENSUS_NET_INITIALISE)) {
			//Get the complete sync package - deep copy.. 
			SyncPackage sp = getMainDB().getSyncPackage(true);
			
			//Get the NetClient...
			NetClient client = (NetClient) zMessage.getObject("netclient");
			Message req      = new Message(NetClient.NETCLIENT_INTRO).addObject("syncpackage", sp);
			
			//And Post it..
			client.PostMessage(req);
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_INTRO)) {
			//Get the Sync Package..
			SyncPackage sp = (SyncPackage) zMessage.getObject("sync");
			
			boolean hardreset = false;
			MiniNumber cross = MiniNumber.MINUSONE;
			
			//Check Versions..
			if(!sp.getSyncVersion().toString().equals(GlobalParams.MINIMA_VERSION)) {
				MinimaLogger.log("DIFFERENT VERSION ON SYNC "+sp.getSyncVersion());
			}
			
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
					if(netweight.compareTo(BigInteger.ZERO)>0) {
						MinimaLogger.log("IRREGULAR POW INTRO CHAIN. NO CROSSOVER BLOCK.. !");
					}
					
					if(netweight.compareTo(myweight)>0) {
						MinimaLogger.log("INTRO CHAIN HEAVIER.. ");
					}else {
						//This normally means you are STUCK.. hmm..
						MinimaLogger.log("YOUR CHAIN HEAVIER.. NO CHANGE REQUIRED");
						mInitialSync = true;
						return;
					}
					
					if(mHardResetAllowed) {
						hardreset = true;
						MinimaLogger.log("HARD RESETTING.. ");
					}else {
						MinimaLogger.log("NO HARD RESET ALLOWED.. ");
						hardreset = false;
						mInitialSync = true;
						return;
					}
				}
			}
			
			//We'll be storing the received txpow messages
			BackupManager backup = getConsensusHandler().getMainHandler().getBackupManager();
			
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
					TxPoW txpow = spack.getTxPOW();
					
					//Store it..
					backup.backupTxpow(txpow);
					
					MMRSet mmr  = spack.getMMRSet();
					boolean cascade = spack.isCascade();
					
					//Add it to the DB..
					BlockTreeNode node = getMainDB().hardAddTxPOWBlock(txpow, mmr, cascade);
					
					//Scan for coins..
					if(mmr!=null) {
						getMainDB().scanMMRSetForCoins(mmr);
					}
					
					//Is this the cascade block
					if(txpow.getBlockNumber().isEqual(sp.getCascadeNode())) {
						getMainDB().hardSetCascadeNode(node);
					}
				}
				
				//Reset weights
				getMainDB().hardResetChain();
			
				//FOR NOW
				MinimaLogger.log("Sync Complete.. Current block : "+getMainDB().getMainTree().getChainTip().getTxPow().getBlockNumber());
			
				//Now the Initial SYNC has been done you can receive TXPOW message..
				mInitialSync = true;
				
				//Do you want a copy of ALL the TxPoW in the Blocks.. ?
				//Only really useful for txpowsearch - DEXXED
				if(mFullSyncOnInit) {
					//Now request all the TXNS in those blocks..
					int reqtxn = 0;
					ArrayList<BlockTreeNode> nodes = getMainDB().getMainTree().getAsList(true);
					for(BlockTreeNode node : nodes) {
						//Get the TxPoW
						TxPoW txpow = node.getTxPow();
						
						//get the Txns..
						if(txpow.hasBody()) {
							ArrayList<MiniData> txns = txpow.getBlockTransactions();
							for(MiniData txn : txns) {
								//We don't have it, get it..
								sendTxPowRequest(zMessage, txn);
								reqtxn++;
							}
						}
					}
					
					if(reqtxn>0) {
						MinimaLogger.log("Requested "+reqtxn+" transaction in Initial Blocks..");	
					}
				}
				
			}else {
				//Some crossover was found..
				MinimaLogger.log("CROSSOVER BLOCK FOUND.. @ "+cross);
				
				//Otherwise.. 
				ArrayList<SyncPacket> intro = sp.getAllNodes();
				int totalreq = 0;
				for(SyncPacket spack : intro) {
					if(spack.getTxPOW().getBlockNumber().isMore(cross)) {
						//Just repost it..
						TxPoW txpow = spack.getTxPOW();
						
						//Get the NetClient...
						NetClient client = (NetClient) zMessage.getObject("netclient");
						
						//Post it as a normal TxPOW..
						Message msg = new Message(CONSENSUS_NET_TXPOW);
						msg.addObject("txpow", txpow);
						msg.addObject("netclient", client);
						
						if(!txpow.hasBody()) {
							MinimaLogger.log("NO BODY IN TXPOW SYNC BLOCK AFTER CASCADE "+txpow.getBlockNumber());
						}
						
						getConsensusHandler().PostMessage(msg);
						
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
				sendTxPowRequest(zMessage, txpowid);
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_NET_TXPOWREQUEST)) {
			//Request for a previously sent txpowid
			MiniData txpowid = (MiniData) zMessage.getObject("txpowid");
			
			//Get it..
			TxPoW txpow = getMainDB().getTxPOW(txpowid);
			if(txpow == null) {
				//OLD orr missing TxPoW
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
			
			//Have we done the initia SYNC..
			if(!mInitialSync) {
				MinimaLogger.log("NET TxPoW received before Initial Sync Finished.");
				return;
			}
			
			//The TxPoW
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
			
			//Do we have it.. now check DB - hmmm..
			if(getMainDB().getTxPOW(txpow.getTxPowID()) != null) {
				MinimaLogger.log("NET Transaction we already have.. "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				return;
			}
			
			//Is it even a valid TxPOW.. not enough POW ? - FIRST CHECK
			if(!txpow.isBlock() && !txpow.isTransaction()) {
				MinimaLogger.log("ERROR NET FAKE - not transaction not block : "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				//Fake - should disconnect this user..
				return;
			}
			
			//Does it have a body.. SHOULD NOT HAPPEN as only comlete post cascade txpow messages can be requested
			if(!txpow.hasBody()) {
				MinimaLogger.log("ERROR NET NO TxBODY for txpow "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				return;
			}
			
			//Check Header and Body Agree..
			MiniData bodyhash = Crypto.getInstance().hashObject(txpow.getTxBody());
			if(!txpow.getTxHeader().getBodyHash().isEqual(bodyhash)) {
				MinimaLogger.log("ERROR NET TxHeader and TxBody Mismatch! "
							+txpow.getBlockNumber()+" "+txpow.getTxPowID()+" "+txpow.getTxHeader().getBodyHash().to0xString()+" "+bodyhash.to0xString()); 
				return;
			}
			
			//Check the Sigs.. just the once..
			boolean sigsok = TxPoWChecker.checkSigs(txpow);
			if(!sigsok) {
				MinimaLogger.log("ERROR NET Invalid Signatures with TXPOW : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
				return;
			}
			
			//Now check the Transaction is Valid As of now ?
			boolean trxok          = TxPoWChecker.checkTransactionMMR(txpow, getMainDB());
			if(!trxok) {
				//Is it Already in a block?
				ArrayList<BlockTreeNode> nodes = getMainDB().getMainTree().getAsList(true);
				for(BlockTreeNode node : nodes) {
					//Get the TxPoW
					TxPoW chaintxpow = node.getTxPow();
					
					//get the Txns..
					if(chaintxpow.hasBody()) {
						ArrayList<MiniData> txns = chaintxpow.getBlockTransactions();
						for(MiniData txn : txns) {
							if(txn.isEqual(txpow.getTxPowID())) {
								//MinimaLogger.log("TXN WE DON'T HAVE FOUND IN BLOCK "+txpow.getTxPowID()); 	

								//Add it to the database..
								TxPOWDBRow row = getMainDB().addNewTxPow(txpow);
								row.setOnChainBlock(false);
								row.setIsInBlock(true);
								row.setInBlockNumber(chaintxpow.getBlockNumber());
								
								//Save it..
								getConsensusHandler().getMainHandler().getBackupManager().backupTxpow(txpow);
								
								//All done..
								return;
							}
						}
					}
				}
				
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
				//MinimaLogger.log("Request Parent TxPoW @ "+txpow.getBlockNumber()+" parent:"+parentID); 
				sendTxPowRequest(zMessage, parentID);
			}

			//And now check the Txn list.. basically a mempool sync
			ArrayList<MiniData> txns = txpow.getBlockTransactions();
			for(MiniData txn : txns) {
				if(getMainDB().getTxPOW(txn) == null ) {
					//MinimaLogger.log("Request missing TxPoW in block "+txpow.getBlockNumber()+" "+txn);
					sendTxPowRequest(zMessage, txn);
				}
			}
			
			//Now - Process the TxPOW
			Message newtxpow = new Message(ConsensusHandler.CONSENSUS_PRE_PROCESSTXPOW).addObject("txpow", txpow);
			
			//Post it
			getConsensusHandler().PostMessage(newtxpow);
		}
	}
	
	private void sendTxPowRequest(Message zFromMessage, MiniData zTxPoWID) {
		//Get the NetClient...
		NetClient client = (NetClient) zFromMessage.getObject("netclient");
			
		Message req = new Message(NetClient.NETCLIENT_SENDTXPOWREQ);
		req.addObject("txpowid", zTxPoWID);
		
		//And Post it..
		client.PostMessage(req);
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
		
		//The incoming chain - could be empty
		ArrayList<SyncPacket> introchain = zIntro.getAllNodes();
		int len = introchain.size();
		if(len == 0) {
			return MiniNumber.MINUSONE;
		}
		
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
