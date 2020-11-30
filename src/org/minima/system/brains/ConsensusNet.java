package org.minima.system.brains;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.Greeting;
import org.minima.objects.greet.HashNumber;
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.SyncPacket;
import org.minima.objects.greet.TxPoWIDList;
import org.minima.objects.greet.TxPoWList;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.Main;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.base.MinimaReader;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.system.txpow.TxPoWMiner;
import org.minima.utils.Crypto;
import org.minima.utils.DataTimer;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class ConsensusNet extends ConsensusProcessor {

	/**
	 * Used for the custom Transactions
	 */
	public static final String CONSENSUS_PREFIX 			= "CONSENSUSNET_";
	
	public static final String CONSENSUS_NET_CHECKSIZE_TXPOW 	    = CONSENSUS_PREFIX+"NET_MESSAGE_MYTXPOW";
	
	public static final String CONSENSUS_NET_INITIALISE 	= CONSENSUS_PREFIX+"NET_INITIALISE";
	public static final String CONSENSUS_NET_INTRO 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_INTRO.getValue();
	public static final String CONSENSUS_NET_TXPOWID 		= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOWID.getValue();
	public static final String CONSENSUS_NET_TXPOWREQUEST	= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOW_REQUEST.getValue();
	public static final String CONSENSUS_NET_TXPOW 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOW.getValue();
	
	public static final String CONSENSUS_NET_GREETING 		    = CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_GREETING.getValue();
	public static final String CONSENSUS_NET_GREETING_REQUEST	= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_GREETING_REQUEST.getValue();
	public static final String CONSENSUS_NET_TXPOWLIST 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOWLIST.getValue();
	public static final String CONSENSUS_NET_TXPOWIDLIST 	    = CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOWIDLIST.getValue();
	
	public static final String CONSENSUS_NET_PING 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_PING.getValue();
	
	private static int MAX_TXPOW_LIST_SIZE = 200;
	
	/**
	 * Will we switch to a heavier chain - DEBUG mode for -private
	 */
	boolean mHardResetAllowed = true;
	
	boolean mFullSyncOnInit = false;
	
	/**
	 * Check when you sent out a request for a TxPOW
	 */
	DataTimer mDataTimer = new DataTimer();
	
	/**
	 * Has the initial Sync been done..
	 */
	public boolean mInitialSync;
	
	public ConsensusNet(MinimaDB zDB, ConsensusHandler zHandler) {
		super(zDB, zHandler);
		
		mInitialSync = false;
	}
	 
	public void setAllowHardResest(boolean zHardResetAllowed) {
		mHardResetAllowed = zHardResetAllowed;
	
		if(!mHardResetAllowed) {
			mInitialSync = true;
		}
	}
	
	public void setFullSyncOnInit(boolean zFull) {
		mFullSyncOnInit = zFull;
	}
	
	public boolean isInitialSyncComplete() {
		return mInitialSync;
	}
	
	public void setInitialSyncComplete() {
		setInitialSyncComplete(true);
	}
	
	public void setInitialSyncComplete(boolean zPostNotify) {
		if(!mInitialSync) {
			mInitialSync = true;
			if(zPostNotify) {
				getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALSYNC));	
			}
		}
	}
	
	
	protected void PostNetClientMessage(Message zOrigMessage, Message zMessage) {
		if(zOrigMessage.exists("netclient")) {
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			zMessage.addObject("netclient", zOrigMessage.getObject("netclient"));
		}
			
		getConsensusHandler().PostMessage(zMessage);
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
		/**
		 * You start a network dialogue with this message
		 */
		if(zMessage.isMessageType(CONSENSUS_NET_INITIALISE)) {
			//An initial Greeting message..
			Greeting greet = new Greeting();
			
			//Get the Tree
			BlockTree tree = getMainDB().getMainTree();
			
			//Is there anything.. ?
			if(tree.getChainRoot()!=null) {
				//Cascade Node
				MiniNumber casc = tree.getCascadeNode().getTxPow().getBlockNumber();
				ArrayList<BlockTreeNode> nodes = tree.getAsList(true);
				
				//Cycle through it all..
				for(BlockTreeNode node : nodes) {
					TxPoW txpow = node.getTxPow();
					MiniNumber block = txpow.getBlockNumber();
					if(block.isMoreEqual(casc)) {
						greet.addBlock(txpow.getTxPowID(), block);
					}
				}
			}
			
			//Get the NetClient...
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			Message req      = new Message(MinimaClient.NETCLIENT_GREETING).addObject("greeting", greet);
			
			//And Post it..
			client.PostMessage(req);
			
		/**
		 * You have received the initial Greeting Message	
		 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_GREETING)) {
			//Get the greeting
			Greeting greet = (Greeting)zMessage.getObject("greeting");
			
			//Check Versions..
			if(!greet.getVersion().equals(GlobalParams.MINIMA_VERSION)) {
				MinimaLogger.log("DIFFERENT VERSION ON GREETING "+greet.getVersion());
			}
			
			//Are we a new User.. with no Chain..
			if(getMainDB().getMainTree().getAsList().size()==0) {
				//First timer.. do nothing.. you'll be sent the INTRO message
				return;
			}
			
			//Get the List..
			ArrayList<HashNumber> blocks = greet.getList();
			int greetlen = blocks.size();
			
			//This User has NO CHAIN - send him our complete version
			if(greetlen == 0) {
				MinimaLogger.log("FIRST TIME SYNC - Sending complete");
				//Get the complete sync package - deep copy.. 
				SyncPackage sp = getMainDB().getSyncPackage(true);
				MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
				Message req      = new Message(MinimaClient.NETCLIENT_INTRO).addObject("syncpackage", sp);
				client.PostMessage(req);
				return;
			}
			
			//Find the crossover - if there is one..
			MiniNumber cross = checkCrossover(greet);

			//If there is one send complete data from then on
			if(cross.isEqual(MiniNumber.MINUSONE)) {
				BackupManager backup = Main.getMainHandler().getBackupManager();
				
				//Check if the cascade is an old block of ours..
				HashNumber startblock = blocks.get(0);
				MiniNumber lowest = blocks.get(0).getNumber();
				MinimaLogger.log("Checking for block "+lowest);
				
				SyncPacket casc = SyncPacket.loadBlock(backup.getBlockFile(lowest));
				
				if(casc == null) {
					return;
				}
				
				if(!casc.getTxPOW().getTxPowID().isEqual(startblock.getHash())) {
					MinimaLogger.log("DIFFERENT HISTORY! - NOTHING TO DO..");
					return;
				}
				
				//NO CROSSOVER..!
				MinimaLogger.log("CROSSOVER BLOCK FOUND!.. SEND OLD BLOCKS");
				return;
			}
			
			//Send the complete stack of TxPoW from cross onwards..
			BlockTreeNode top = getMainDB().getMainTree().getChainTip();
			
			//How Many blocks do we need to send..
			int blocklen = top.getBlockNumber().sub(cross).getAsInt(); 
			if(blocklen == 0) {
				MinimaLogger.log("ALLREADY IN SYNC.. NOTHING TO SEND!");
				return;
			}
			
			MinimaLogger.log("CROSSOVER FOUND!.. SENDING "+blocklen+" FULL BLOCKS");
			int counter=0;
			ArrayList<TxPoW> full_list = new ArrayList<>();
			while(counter<blocklen) {
				full_list.add(0,top.getTxPow());
				
				//Keep going..
				counter++;
				top = top.getParent();
			}
			
			//Now cycle through from the bottom to the top..
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			TxPoWList currentblocks = new TxPoWList();
			
			for(TxPoW blk : full_list) {
				
				//Add all the TXNS..
				ArrayList<MiniData> txns = blk.getBlockTransactions();
				for(MiniData txn : txns) {
					TxPoW txpow = getMainDB().getTxPOW(txn);
					if(txpow!=null) {
						currentblocks.addTxPow(txpow);
					}
				}
				
				//Add this TxPoW and the Txns in it..
				currentblocks.addTxPow(blk);
				
				//Check the size..
				if(currentblocks.size() > 100) {
					//And send it to the client
					client.PostMessage(new Message(MinimaClient.NETCLIENT_TXPOWLIST).addObject("txpowlist", currentblocks));
				
					//And create..
					currentblocks = new TxPoWList();
				}
			}
			
			//Clean up and send the final blocks..
			if(currentblocks.size() > 0) {
				//And send it to the client
				client.PostMessage(new Message(MinimaClient.NETCLIENT_TXPOWLIST).addObject("txpowlist", currentblocks));
			}
			
		/**
		 * You have received multiple TxPoW messages 	
		 */
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWLIST)) {
			MinimaLogger.log(zMessage.toString());
			
			TxPoWList block = (TxPoWList)zMessage.getObject("txpowlist"); 
			ArrayList<TxPoW> txps = block.getList();
			
			//Cycle through..
			for(TxPoW txp : txps) {
				//POST IT..
				PostNetClientMessage(zMessage, new Message(CONSENSUS_NET_TXPOW).addObject("txpow", txp));
			}
			
		/**
		 * You have NO CHAIN and this is your initial setup message
		 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_INTRO)) {
			//Only do this if you have no chain..
			if(getMainDB().getMainTree().getAsList().size()!=0) {
				MinimaLogger.log("ERROR : INTRO SYNC message received.. even though I HAVE a chain..");
				return;
			}
				
			//Get the Sync Package..
			SyncPackage sp = (SyncPackage) zMessage.getObject("sync");
			
			//We'll be storing the received txpow messages
			BackupManager backup = Main.getMainHandler().getBackupManager();
			
			//Clear the database..
			getMainDB().getMainTree().clearTree();
			getMainDB().getCoinDB().clearDB();
			getMainDB().getTxPowDB().ClearDB();
			
			//Wipe the txpow folder..
			BackupManager.safeDelete(backup.getBackUpFolder());
				
			//Drill down 
			ArrayList<SyncPacket> packets = sp.getAllNodes();
			float totpacks = packets.size();
			float counter  = 0;
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
				
				//Add all the tokens..
				if(txpow.isTransaction()) {
					TokenProof tokp = txpow.getTransaction().getTokenGenerationDetails();
					if(tokp!=null) {
						getMainDB().getUserDB().addTokenDetails(tokp);
					}	
					
					ArrayList<TokenProof> tokens =  txpow.getWitness().getAllTokenDetails();
					for(TokenProof tp : tokens) {
						getMainDB().getUserDB().addTokenDetails(tp);
					}
				}
				
				//Notify..
				counter++;
				int totperc = (int)((counter / totpacks) * 100.0f);
				getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALPERC).addString("info", "Loading "+totperc+"%"));
			}
				
			//Reset weights
			getMainDB().hardResetChain();
			
			//FOR NOW
			TxPoW tip = getMainDB().getMainTree().getChainTip().getTxPow();
			MinimaLogger.log("Initial Sync Complete.. Reset Current block : "+tip.getBlockNumber());
		
			//Do the balance.. Update listeners if changed..
			getConsensusHandler().PostMessage(new Message(ConsensusPrint.CONSENSUS_BALANCE).addBoolean("hard", true));
			
			//Post a message to those listening
			getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_NEWBLOCK).addObject("txpow", tip));
			
			//Backup the system..
			getConsensusHandler().PostTimerMessage(new TimerMessage(2000,ConsensusBackup.CONSENSUSBACKUP_BACKUP));
				
		/**
		 * A TxPoWID message from a client.. do you need it ?	
		 */
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWID)) {
			//Get the ID
			MiniData txpowid = (MiniData) zMessage.getObject("txpowid");
			
			//Do we have it..
			if(getMainDB().getTxPOW(txpowid) == null) {
				//MinimaLogger.log("NEW TXPOWID "+txpowid.to0xString()+" from "+zMessage.getObject("netclient"));
				//We don't have it, get it..
				sendTxPowRequest(zMessage, txpowid);
			}
		
		/**
		 * Client requests a TxPoW from you..	
		 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_TXPOWREQUEST)) {
			//Request for a previously sent txpowid
			MiniData txpowid = (MiniData) zMessage.getObject("txpowid");
			
			//Get it..
			TxPoW txpow = getMainDB().getTxPOW(txpowid);
			if(txpow == null) {
				//OLD or missing TxPoW
				MinimaLogger.log("NET TXPOWREQUEST OF MISSING TXPOW "+txpowid);
			}else {
				//Send it to him..
				MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
				Message tx = new Message(MinimaClient.NETCLIENT_SENDTXPOW).addObject("txpow", txpow);
				client.PostMessage(tx);
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_NET_PING)) {
			//Send it on to the netwclient..
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			client.PostMessage(new Message(MinimaClient.NETCLIENT_PING));
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_CHECKSIZE_TXPOW)) {
			//Internal message sent from you..
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
			
			//Create the follow up message
			Message txpownet = new Message(CONSENSUS_NET_TXPOW).addObject("txpow", txpow);
			
			//Is this from the net
			if(zMessage.exists("netclient")) {
				//Get the NetClient...
				MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
				txpownet.addObject("netclient", client);
			}
			
			if(txpow.getSizeinBytes() > MinimaReader.MAX_TXPOW) {
				MinimaLogger.log("ERROR - You've Mined A TxPoW that is too BIG! "+txpow.getSizeinBytes()+" / "+MinimaReader.MAX_TXPOW);
				return;
			}
			
			//Forward it properly
			getConsensusHandler().PostMessage(txpownet);
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_TXPOW)) {
			/**
			 * The SINGLE entry point into the system for NEW TXPOW messages..
			 */
			//Have we done the initial SYNC..
//			if(!mInitialSync) {
//				MinimaLogger.log("NET TxPoW received before Initial Sync Finished.");
//				return;
//			}
			
			//The TxPoW
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
		
			//DEBUG logs..
			MinimaLogger.log("TXPOW RECEIVED "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
			
			//Do we have it.. now check DB - hmmm..
			if(getMainDB().getTxPOW(txpow.getTxPowID()) != null) {
				MinimaLogger.log("NET Transaction we already have.. "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				return;
			}
			
//			//Get the TxPowID..
//			String txpowid = txpow.getTxPowID().to0xString();
//			
//			//Was it requested anyway.. ?
//			boolean requested = false;
//			if(getNetworkHandler().isRequestedTxPow(txpowid)) {
//				requested = true;
//			}
//			
//			//Remove it from the list - just in case..
//			getNetworkHandler().removeRequestedTxPow(txpowid);
//			
//			//Check the Validity..
//			boolean txnok = TxPoWChecker.checkTransactionMMR(txpow, getMainDB());
//			if(!txnok) {
//				//Was it requested.. ?
//				if(requested) {
//					//Ok - could be from a different branch block.. 
//					MinimaLogger.log("WARNING NET Invalid TXPOW (Requested..) : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
//				}else {
//					//Not requested invalid transaction..
//					MinimaLogger.log("ERROR NET Invalid TXPOW (UN-Requested..) : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
//					return;	
//				}
//			}
			
			/**
			 * IT PASSES!
			 * 
			 * Add it to the database.. Do this HERE as there may be other messages in the queue. 
			 * Can't wait for ConsensusHandler to catch up.
			 */
			getMainDB().addNewTxPow(txpow);
			
			//Now - Process the TxPOW
			Message newtxpow = new Message(ConsensusHandler.CONSENSUS_PROCESSTXPOW).addObject("txpow", txpow);
			getConsensusHandler().PostMessage(newtxpow);
			
			//Now check we have the parent.. (Whether or not it is a block we may be out of alignment..)
			MiniData parentID = txpow.getParentID();
			if(getMainDB().getTxPOW(parentID) == null) {
				//We don't have it, get it..
				MinimaLogger.log("Request Parent TxPoW @ "+txpow.getBlockNumber()+" parent:"+parentID); 
				sendTxPowRequest(zMessage, parentID);
			}
			
			//And now check the Txn list..
			if(txpow.isBlock()) {
				ArrayList<MiniData> txns = txpow.getBlockTransactions();
				for(MiniData txn : txns) {
					if(getMainDB().getTxPOW(txn) == null ) {
						MinimaLogger.log("Request missing TxPoW in block "+txpow.getBlockNumber()+" "+txn);
						sendTxPowRequest(zMessage, txn);
					}
				}
			}
		}
	}
	
	/**
	 * Send a Request for a Missing TxPOW
	 * Check if has been done recently and reposts with a 5 second delay if it has
	 * 
	 * @param zFromMessage
	 * @param zTxPoWID
	 */
	private void sendTxPowRequest(Message zFromMessage, MiniData zTxPoWID) {
		//Check if we have sent off for it recently..
		String data  = zTxPoWID.to0xString();
		
		//Get the NetClient...
		MinimaClient client = (MinimaClient) zFromMessage.getObject("netclient");
				
		//Check for it.. in last 5 seconds..
		boolean found = mDataTimer.checkForData(data, 5000);
		
		//If found.. repost the request on a 5 second timer..
		if(found) {
			//MinimaLogger.log("Delay SendTxPOWRequest for 10 secs.."+data+" from "+client);
			TimerMessage newtxpowid = new TimerMessage(10000, CONSENSUS_NET_TXPOWID);
			//Add the TxPOWID
			newtxpowid.addObject("txpowid", zTxPoWID);
			//And the Net Client..
			newtxpowid.addObject("netclient", client);
			
			//Post it for later..
			getConsensusHandler().PostTimerMessage(newtxpowid);
			return;
		}
		
		//Give it to the client to send on..	
		Message req = new Message(MinimaClient.NETCLIENT_SENDTXPOWREQ);
		req.addObject("txpowid", zTxPoWID);
		
		//And Post it..
		client.PostMessage(req);
	}
	
	/**
	 * Find a crossover node.. Check 2 chains and find where they FIRST intersect.
	 */
	private MiniNumber checkCrossover(Greeting zGreeting){
		//Our Chain.. FROM TIP backwards..
		ArrayList<BlockTreeNode> chain = getMainDB().getMainTree().getAsList();
				
		//Our cascade node..
		MiniNumber maintip     = getMainDB().getMainTree().getChainTip().getTxPow().getBlockNumber();
		MiniNumber maincascade = getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber();
		
		//The incoming chain - could be empty
		ArrayList<HashNumber> introchain = zGreeting.getList();
		int len = introchain.size();
		if(len == 0) {
			return MiniNumber.MINUSONE;
		}
		
		HashNumber tip = introchain.get(len-1);
		
		//The Intro cascade node..
		MiniNumber introtip     = tip.getNumber();
		MiniNumber introcascade = introchain.get(0).getNumber();
	
		MinimaLogger.log("GREETING mytip:"+maintip+" mycascade:"+maincascade+" greetingtip:"+introtip+" greetingcascade:"+introcascade);
		
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
				for(HashNumber spack : introchain) {
					MiniNumber snum  = spack.getNumber();
					
					//Only use nodes after intro cascade
					if(snum.isMore(introcascade)) {
						if(spack.getNumber().isEqual(bnum)) {
							//Check the TxPOWID..
							if(spack.getHash().isEqual(txpowid)) {
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
