package org.minima.system.brains;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
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
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.greet.Greeting;
import org.minima.objects.greet.HashNumber;
import org.minima.objects.greet.TxPoWList;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.backup.BackupManager;
import org.minima.system.backup.SyncPackage;
import org.minima.system.backup.SyncPacket;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetClientReader;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class ConsensusNet extends ConsensusProcessor {

	/**
	 * Used for the custom Transactions
	 */
	public static final String CONSENSUS_PREFIX 			= "CONSENSUSNET_";
	
	public static final String CONSENSUS_NET_CHECKSIZE_TXPOW 	    = CONSENSUS_PREFIX+"NET_MESSAGE_MYTXPOW";
	
	public static final String CONSENSUS_NET_INITIALISE 	= CONSENSUS_PREFIX+"NET_INITIALISE";
	public static final String CONSENSUS_NET_INTRO 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_INTRO.getValue();
	public static final String CONSENSUS_NET_TXPOWID 		= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOWID.getValue();
	public static final String CONSENSUS_NET_TXPOWREQUEST	= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOW_REQUEST.getValue();
	public static final String CONSENSUS_NET_TXPOW 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOW.getValue();
	
	public static final String CONSENSUS_NET_GREETING 		    = CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_GREETING.getValue();
	public static final String CONSENSUS_NET_TXPOWLIST_REQUEST	= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOWLIST_REQUEST.getValue();
	public static final String CONSENSUS_NET_TXPOWLIST 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_TXPOWLIST.getValue();
	
	public static final String CONSENSUS_NET_PING 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+NetClientReader.NETMESSAGE_PING.getValue();
	
	/**
	 * Will we switch to a heavier chain - DEBUG mode for -private
	 */
	boolean mHardResetAllowed = true;
	
	boolean mFullSyncOnInit = true;
	
	/**
	 * Has the initial Sync been done..
	 */
	public boolean mInitialSync;
	
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
	
	public void initialSyncComplete() {
		if(!mInitialSync) {
			mInitialSync = true;
			getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALSYNC));	
		}
	}
	
	public void processMessage(Message zMessage) throws Exception {
		
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
			NetClient client = (NetClient) zMessage.getObject("netclient");
			Message req      = new Message(NetClient.NETCLIENT_GREETING).addObject("greeting", greet);
			
			//And Post it..
			client.PostMessage(req);
			
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
					if(netweight.compareTo(BigInteger.ZERO)>0) {
						MinimaLogger.log("IRREGULAR POW INTRO CHAIN. NO CROSSOVER BLOCK.. !");
					}
					
					if(netweight.compareTo(myweight)>0) {
						MinimaLogger.log("INTRO CHAIN HEAVIER.. ");
					}else {
						//This normally means you are STUCK.. hmm..
						MinimaLogger.log("YOUR CHAIN HEAVIER.. NO CHANGE REQUIRED");
						initialSyncComplete();
						return;
					}
					
					if(mHardResetAllowed) {
						hardreset = true;
						MinimaLogger.log("HARD RESETTING.. ");
					}else {
						MinimaLogger.log("NO HARD RESET ALLOWED.. ");
						hardreset = false;
						initialSyncComplete();
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
				}
				
				//Reset weights
				getMainDB().hardResetChain();
			
				//Now the Initial SYNC has been done you can receive TXPOW message..
				initialSyncComplete();
				
				//FOR NOW
				TxPoW tip = getMainDB().getMainTree().getChainTip().getTxPow();
				MinimaLogger.log("Sync Complete.. Reset Current block : "+tip.getBlockNumber());
			
				//Post a message to those listening
				getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_NEWBLOCK).addObject("txpow", tip));
				
				//Backup the system..
				getConsensusHandler().PostTimerMessage(new TimerMessage(2000,ConsensusBackup.CONSENSUSBACKUP_BACKUP));
				
//				//Do you want a copy of ALL the TxPoW in the Blocks.. ?
//				//Only really useful for txpowsearch - DEXXED
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
								//MinimaLogger.log("REQ TXN from initial sync "+txn);
								
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
			}
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_GREETING)) {
			//Get the greeting
			Greeting greet = (Greeting)zMessage.getObject("greeting");
			
			//Check Versions..
			if(!greet.getVersion().equals(GlobalParams.MINIMA_VERSION)) {
				MinimaLogger.log("DIFFERENT VERSION ON GREETING "+greet.getVersion());
			}
			
			//Are we a beginner..
			if(getMainDB().getMainTree().getAsList().size()==0) {
				//First timer.. do nothing.. you'll be sent the INTRO message
				return;
			}
			
			//Get the List..
			ArrayList<HashNumber> blocks = greet.getList();
			int greetlen = blocks.size();
			
			//Do we post a complete package..
			if(greetlen == 0) {
				MinimaLogger.log("FIRST TIME SYNC - Sending complete");
				//Get the complete sync package - deep copy.. 
				SyncPackage sp = getMainDB().getSyncPackage(true);
				NetClient client = (NetClient) zMessage.getObject("netclient");
				Message req      = new Message(NetClient.NETCLIENT_INTRO).addObject("syncpackage", sp);
				client.PostMessage(req);
				return;
			}
			
			MiniNumber cross = checkCrossover(greet);
			if(cross.isEqual(MiniNumber.MINUSONE)) {
				MinimaLogger.log("NO CROSSOVER - Sending complete");
				//Get the complete sync package - deep copy.. 
				SyncPackage sp = getMainDB().getSyncPackage(true);
				NetClient client = (NetClient) zMessage.getObject("netclient");
				Message req      = new Message(NetClient.NETCLIENT_INTRO).addObject("syncpackage", sp);
				client.PostMessage(req);
				return;
			}
			
			//Get the tip..
			MiniData top   = blocks.get(greetlen-1).getHash();
			MiniNumber len = blocks.get(greetlen-1).getNumber().sub(cross);
			
			if(len.getAsInt() == 0) {
				initialSyncComplete();
				return;
			}else {
				MinimaLogger.log("CROSSOVER FOUND Requesting from "+cross+" to "+blocks.get(greetlen-1).getNumber());	
			}
			
			//Ask for Just the required Blocks..
			HashNumber hn = new HashNumber(top, len);
			
			NetClient client = (NetClient) zMessage.getObject("netclient");
			Message req      = new Message(NetClient.NETCLIENT_TXPOWLIST_REQ).addObject("hashnumber", hn);
			client.PostMessage(req);
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWLIST_REQUEST)) {
			//Get the details
			HashNumber hashnum = (HashNumber)zMessage.getObject("hashnumber");
			int max = hashnum.getNumber().getAsInt();
			
			TxPoWList txplist = new TxPoWList();
			int counter = 0;
			
			BlockTreeNode top = getMainDB().getMainTree().findNode(hashnum.getHash());
			while(top!=null && counter<max) {
				txplist.addTxPow(top.getTxPow());
				top = top.getParent();
				counter++;
			}
			
			//Now send that..!
			NetClient client = (NetClient) zMessage.getObject("netclient");
			Message req      = new Message(NetClient.NETCLIENT_TXPOWLIST).addObject("txpowlist", txplist);
			client.PostMessage(req);
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWLIST)) {
			TxPoWList txplist = (TxPoWList)zMessage.getObject("txpowlist"); 
			
			//Get the NetClient...
			NetClient client = (NetClient) zMessage.getObject("netclient");
			
			//Cycle through and add as a normal message - extra transactions will be requested as normal
			ArrayList<TxPoW> txps = txplist.getList();
			for(TxPoW txp : txps) {
				Message msg = new Message(CONSENSUS_NET_TXPOW);
				msg.addObject("txpow", txp);
				msg.addObject("netclient", client);
				getConsensusHandler().PostMessage(msg);
			}
			
			//Now the Initial SYNC has been done you can receive TXPOW message..
			initialSyncComplete();
			
			//Do a complete backup..
			getConsensusHandler().PostTimerMessage(new TimerMessage(20000,ConsensusBackup.CONSENSUSBACKUP_BACKUP));
			
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
				//OLD or missing TxPoW
				MinimaLogger.log("NET TXPOWREQUEST OF MISSING TXPOW "+txpowid);
			
			}else {
				//Bit Special..Get the NetClient...
				NetClient client = (NetClient) zMessage.getObject("netclient");
				
				//Send it to him..
				Message tx = new Message(NetClient.NETCLIENT_SENDTXPOW).addObject("txpow", txpow);
				client.PostMessage(tx);
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_NET_PING)) {
			//Send it on to the netwclient..
			NetClient client = (NetClient) zMessage.getObject("netclient");
			client.PostMessage(new Message(NetClient.NETCLIENT_PING));
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_CHECKSIZE_TXPOW)) {
			//Internal message sent from you..
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
			
			//Create the follow up message
			Message txpownet = new Message(CONSENSUS_NET_TXPOW).addObject("txpow", txpow);
			
			//Is this from the net
			if(zMessage.exists("netclient")) {
				//Get the NetClient...
				NetClient client = (NetClient) zMessage.getObject("netclient");
				txpownet.addObject("netclient", client);
			}
			
			//DOUBLE CHECK THE SIZE
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			txpow.writeDataStream(dos);
			dos.flush();
			int txpowsize = baos.toByteArray().length;
			dos.close();
			baos.close();
			
			if(txpowsize > NetClientReader.MAX_TXPOW) {
				MinimaLogger.log("ERROR - You've Mined A TxPoW that is too BIG! "+txpowsize+" / "+NetClientReader.MAX_TXPOW);
				return;
			}
			
			//Forward it properly
			getConsensusHandler().PostMessage(txpownet);
			
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
				return;
			}
			
			//Does it have a body.. SHOULD NOT HAPPEN as only complete post cascade txpow messages can be requested
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
			
			//Now check the Transaction - if it fails, could already be in a block..
			//THIS ONLY from the starter sync.. and adds tokens found..
			//Also - Gimme50 transactions PASS even though they are already in blocks.. need to check for them..
			boolean trxok = TxPoWChecker.checkTransactionMMR(txpow, getMainDB());
			if(!trxok || txpow.getTransaction().isGimme50()) {
				//Is it Already in a VALID block?
				TxPoW validblock = getMainDB().findBlockForTransaction(txpow);
				if(validblock != null) {
					//Add it to the database..
					TxPOWDBRow row = getMainDB().addNewTxPow(txpow);
					row.setOnChainBlock(false);
					row.setIsInBlock(true);
					row.setInBlockNumber(validblock.getBlockNumber());
					
					//Add all the tokens..
					TokenProof tokp = txpow.getTransaction().getTokenGenerationDetails();
					if(tokp!=null) {
						getMainDB().getUserDB().addTokenDetails(tokp);
					}
					
					ArrayList<TokenProof> tokens =  txpow.getWitness().getAllTokenDetails();
					for(TokenProof tp : tokens) {
						getMainDB().getUserDB().addTokenDetails(tp);
					}
					
					//Save it..
					getConsensusHandler().getMainHandler().getBackupManager().backupTxpow(txpow);
					
					//All done..
					return;
				}
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
				sendTxPowRequest(zMessage, parentID);
			}

			//And now check the Txn list.. basically a mempool sync
			ArrayList<MiniData> txns = txpow.getBlockTransactions();
			for(MiniData txn : txns) {
				if(getMainDB().getTxPOW(txn) == null ) {
					MinimaLogger.log("Request missing TxPoW in block "+txpow.getBlockNumber()+" "+txn);
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
	
		MinimaLogger.log("SYNCPACKAGE mytip:"+maintip+" mycascade:"+maincascade+" synctip:"+introtip+" synccascade:"+introcascade);
		
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
	
	/**
	 * Find a crossover node.. Check 2 chains and find where they FIRST intersect.
	 */
	public MiniNumber checkCrossover(Greeting zGreeting){
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
