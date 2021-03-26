package org.minima.system.brains;

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
import org.minima.objects.greet.SyncPackage;
import org.minima.objects.greet.SyncPacket;
import org.minima.objects.greet.TxPoWList;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.Main;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.base.MinimaReader;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.utils.DataTimer;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class ConsensusNet extends ConsensusProcessor {

	/**
	 * Used for the custom Transactions
	 */
	public static final String CONSENSUS_PREFIX 				= "CONSENSUSNET_";
	
	public static final String CONSENSUS_NET_CHECKSIZE_TXPOW 	= CONSENSUS_PREFIX+"NET_MESSAGE_MYTXPOW";
	
	public static final String CONSENSUS_NET_INITIALISE 		= CONSENSUS_PREFIX+"NET_INITIALISE";
	
	public static final String CONSENSUS_NET_INTRO 				= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_INTRO.getValue();
	public static final String CONSENSUS_NET_RESYNC 			= CONSENSUS_PREFIX+"RESYNC";
	
	public static final String CONSENSUS_NET_TXPOWID 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOWID.getValue();
	public static final String CONSENSUS_NET_TXPOWREQUEST		= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOW_REQUEST.getValue();
	public static final String CONSENSUS_NET_TXPOW 				= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOW.getValue();
	
	public static final String CONSENSUS_NET_GREETING 		    = CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_GREETING.getValue();
	
	public static final String CONSENSUS_NET_GREET_FULLINTRO	= CONSENSUS_PREFIX+"GREET_FULLINTRO";
	public static final String CONSENSUS_NET_GREET_RAMSYNCUP	= CONSENSUS_PREFIX+"GREET_RAMSYNCUP";
	public static final String CONSENSUS_NET_GREET_BACKSYNC		= CONSENSUS_PREFIX+"GREET_BACKSYNC";
	
	public static final String CONSENSUS_NET_TXPOWLIST 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_TXPOWLIST.getValue();
	
	public static final String CONSENSUS_NET_PING 				= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_PING.getValue();
	
	public static final String CONSENSUS_NET_GENERIC 			= CONSENSUS_PREFIX+"NET_MESSAGE_"+MinimaReader.NETMESSAGE_GENERIC.getValue();
	
	public static final String CONSENSUS_NET_SYNCOMPLETE 		= CONSENSUS_PREFIX+"NET_MESSAGE_SYNCCOMPLETE";
	
	private static int MAX_TXPOW_LIST_SIZE = 100;
	
	/**
	 * Check when you sent out a request for a TxPOW
	 */
	DataTimer mDataTimer = new DataTimer();
	
	/**
	 * What tip are we syncing to..
	 */
	MiniNumber mCurrentSyncTip = MiniNumber.MINUSONE;
	
	/**
	 * Has the initial Sync been done..
	 */
	public boolean mInitialSync;
	
	public ConsensusNet(MinimaDB zDB, ConsensusHandler zHandler) {
		super(zDB, zHandler);
		
		mInitialSync = false;
	}
	
	public boolean isInitialSyncComplete() {
		return mInitialSync;
	}
	
	public void setInitialSyncComplete() {
		setInitialSyncComplete(true);
	}
	
	public void setInitialSyncComplete(boolean zPostNotify) {
		//if(!mInitialSync) {
			mInitialSync = true;
			if(zPostNotify) {
				getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_INITIALSYNC));	
			}
		//}
	}
	
	private void PostNetClientMessage(Message zOrigMessage, Message zMessage) {
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
				ArrayList<BlockTreeNode> nodes = tree.getAsList();
				
				//Cycle through it all..
				for(BlockTreeNode node : nodes) {
					TxPoW txpow = node.getTxPow();
					MiniNumber block = txpow.getBlockNumber();
					if(block.isMoreEqual(casc)) {
						greet.addBlock(txpow);
					}
				}
			}
			
			//Get the NetClient...
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			
			//And Post it..
			client.PostMessage(new Message(MinimaClient.NETCLIENT_GREETING).addObject("greeting", greet));
			
		/**
		 * You have received the initial Greeting Message	
		 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_GREETING)) {
			//Get the greeting
			Greeting greet = (Greeting)zMessage.getObject("greeting");
			
			//Get the Client..
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			
			//Only allow 0.97 for this..
			if(!greet.getVersion().startsWith("0.98")) {
				MinimaLogger.log("INCOMPATIBLE VERSION ON GREETING "+greet.getVersion()+" MUST BE 0.98");
				MinimaLogger.log("SHUTTING DOWN CONNECTION..");
				
				//Shut down..
				client.noReconnect();
				client.PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
				
				return;
			}
			
			//Are we on the same version
			if(!greet.getVersion().equals(GlobalParams.MINIMA_VERSION)) {
				MinimaLogger.log("DIFFERENT VERSION ON GREETING "+greet.getVersion());
			}
			
			//Are we a new User.. with no Chain.. if so you can do nothing
			if(getMainDB().getMainTree().getAsList().size()==0) {
				return;
			}
			
			//Get the List..
			ArrayList<MiniData> blocks = greet.getList();
			int greetlen = blocks.size();
			
			//This User has NO CHAIN - send him our complete version
			if(greetlen == 0) {
				MinimaLogger.log("Zero length chain sent.. send full intro");
				PostNetClientMessage(zMessage, new Message(CONSENSUS_NET_GREET_FULLINTRO));
				return;
			}
			
			//Find the crossover - if there is one..
			MiniNumber cross = checkCrossover(greet);
			MinimaLogger.log("Greeting Crossover block found : "+cross);
			
			//If there no immediate crossover check backup files..
			if(cross.isEqual(MiniNumber.MINUSONE)) {
				PostNetClientMessage(zMessage, new Message(CONSENSUS_NET_GREET_BACKSYNC).addObject("greet", greet));
//				PostNetClientMessage(zMessage, new Message(CONSENSUS_NET_GREET_BACKSYNC).addObject("greetlist", blocks));
				return;
			}
			
			//Send the complete stack of TxPoW from cross onwards..
			BlockTreeNode top = getMainDB().getMainTree().getChainTip();
			
			//How Many blocks do we need to send..
			int blocklen = top.getBlockNumber().sub(cross).getAsInt(); 
			MinimaLogger.log("BLOCKS TO SEND "+blocklen);
			
			//How long
			int len = greet.getList().size();
			
			//What is the top block..
			MiniNumber greettip = greet.getTopBlock();
			
			//Did we find a crossover..
			if(greettip.isEqual(top.getBlockNumber())) {
				//We are equal..
				getConsensusHandler().PostMessage(CONSENSUS_NET_SYNCOMPLETE);
				
			}else{
				//Set a sync tip..
				if(greettip.isMore(mCurrentSyncTip)) {
					mCurrentSyncTip = greettip;
				}
				MinimaLogger.log("SYNC TO "+mCurrentSyncTip);
				
				//Set the initial sync threshold and fire a timer message just in case..
				getConsensusHandler().PostTimerMessage(new TimerMessage(60000, CONSENSUS_NET_SYNCOMPLETE));
			}
			
			if(blocklen == 0) {
				//Nothing to send..
				return;
			}
			
			//Send this many blocks in full..
			PostNetClientMessage(zMessage, new Message(CONSENSUS_NET_GREET_RAMSYNCUP).addObject("cross", cross));
			
			/**
			 * Send the ENTIRE backup intro message
			 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_GREET_FULLINTRO)) {
			MinimaLogger.log("FIRST TIME SYNC - Sending complete");
			
			//Get the Client..
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			
			//Give them the SYNC 
			client.PostMessage(new Message(MinimaClient.NETCLIENT_INTRO).addObject("syncpackage", getMainDB().getSyncPackage()));
			
			/**
			 * Send every TxPoW Onwards to the User - sync him up in FULL
			 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_GREET_RAMSYNCUP)) {
			//Get the crossover block
			MiniNumber cross = (MiniNumber) zMessage.getObject("cross");
			
			//Send the complete stack of TxPoW from cross onwards..
			BlockTreeNode top = getMainDB().getMainTree().getChainTip();
			
			//How Many blocks do we need to send..
			int blocklen = top.getBlockNumber().sub(cross).getAsInt(); 
			if(blocklen == 0) {
				return;
			}

			MinimaLogger.log("CROSSOVER FOUND!.. SENDING "+blocklen+" FULL BLOCKS");
			
			//Get the Client..
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			
			ArrayList<TxPoW> full_list = new ArrayList<>();
			while(!top.getBlockNumber().isEqual(cross)) {
				//Get the TxPow..
				TxPoW txp = top.getTxPow();
				
				//Check is a full block..
				if(!txp.hasBody()) {
					MinimaLogger.log("CANCEL RESYNC : Attempting to sync user with NoBody Blocks.. "+txp.getBlockNumber());
					
					//Disconnect him..
					client.noReconnect();
					client.PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
					
					return;
				}
				
				//Add this to the list
				full_list.add(0,top.getTxPow());
				
				//Keep going..
				top = top.getParent();
			}
			
			//Now cycle through from the bottom to the top..
			TxPoWList currentblocks = new TxPoWList();
			for(TxPoW blk : full_list) {
				//Add all the TXNS..
				ArrayList<MiniData> txns = blk.getBlockTransactions();
				for(MiniData txn : txns) {
					TxPoW txpow = getMainDB().getTxPOW(txn);
					
					//Check is a full block..
					if(txpow==null) {
						MinimaLogger.log("CANCEL RESYNC : Missing TxPoW in "+blk.getBlockNumber());
						
						//Disconnect him..
						client.noReconnect();
						client.PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
						
						return;
					}
					
					//Add to the list
					currentblocks.addTxPow(txpow);
				}
				
				//Add this TxPoW and the Txns in it..
				currentblocks.addTxPow(blk);
				
				//Check the size..
				if(currentblocks.size() > MAX_TXPOW_LIST_SIZE) {
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
			 * User connected late - send him the min Backups that allow sync but not full check
			 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_GREET_BACKSYNC)) {
			//Get the greeting list
			Greeting greet = (Greeting)zMessage.getObject("greet"); 
//			ArrayList<HashNumber> blocks = (ArrayList<HashNumber>) zMessage.getObject("greetlist");
			
			//Check if we are below..
			MiniNumber mytop = getMainDB().getMainTree().getChainTip().getBlockNumber();
			if(greet.getFirstBlock().isMore(mytop)) {
				MinimaLogger.log("WE ARE BEHIND THEM - NO SYNC.. ");
				return;
			}
			
			//Check if the cascade is an old block of ours..
//			HashNumber startblock = blocks.get(0);
			MiniNumber lowestnum  = greet.getFirstBlock();
			
			//Get the Backup manager where OLD blocks are stored..
			BackupManager backup = Main.getMainHandler().getBackupManager();
			
			//Get the Client..
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			
			//Are they within range
			if(backup.getOldestBackupBlock().isMore(lowestnum)) {
				MinimaLogger.log("TOO FAR BACK TO SYNC THEM.. "+backup.getOldestBackupBlock()+" / "+lowestnum);
				
				//Disconnect him..
				client.noReconnect();
				client.PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
				
				return;
			}
			
			//Otherwise lets load blocks and send them..
			MiniNumber mycasc = getMainDB().getMainTree().getCascadeNode().getBlockNumber();
			int blockstoload = mycasc.sub(lowestnum).getAsInt();
			MinimaLogger.log("SENDING RESYNC ONLY BLOCKS "+blockstoload);
			
			//Create a non-intro syncpackage
			SyncPackage sp = new SyncPackage();
			sp.setCascadeNode(MiniNumber.MINUSONE);
			
			MiniNumber currentblock = lowestnum;
			for(int i=0;i<blockstoload;i++) {
				//Load it.. 
				SyncPacket spack = SyncPacket.loadBlock(backup.getBlockFile(currentblock));
				if(spack == null) {
					//Hmm..
					MinimaLogger.log("SERIOUS ERROR : Blocks not loading from file.. ");
					
					//Add it..
					sp.getAllNodes().add(spack);
					
					//Can't sync him.. Disconnect him..
					client.noReconnect();
					client.PostMessage(new Message(MinimaClient.NETCLIENT_SHUTDOWN));
					
					return;
				}
				
				//Add it..
				sp.getAllNodes().add(spack);
				
				//increment
				currentblock = currentblock.increment();
			}
			
			//And send it..
			client.PostMessage(new Message(MinimaClient.NETCLIENT_INTRO).addObject("syncpackage", sp));
			
			//And the rest.. ignoring the cascade nodes..
			sp   = getMainDB().getSyncPackage();
			sp.setCascadeNode(MiniNumber.MINUSONE);
			client.PostMessage(new Message(MinimaClient.NETCLIENT_INTRO).addObject("syncpackage", sp));
			
		/**
		 * You have A CHAIN and this is your resync message from way back
		 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_RESYNC)) {
			//Get the Sync Package..
			SyncPackage sp = (SyncPackage) zMessage.getObject("sync");
			BackupManager backup = Main.getMainHandler().getBackupManager();
			
			MiniNumber casc = getMainDB().getMainTree().getCascadeNode().getBlockNumber();
			MiniNumber tip  = getMainDB().getMainTree().getChainTip().getBlockNumber();
			
			MinimaLogger.log("RESYNC MESSAGE RECEIVED! mycasc:"+casc+" mytip:"+tip);
			
			//Drill down 
			ArrayList<SyncPacket> packets = sp.getAllNodes();
			for(SyncPacket spack : packets) {
				TxPoW txpow = spack.getTxPOW();
				MMRSet mmr  = spack.getMMRSet();
				
				//Check is above MY Cascade..
				if(!txpow.getBlockNumber().isMore(casc)) {
					//MinimaLogger.log("SKIP UNEEDED BLOCK PAST CASCADE "+txpow.getBlockNumber());
					continue;
				}
				
				//Could be an older cascade block
				if(mmr==null) {
					//MinimaLogger.log("NULL MMR ON RESYNC BLOCK"+txpow.getBlockNumber());
					continue;
				}
				
				//Store it..
				backup.backupTxpow(txpow);
				
				//Add to the list
				TxPOWDBRow row = getMainDB().getTxPowDB().addTxPOWDBRow(txpow);
				row.setMainChainBlock(true);
				row.setIsInBlock(true);
				row.setInBlockNumber(txpow.getBlockNumber());
				row.setBlockState(TxPOWDBRow.TXPOWDBROW_STATE_FULL);
				
				//Get the Parent node..
				BlockTreeNode parent = getMainDB().getMainTree().findNode(txpow.getParentID(), true);
				
				//DO we have it..
				if(parent == null) {
					MinimaLogger.log("ERROR : NULL PARENT IN RESYNC.. .. "+txpow.getBlockNumber());
					return;
				}
				
				//Now create a new Node.. 
				BlockTreeNode node = new BlockTreeNode(txpow);
				node.setCascade(false);
				node.setState(BlockTreeNode.BLOCKSTATE_VALID);
				
				//Set the MMR
				node.setMMRset(mmr);
				
				//Set the MMR parent..
				mmr.setParent(parent.getMMRSet());
				
				//Add to the Parent..
				parent.addChild(node);
				
				//Scan for coins..
				getMainDB().scanMMRSetForCoins(mmr);
			}
			
			finishUpSync();
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_INTRO)) {
			//Get the Sync Package..
			SyncPackage sp = (SyncPackage) zMessage.getObject("sync");
			
			//Is this a resync message..
			if(sp.getCascadeNode().isEqual(MiniNumber.MINUSONE)) {
				PostNetClientMessage(zMessage, new Message(CONSENSUS_NET_RESYNC).addObject("sync", sp));
				return;
			}
			
			//Only do this if you have no chain..
			if(getMainDB().getMainTree().getAsList().size()!=0) {
				MinimaLogger.log("ERROR : INTRO SYNC message received.. even though I HAVE a chain..");
				return;
			}
			
			//We'll be storing the received txpow messages
			BackupManager backup = Main.getMainHandler().getBackupManager();
			
			//Clear the database..
			getMainDB().getMainTree().clearTree();
//			getMainDB().getCoinDB().clearDB();
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
				
				//Add it to the DB..
				BlockTreeNode node = getMainDB().hardAddTxPOWBlock(txpow, mmr, spack.isCascade());
				
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
				
			
			finishUpSync();
			
			/**
			 * Has the Sync Completed..
			 */
		}else if(zMessage.isMessageType(CONSENSUS_NET_SYNCOMPLETE)) {
			if(!mInitialSync) {
				MinimaLogger.log("Initial Sync Complete..");
			}
			setInitialSyncComplete();
			
		/**
		 * You have received multiple TxPoW messages 	
		 */
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWLIST)) {
			TxPoWList block = (TxPoWList)zMessage.getObject("txpowlist"); 
			
			//Cycle through.. and Post as normal..
			ArrayList<TxPoW> txps = block.getList();
			for(TxPoW txp : txps) {
				PostNetClientMessage(zMessage, new Message(CONSENSUS_NET_TXPOW).addObject("txpow", txp).addObject("sync", true));
			
				//Have we reached the sync tip..
				if(txp.getBlockNumber().isMoreEqual(mCurrentSyncTip)) {
					//We are equal..
					MinimaLogger.log("SYNC TIP HIT!!");
					getConsensusHandler().PostMessage(CONSENSUS_NET_SYNCOMPLETE);
				}
			}
			
		/**
		 * A TxPoWID message from a client.. do you need it ?	
		 */
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWID)) {
			//Get the ID
			MiniData txpowid = (MiniData) zMessage.getObject("txpowid");
			
			//Do we have it..
			if(getMainDB().getTxPOW(txpowid) == null) {
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
				//Do we have a body.. or is it a very old txn..
				if(!txpow.hasBody()) {
					MinimaLogger.log("NET TXPOWREQUEST OF TXPOW WITH NO BODY ( TOO OLD )"+txpowid);
					return;
				}
				
				//Send it to him..
				MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
				Message tx = new Message(MinimaClient.NETCLIENT_SENDTXPOW).addObject("txpow", txpow);
				client.PostMessage(tx);
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_NET_PING)) {
			//Send it on to the netwclient..
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			client.PostMessage(new Message(MinimaClient.NETCLIENT_PING));
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_GENERIC)) {
			MinimaLogger.log("GENERIC NET MESSAGE : "+zMessage);
		
		}else if(zMessage.isMessageType(CONSENSUS_NET_CHECKSIZE_TXPOW)) {
			//Internal message sent from you..
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
			
			//DO a basic check..
			if(!TxPoWChecker.basicTxPowChecks(txpow)) {
				MinimaLogger.log("ERROR - You've Mined A TxPoW that fails basic checks!");
				return;
			}
			
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
			//Is this a sync message
			boolean syncmessage = false;
			if(zMessage.exists("sync")) {
				syncmessage = true;
			}
			
			/**
			 * The SINGLE entry point into the system for NEW TXPOW messages..
			 */
			//Have we done the initial SYNC..
			if(!mInitialSync && !syncmessage) {
				MinimaLogger.log("NET TxPoW received before Initial Sync Finished.");
				return;
			}
			
			//The TxPoW
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
			
			//First check that this is WITHIN acceptable linits .. so not too far ahead of the current chain..
			MiniNumber timetip   = getMainDB().getMainTree().getChainTip().getTxPow().getBlockNumber();
			MiniNumber timeblock = txpow.getBlockNumber();
			if(timeblock.sub(timetip).isMore(GlobalParams.MINIMA_CASCADE_START_DEPTH)) {
				MinimaLogger.log("NET Transaction TOO FAR IN THE FUTURE.. new:"+timeblock+" / current:"+timetip);
				return;
			}
			
			//Do we have it.. now check DB - hmmm..
			if(getMainDB().getTxPOW(txpow.getTxPowID()) != null) {
				MinimaLogger.log("NET Transaction we already have.. "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				return;
			}
			
			/**
			 * IT PASSES! - process..
			 */
			Message newtxpow = new Message(ConsensusHandler.CONSENSUS_PROCESSTXPOW).addObject("txpow", txpow).addObject("sync", syncmessage);
			getConsensusHandler().PostMessage(newtxpow);
			
			//if this is a sync message no need to check.. the details will be coming..
			if(!syncmessage) {
				//Now check we have the parent.. and txns..
				if(txpow.isBlock()) {
					MiniData parentID = txpow.getParentID();
					if(getMainDB().getTxPOW(parentID) == null) {
						//We don't have it, get it..
						MinimaLogger.log("Request Parent TxPoW @ "+txpow.getBlockNumber()+" parent:"+parentID); 
						sendTxPowRequest(zMessage, parentID);
					}
				
					//And now check the Txn list..
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
	}
	
	/**
	 * Send a Request for a Missing TxPOW
	 * Check if has been done recently and reposts with a 5 second delay if it has
	 */
	public void sendTxPowRequest(MiniData zTxPoWID) {
		//Asks ALL the clients..
		ArrayList<MinimaClient> allclients = getNetworkHandler().getNetClients();
		for(MinimaClient client : allclients) {
			sendTxPowRequest(client,zTxPoWID);
		}
	}
	
	public void sendTxPowRequest(Message zFromMessage, MiniData zTxPoWID) {
		//Get the NetClient...
		MinimaClient client = (MinimaClient) zFromMessage.getObject("netclient");
		sendTxPowRequest(client, zTxPoWID);
	}
	
	public void sendTxPowRequest(MinimaClient zClient, MiniData zTxPoWID) {
		//Don't ask for 0x00..
		if(zTxPoWID.isEqual(MiniData.ZERO_TXPOWID)) {
			//it's the genesis..
			return;
		}
		
		//Check if we have sent off for it recently..
		String data  = zTxPoWID.to0xString();
		
		//Check for it.. in last 5 seconds..
		boolean found = mDataTimer.checkForData(data, 5000);
		
		//If found.. repost the request on a 5 second timer..
		if(found) {
			//Wait 10 seconds before trying again..
			TimerMessage newtxpowid = new TimerMessage(10000, CONSENSUS_NET_TXPOWID);
			//Add the TxPOWID
			newtxpowid.addObject("txpowid", zTxPoWID);
			//And the Net Client..
			newtxpowid.addObject("netclient", zClient);
			//Post it for later..
			getConsensusHandler().PostTimerMessage(newtxpowid);
			return;
		}
		
		//Add it to the list of requested..
		getNetworkHandler().addRequestedTxPow(zTxPoWID.to0xString());
				
		//Give it to the client to send on..	
		Message req = new Message(MinimaClient.NETCLIENT_SENDTXPOWREQ);
		req.addObject("txpowid", zTxPoWID);
		
		//And Post it..
		zClient.PostMessage(req);
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
		ArrayList<MiniData> introchain = zGreeting.getList();
		int len = introchain.size();
		if(len == 0) {
			return MiniNumber.MINUSONE;
		}
		
		//The external node..
		MiniNumber introtip     = zGreeting.getTopBlock();
		MiniNumber introcascade = zGreeting.getFirstBlock();
	
		MinimaLogger.log("GREETING mytip:"+maintip+" mycascade:"+maincascade+" greetingtip:"+introtip+" greetingcascade:"+introcascade);
		
		boolean found        = false;
		MiniNumber crossover = MiniNumber.MINUSONE;
		
		//Simple checks first..
		if(introtip.isLess(maincascade) || maintip.isLess(introcascade)) {
			return crossover;
		}
		
		//Cycle..
		for(BlockTreeNode block : chain) {
			//BLock number and hash.. BOTH have to match
			MiniNumber bnum  = block.getTxPow().getBlockNumber();
			
			//only use nodes after our cascade..
			if(bnum.isMore(maincascade)) {
				//Run through the intro chain..
				MiniNumber snum = zGreeting.getTopBlock();
				for(MiniData introtxpowid : introchain) {
					if(snum.isMore(introcascade)) {
						if(snum.isEqual(bnum)) {
							//Only check for hash now..
							if(introtxpowid.isEqual(block.getTxPowID())) {
								found     = true;
								crossover = bnum;
								break;
							}
							
						}else if(snum.isLess(bnum)) {
							//no chance of collision..
							break;
						}
					}
					
					//One down..
					snum = snum.decrement();
				}
			}
		
			if(found) {
				break;
			}
		}
		
		//Return what we found..
		return crossover;
	}
	
	/**
	 * When you finish a Sync Up.. 
	 */
	private void finishUpSync() {
		//Reset weights
		getMainDB().getMainTree().resetWeights();
		
		//Now correect the TxPoWDB
		getMainDB().resetAllTxPowOnMainChain();
		
		//And finally remove any unwanted TxPoW.. ( they will ALL be on the main chain)
		getMainDB().getTxPowDB().removeAllUnused();
		
		//Cascade..
		getMainDB().cascadeTheTree();
		
		//FOR NOW
		TxPoW tip = getMainDB().getMainTree().getChainTip().getTxPow();
		MinimaLogger.log("Initial Sync Complete.. Reset Current block : "+tip.getBlockNumber());
	
		//Do the balance.. Update listeners if changed..
		getConsensusHandler().PostMessage(new Message(ConsensusPrint.CONSENSUS_BALANCE).addBoolean("hard", true));
		
		//Post a message to those listening
		getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_NEWBLOCK).addObject("txpow", tip));
		
		//Backup the system..
		getConsensusHandler().PostTimerMessage(new TimerMessage(2000,ConsensusBackup.CONSENSUSBACKUP_BACKUP));
	
		//Sync complete
		getConsensusHandler().PostMessage(CONSENSUS_NET_SYNCOMPLETE);
	}
	
	
}
