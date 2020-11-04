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
	
	boolean mFullSyncOnInit = true;
	
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
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			Message req      = new Message(MinimaClient.NETCLIENT_GREETING).addObject("greeting", greet);
			
			//And Post it..
			client.PostMessage(req);
			
		}else if(zMessage.isMessageType(CONSENSUS_NET_INTRO)) {
			//MinimaLogger.log("INTRO SYNC message received..");
			
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
						setInitialSyncComplete();
						return;
					}
					
					if(mHardResetAllowed) {
						hardreset = true;
						MinimaLogger.log("HARD RESETTING.. ");
					}else {
						MinimaLogger.log("NO HARD RESET ALLOWED.. ");
						hardreset = false;
						setInitialSyncComplete();
						return;
					}
				}
			}
			
			//We'll be storing the received txpow messages
			BackupManager backup = Main.getMainHandler().getBackupManager();
			
			//Complete Refresh..
			if(hardreset) {
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
				MinimaLogger.log("Sync Complete.. Reset Current block : "+tip.getBlockNumber());
			
				//Do the balance.. Update listeners if changed..
				getConsensusHandler().PostMessage(new Message(ConsensusPrint.CONSENSUS_BALANCE).addBoolean("hard", true));
				
				//Post a message to those listening
				getConsensusHandler().updateListeners(new Message(ConsensusHandler.CONSENSUS_NOTIFY_NEWBLOCK).addObject("txpow", tip));
				
				//Backup the system..
				getConsensusHandler().PostTimerMessage(new TimerMessage(2000,ConsensusBackup.CONSENSUSBACKUP_BACKUP));
				
				//Now the Initial SYNC has been done you can receive TXPOW message..
				setInitialSyncComplete(false);
				
				//Do you want a copy of ALL the TxPoW in the Blocks.. ? Only really useful for txpowsearch - DEXXED
				if(mFullSyncOnInit) {
					//The total list..
					ArrayList<String> checklist = new ArrayList<>();
					
					//Create a TxPOWIDlist of the requested TxPOW..
					TxPoWIDList txpidlist = new TxPoWIDList();
					
					//Get the Client..
					MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
					
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
								//Check for it..
								String checker = txn.to0xString();
								if(checklist.contains(checker)) {
									//We have already sent for it..
									continue;
								}
								
								//Add it to the list
								getNetworkHandler().addRequestedInitialSyncTxPow(txn.to0xString());
								
								//Add it..
								checklist.add(checker);
								
								//We don't have it, get it..
								txpidlist.addTxPowID(txn);
							
								//Have we reached the limit..
								if(txpidlist.size() > MAX_TXPOW_LIST_SIZE) {
									//Send it..
									Message req = new Message(MinimaClient.NETCLIENT_TXPOWIDLIST).addObject("txpowidlist", txpidlist);
									client.PostMessage(req);
											
									//Reset..
									txpidlist = new TxPoWIDList();
								}

								//Total requests made
								reqtxn++;
							}
						}
					}
					
					//And finally.. any left..
					if(txpidlist.size() > 0) {
						//Send it..
						Message req = new Message(MinimaClient.NETCLIENT_TXPOWIDLIST).addObject("txpowidlist", txpidlist);
						client.PostMessage(req);
					}
					
					if(reqtxn>0) {
						MinimaLogger.log("Requested "+reqtxn+" transactions in Initial Blocks..");	
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
				MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
				Message req      = new Message(MinimaClient.NETCLIENT_INTRO).addObject("syncpackage", sp);
				client.PostMessage(req);
				return;
			}
			
			MiniNumber cross = checkCrossover(greet);
			if(cross.isEqual(MiniNumber.MINUSONE)) {
				MinimaLogger.log("NO CROSSOVER - Sending complete");
				//Get the complete sync package - deep copy.. 
				SyncPackage sp = getMainDB().getSyncPackage(true);
				MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
				Message req      = new Message(MinimaClient.NETCLIENT_INTRO).addObject("syncpackage", sp);
				client.PostMessage(req);
				return;
			}
			
			//Get the tip..
			MiniData top   = blocks.get(greetlen-1).getHash();
			MiniNumber len = blocks.get(greetlen-1).getNumber().sub(cross);
			
			if(len.getAsInt() == 0) {
				setInitialSyncComplete();
				return;
			}else {
				MinimaLogger.log("CROSSOVER FOUND Requesting from "+cross+" to "+blocks.get(greetlen-1).getNumber());	
			}
			
			//Ask for Just the required Blocks..
			HashNumber hn = new HashNumber(top, len);
			
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			Message req      = new Message(MinimaClient.NETCLIENT_GREETING_REQ).addObject("hashnumber", hn);
			client.PostMessage(req);
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_GREETING_REQUEST)) {
			//Get the details
			HashNumber hashnum = (HashNumber)zMessage.getObject("hashnumber");
			int max = hashnum.getNumber().getAsInt();
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			
			TxPoWList txpowlist = new TxPoWList();
			txpowlist.setCrossOver(true);
			int counter = 0;
			
			BlockTreeNode top = getMainDB().getMainTree().findNode(hashnum.getHash());
			while(top!=null && counter<max) {
				//Get the Block TxPoW
				TxPoW block = top.getTxPow();
				
				//Add this TxPoW and the Txns in it..
				txpowlist.addTxPow(block);
				
				//Add all the TXNS as well..
				ArrayList<MiniData> txns = block.getBlockTransactions();
				for(MiniData txn : txns) {
					TxPoW txpow = getMainDB().getTxPOW(txn);
					if(txpow!=null) {
						txpowlist.addTxPow(txpow);
					}
				}
				
				//Now check if we are at the limit..
				if(txpowlist.size() > MAX_TXPOW_LIST_SIZE) {
					//Send this on and start a new list..
					client.PostMessage(new Message(MinimaClient.NETCLIENT_TXPOWLIST).addObject("txpowlist", txpowlist));
					
					//Create a new list
					txpowlist = new TxPoWList();
					txpowlist.setCrossOver(true);
				}
				
				//Get the next block..
				top = top.getParent();
				counter++;
			}
			
			if(txpowlist.size() > 0) {
				//Now send that..!
				client.PostMessage(new Message(MinimaClient.NETCLIENT_TXPOWLIST).addObject("txpowlist", txpowlist));	
			}
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWIDLIST)) {
			//Get the List of requested TxPoW
			TxPoWIDList txpidlist = (TxPoWIDList)zMessage.getObject("txpowidlist");
			
			//Now get all the txp
			TxPoWList txpowlist = new TxPoWList();
			txpowlist.setCrossOver(false);
			
			ArrayList<MiniData> list = txpidlist.getList();
			for(MiniData txpid : list) {
				//Get the TxPOW
				TxPoW txp = getMainDB().getTxPOW(txpid);
				
				//Do we have it..
				if(txp != null) {
					txpowlist.addTxPow(txp);
				}
			}
			
			//Now send that..!
			MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
			client.PostMessage(new Message(MinimaClient.NETCLIENT_TXPOWLIST).addObject("txpowlist", txpowlist));
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWLIST)) {
			TxPoWList txplist     = (TxPoWList)zMessage.getObject("txpowlist"); 
			ArrayList<TxPoW> txps = txplist.getList();
			
			boolean firsttime = isInitialSyncComplete();
			
			//Now the Initial SYNC has been done you can receive TXPOW message..
			setInitialSyncComplete(false);
			
			//Is this an Initial request
			boolean initial = false;
			
			//Cycle through..
			for(TxPoW txp : txps) {
				boolean isvalid = false;
				if(getNetworkHandler().isRequestedInitialTxPow(txp.getTxPowID().to0xString())) {
					isvalid = true;
					initial = true;
				}
				
				//Not immediately valid - as not from the initial IBD list
				processIBDTxPoW(txp, isvalid);
			}
			
			//Do this ONLY if initial messages - since they are considered valid
			if(initial) {
				//And NOW sort the Tree..
				ArrayList<BlockTreeNode> list = getMainDB().getMainTree().getAsList(true);
				for(BlockTreeNode treenode : list) {
					//Get the Block
					TxPoW txpow = treenode.getTxPow();
			
					//Get the database txpow..
					TxPOWDBRow trow = getMainDB().getTxPOWRow(txpow.getTxPowID());
					if(trow != null) {
						//What Block
						MiniNumber block = txpow.getBlockNumber();
						
						//Set the details
						trow.setMainChainBlock(true);
						trow.setIsInBlock(true);
						trow.setInBlockNumber(block);
						
						//Now the Txns..
						ArrayList<MiniData> txpowlist = txpow.getBlockTransactions();
						for(MiniData txid : txpowlist) {
							trow = getMainDB().getTxPOWRow(txid);
							if(trow!=null) {
								//Set that it is in this block
								trow.setMainChainBlock(false);
								trow.setIsInBlock(true);
								trow.setInBlockNumber(block);
							}
						}
					}
				}
			}
			
			//Now Perform 1 update..
			getMainDB().processTxPOW(null);
			
//			if(txplist.isCrossover()) {
//				//Treat as normal TxPOW messages.. checking everything..
//				for(TxPoW txp : txps) {
//					//Not immediately valid - as not from the initial IBD list
//					processIBDTxPoW(txp, false);
//				}
//				
//			}else {
//				//Cycle through and process as if IBD data..
//				for(TxPoW txp : txps) {
//					processIBDTxPoW(txp, true);
//				}
//				
//				//And NOW sort the Tree..
//				ArrayList<BlockTreeNode> list = getMainDB().getMainTree().getAsList(true);
//				for(BlockTreeNode treenode : list) {
//					//Get the Block
//					TxPoW txpow = treenode.getTxPow();
//			
//					//Get the database txpow..
//					TxPOWDBRow trow = getMainDB().getTxPOWRow(txpow.getTxPowID());
//					if(trow != null) {
//						//What Block
//						MiniNumber block = txpow.getBlockNumber();
//						
//						//Set the details
//						trow.setMainChainBlock(true);
//						trow.setIsInBlock(true);
//						trow.setInBlockNumber(block);
//						
//						//Now the Txns..
//						ArrayList<MiniData> txpowlist = txpow.getBlockTransactions();
//						for(MiniData txid : txpowlist) {
//							trow = getMainDB().getTxPOWRow(txid);
//							if(trow!=null) {
//								//Set that it is in this block
//								trow.setMainChainBlock(false);
//								trow.setIsInBlock(true);
//								trow.setInBlockNumber(block);
//							}
//						}
//					}
//				}
//			}
			
			//Do a complete backup..
			if(firsttime) {
				getConsensusHandler().PostTimerMessage(new TimerMessage(20000,ConsensusBackup.CONSENSUSBACKUP_BACKUP));
			}
			
		}else if ( zMessage.isMessageType(CONSENSUS_NET_TXPOWID)) {
			//Get the ID
			MiniData txpowid = (MiniData) zMessage.getObject("txpowid");
			
			//Do we have it..
			if(getMainDB().getTxPOW(txpowid) == null) {
				//MinimaLogger.log("NEW TXPOWID "+txpowid.to0xString()+" from "+zMessage.getObject("netclient"));
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
				MinimaClient client = (MinimaClient) zMessage.getObject("netclient");
				
				//Send it to him..
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
			if(!mInitialSync) {
				MinimaLogger.log("NET TxPoW received before Initial Sync Finished.");
				return;
			}
			
			//The TxPoW
			TxPoW txpow = (TxPoW)zMessage.getObject("txpow");
		
			//DEBUG logs..
			//MinimaLogger.log("TXPOW RECEIVED "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
			
			//Do we have it.. now check DB - hmmm..
			if(getMainDB().getTxPOW(txpow.getTxPowID()) != null) {
				MinimaLogger.log("NET Transaction we already have.. "+txpow.getBlockNumber()+" "+txpow.getTxPowID());
				return;
			}
			
			//Is it even a valid TxPOW.. not enough POW ? - FIRST CHECK
			if(!txpow.isBlock() && !txpow.isTransaction()) {
				MinimaLogger.log("ERROR NET FAKE - not transaction not block : "+txpow.getBlockNumber()+" "+txpow);
				return;
			}
			
			//Is the Transaction PoWerful enough..
			if(txpow.isTransaction()) {
				if(txpow.getTxnDifficulty().isMore(TxPoWMiner.BASE_TXN)) {
					MinimaLogger.log("ERROR NET - Transaction not enough TxPOW: "+txpow.getTxnDifficulty()+" "+txpow);
					return;
				}
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
			
			//Check the Signatures.. just the once..
			boolean sigsok = TxPoWChecker.checkSigs(txpow);
			if(!sigsok) {
				MinimaLogger.log("ERROR NET Invalid Signatures with TXPOW : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
				return;
			}

			//Get the TxPowID..
			String txpowid = txpow.getTxPowID().to0xString();
			
			//Was it requested anyway.. ?
			boolean requested = false;
			if(getNetworkHandler().isRequestedTxPow(txpowid)) {
				requested = true;
			}
			
			//Remove it from the list - just in case..
			getNetworkHandler().removeRequestedTxPow(txpowid);
			
			//Check the Validity..
			boolean txnok = TxPoWChecker.checkTransactionMMR(txpow, getMainDB());
			if(!txnok) {
				//Was it requested.. ?
				if(requested) {
					//Ok - could be from a different branch block.. 
					MinimaLogger.log("WARNING NET Invalid TXPOW (Requested..) : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
				}else {
					//Not requested invalid transaction..
					MinimaLogger.log("ERROR NET Invalid TXPOW (UN-Requested..) : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
					return;	
				}
			}
			
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
	 * Process a TxPoW requested at startup..
	 * 
	 * @param zTxPow
	 */
	private void processIBDTxPoW(TxPoW zTxPoW, boolean isAllreadyValid) {
		//Check some basics..
		if(!isAllreadyValid) {
			//Check if we have it.
			if(getMainDB().getTxPOW(zTxPoW.getTxPowID()) != null) {
				MinimaLogger.log("IBD : NET Transaction we already have.. "+zTxPoW.getBlockNumber()+" "+zTxPoW.getTxPowID());
				return;
			}
			
			//Is it even a valid TxPOW.. not enough POW ? - FIRST CHECK
			if(!zTxPoW.isBlock() && !zTxPoW.isTransaction()) {
				MinimaLogger.log("IBD : ERROR NET FAKE - not transaction not block : "+zTxPoW.getBlockNumber()+" "+zTxPoW);
				return;
			}
			
			//Is the Transaction PoWerful enough..
			if(zTxPoW.isTransaction()) {
				if(zTxPoW.getTxnDifficulty().isMore(TxPoWMiner.BASE_TXN)) {
					MinimaLogger.log("IBD : ERROR NET - Transaction not enough TxPOW: "+zTxPoW.getTxnDifficulty()+" "+zTxPoW);
					return;
				}
			}
			
			//Does it have a body.. SHOULD NOT HAPPEN as only complete post cascade txpow messages can be requested
			if(!zTxPoW.hasBody()) {
				MinimaLogger.log("IBD : ERROR NET NO TxBODY for txpow "+zTxPoW.getBlockNumber()+" "+zTxPoW.getTxPowID());
				return;
			}
			
			//Check Header and Body Agree..
			MiniData bodyhash = Crypto.getInstance().hashObject(zTxPoW.getTxBody());
			if(!zTxPoW.getTxHeader().getBodyHash().isEqual(bodyhash)) {
				MinimaLogger.log("IBD : ERROR NET TxHeader and TxBody Mismatch! "
							+zTxPoW.getBlockNumber()+" "+zTxPoW.getTxPowID()+" "+zTxPoW.getTxHeader().getBodyHash().to0xString()+" "+bodyhash.to0xString()); 
				return;
			}
			
			//Check the Signatures.. just the once..
			boolean sigsok = TxPoWChecker.checkSigs(zTxPoW);
			if(!sigsok) {
				MinimaLogger.log("IBD : ERROR NET Invalid Signatures with TXPOW : "+zTxPoW.getBlockNumber()+" "+zTxPoW.getTxPowID()); 
				return;
			}
			
			//Only do this once..
			boolean relevant = false;
			if(zTxPoW.isTransaction()) {
				//Is it relevant to us..
				relevant = getMainDB().getUserDB().isTransactionRelevant(zTxPoW.getTransaction());
			
				//Store it.. ?
				if(relevant) {
					//Get the Token Amounts..
					Hashtable<String, MiniNumber> tokamt = getMainDB().getTransactionTokenAmounts(zTxPoW);
					
					//Store ion the database..
					getMainDB().getUserDB().addToHistory(zTxPoW,tokamt);
				}
			}
			
			//Send a message to all about a new TxPoW (may or may not be a transaction or a block..)
			JSONObject newtxpow = new JSONObject();
			newtxpow.put("event","newtxpow");
			newtxpow.put("txpow",zTxPoW.toJSON());
			newtxpow.put("relevant",relevant);
			getConsensusHandler().PostDAPPJSONMessage(newtxpow);
		}
		
		//OK - it passes a general test.. add it to the database..
		TxPOWDBRow row = getMainDB().addNewTxPow(zTxPoW);
		
		if(isAllreadyValid) {
			//Add all the tokens..
			TokenProof tokp = zTxPoW.getTransaction().getTokenGenerationDetails();
			if(tokp!=null) {
				getMainDB().getUserDB().addTokenDetails(tokp);
			}
			
			ArrayList<TokenProof> tokens =  zTxPoW.getWitness().getAllTokenDetails();
			for(TokenProof tp : tokens) {
				getMainDB().getUserDB().addTokenDetails(tp);
			}
		}
		
		//Save it..
		Main.getMainHandler().getBackupManager().backupTxpow(zTxPoW);
		
		//Is it a block ?
		if(zTxPoW.isBlock()) {
			//Add all the children
			if(getMainDB().getMainTree().addNode(new BlockTreeNode(zTxPoW))) {
				getMainDB().addTreeChildren(zTxPoW.getTxPowID());
			}
		}

		//And remove the link..
		getNetworkHandler().removeRequestedTxPow(zTxPoW.getTxPowID().to0xString());
				
		//OLD SLOW METHOD>>
//		//Check the block it is in..
//		TxPoW validblock = getMainDB().findBlockForTransaction(txpow);
//		if(validblock != null) {
//			//Add it to the database..
//			TxPOWDBRow row = getMainDB().addNewTxPow(txpow);
//			row.setMainChainBlock(false);
//			row.setIsInBlock(true);
//			row.setInBlockNumber(validblock.getBlockNumber());
//			
//			//Add all the tokens..
//			TokenProof tokp = txpow.getTransaction().getTokenGenerationDetails();
//			if(tokp!=null) {
//				getMainDB().getUserDB().addTokenDetails(tokp);
//			}
//			
//			ArrayList<TokenProof> tokens =  txpow.getWitness().getAllTokenDetails();
//			for(TokenProof tp : tokens) {
//				getMainDB().getUserDB().addTokenDetails(tp);
//			}
//			
//			//Save it..
//			Main.getMainHandler().getBackupManager().backupTxpow(txpow);
//			
//			//Is it a block ?
//			if(txpow.isBlock()) {
//				//Add all the children
//				if(getMainDB().getMainTree().addNode(new BlockTreeNode(txpow))) {
//					getMainDB().addTreeChildren(txpow.getTxPowID());
//				}
//				
//				//And now check the Txn list..
//				ArrayList<MiniData> txns = txpow.getBlockTransactions();
//				for(MiniData txn : txns) {
//					if(getMainDB().getTxPOW(txn) == null ) {
//						MinimaLogger.log("Request missing TxPoW in IBD block "+txpow.getBlockNumber()+" "+txn);
//						sendTxPowRequest(zMessage, txn);
//					}
//				}
//			}
//		}else {
//			MinimaLogger.log("WARNING NET IBD TXPOW request block not found : "+txpow.getBlockNumber()+" "+txpow.getTxPowID()); 
//		}
//		
//		//And remove the link..
//		getNetworkHandler().removeRequestedTxPow(txpowid);
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
