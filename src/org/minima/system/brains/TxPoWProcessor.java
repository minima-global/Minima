package org.minima.system.brains;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.IBD;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Stack;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class TxPoWProcessor extends MessageProcessor {
	
	private static final String TXPOWPROCESSOR_PROCESSTXPOW 		= "TXP_PROCESSTXPOW";
	private static final String TXPOWPROCESSOR_PROCESS_IBD 			= "TXP_PROCESS_IBD";
	private static final String TXPOWPROCESSOR_PROCESS_SYNCIBD 		= "TXP_PROCESS_SYNCIBD";
	private static final String TXPOWPROCESSOR_PROCESS_ARCHIVEIBD 	= "TXP_PROCESS_ARCHIVEIBD";
	
	public TxPoWProcessor() {
		super("TXPOWPROCESSOR");
	}
	
	/**
	 * Main entry point for a TxPoW into the system
	 */
	public void postProcessTxPoW(TxPoW zTxPoW) {
		//Add / Update last access to the DB
		MinimaDB.getDB().getTxPoWDB().addTxPoW(zTxPoW);
		
		//Post a message on the single threaded stack
		PostMessage(new Message(TXPOWPROCESSOR_PROCESSTXPOW).addObject("txpow", zTxPoW));
	}
	
	/**
	 * Main Entry point for IBD messages
	 */
	public void postProcessIBD(IBD zIBD, String zClientUID) {
		//Post a message on the single threaded stack
		PostMessage(new Message(TXPOWPROCESSOR_PROCESS_IBD).addObject("ibd", zIBD).addString("uid", zClientUID));
	}
	
	/**
	 * Main Entry point for Sync IBD messages
	 */
	public void postProcessSyncIBD(IBD zIBD, String zClientUID) {
		//Post a message on the single threaded stack
		PostMessage(new Message(TXPOWPROCESSOR_PROCESS_SYNCIBD).addObject("ibd", zIBD).addString("uid", zClientUID));
	}
	
	/**
	 * Main Entry point for Archive IBD messages
	 */
	public void postProcessArchiveIBD(IBD zIBD, String zClientUID) {
		//Post a message on the single threaded stack
		PostMessage(new Message(TXPOWPROCESSOR_PROCESS_ARCHIVEIBD).addObject("ibd", zIBD).addString("uid", zClientUID));
	}
	
	/**
	 * Main TxPoW process function
	 *  
	 * @param zTxPoW
	 */
	private void processTxPoW(TxPoW zTxPoW) {
		
		//Has something on tree changed
		boolean recalculate = false;
		
		//Fast access DB
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
		Cascade	cascdb		= MinimaDB.getDB().getCascade();
		
		//Process a stack of TxPoW if necessary
		Stack processstack = new Stack();
		processstack.push(zTxPoW);
		
		//Now work through all the required blocks
		while(!processstack.isEmpty()) {
		
			//Get the next txpow
			TxPoW txpow = (TxPoW) processstack.pop();
			
			//Check we are at least enough blocks on from the root of the tree.. for speed and difficulty calcs
			MiniNumber blknum 	= txpow.getBlockNumber();
			MiniNumber rootnum 	= txptree.getRoot().getBlockNumber();
			boolean validrange 	= blknum.isMore(rootnum);
			
			//Is it a block.. that is the only time we crunch
			if(txpow.isBlock() && validrange) {
				
				//Check not already added
				TxPoWTreeNode oldnode = txptree.findNode(txpow.getTxPoWID());
				if(oldnode == null) {
					
					//Is there a valid parent block node..
					TxPoWTreeNode parentnode = txptree.findNode(txpow.getParentID().to0xString());
					if(parentnode != null) {
					
						//Do we have all the transactions in the block
						ArrayList<String> txns 		= txpow.getTransactions();
						int numtxns 				= txns.size();
						ArrayList<TxPoW> alltrans 	= txpdb.getAllTxPoW(txns);
						
						//Do we have them all
						if(alltrans.size() == numtxns) {
						
							//OK - Lets check this block
							if(TxPoWChecker.checkTxPoWBlock(parentnode, txpow, alltrans)) {
								
								//Create a TxBlock..
								TxBlock txblock = new TxBlock(parentnode.getMMR(), txpow, alltrans);
								
								//Create a new node
								TxPoWTreeNode newblock = new TxPoWTreeNode(txblock);
								
								//Lets add it to the tree
								parentnode.addChildNode(newblock);
								
								//Add fast link in tree - otherwise only reset / added when recalculate tree is called
								txptree.addFastLink(newblock);
								
								//we need to recalculate the Tree
								recalculate = true;
								
								//Do we have children for this block
								ArrayList<TxPoW> children = txpdb.getChildBlocks(txpow.getTxPoWID());
								for(TxPoW child : children) {
									processstack.push(child);
								}
							}
						}
						
					}else {
						//Do we have the Parent TxPoW
						TxPoW parent = txpdb.getTxPoW(txpow.getParentID().to0xString());
						if(parent != null) {
							//If Parent not added.. must be missing transactions.. try now ( This block builds on it soo.. )
							processstack.push(parent);
						}
					}
				}
			}
		}
		
		//Did something change..
		if(recalculate) {
			
			//Recalculate the whole tree
			recalculateTree();
		}
	}
	
	private boolean processSyncBlock(TxBlock zTxBlock) throws Exception {
		
		//Are we shutting down..
//		if(Main.getInstance().isShuttingDown()) {
//			return false;
//		}
		
		Cascade cascdb		= MinimaDB.getDB().getDB().getCascade();
		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
		
		//Add the TxPoW to the database - in case we don't have it
		txpdb.addTxPoW(zTxBlock.getTxPoW());
		
		//Do we have ANY TxPoW in the tree at all..
		if(txptree.getTip() == null) {
			
			//Check the cascade ends where this block begins..
			if(cascdb.getTip() != null) {
				
				//The block numbers
				MiniNumber txblknum = zTxBlock.getTxPoW().getBlockNumber();
				MiniNumber cascblk 	= cascdb.getTip().getTxPoW().getBlockNumber();
				
				//Check is 1 less than this block..
				if(!cascblk.isEqual(txblknum.sub(MiniNumber.ONE))) {
					//Error cascade should start where this ends..
					throw new Exception("Invalid SyncBlock Cascade Tip not parent "+txblknum+" casctip:"+cascblk);
				}			
			}
			
			//Create a new node
			TxPoWTreeNode newblock = new TxPoWTreeNode(zTxBlock);
			
			//Set it as root - this recalculates the tree automagically
			txptree.setRoot(newblock);
			
			return true;
		}
		
		//Check not already added
		TxPoWTreeNode oldnode = txptree.findNode(zTxBlock.getTxPoW().getTxPoWID());
		if(oldnode == null) {
		
			//Is there a valid parent block node..
			TxPoWTreeNode parentnode = txptree.findNode(zTxBlock.getTxPoW().getParentID().to0xString());
			if(parentnode != null) {
			
				//Create a new node
				TxPoWTreeNode newblock = new TxPoWTreeNode(zTxBlock);
				
				//Lets add it to the tree
				parentnode.addChildNode(newblock);
				
				//Add fast link in tree - otherwise only reset / added when recalculate tree is called
				txptree.addFastLink(newblock);
				
				return true;
			}else {
				throw new Exception("Invalid SyncBlock as NO PARENT! syncblock:"+zTxBlock.getTxPoW().getBlockNumber()); 
			}
		}
		
		return false;
	}
	
	
	private void recalculateTree() {
		
		//Are we shutting down..
//		if(Main.getInstance().isShuttingDown()) {
//			return;
//		}
		
		//Required DBs
		TxPoWDB txpdb		= MinimaDB.getDB().getTxPoWDB();
		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
		Cascade	cascdb		= MinimaDB.getDB().getCascade();
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		//Need to LOCK DB
		MinimaDB.getDB().writeLock(true);
		
		try {
		
			//What is the current tip..
			TxPoWTreeNode currenttip = txptree.getTip();
			
			//Recalculate the tree and tip
			txptree.recalculateTree();
			
			//How big..
			int maxlen = GlobalParams.MINIMA_CASCADE_START_DEPTH.add(GlobalParams.MINIMA_CASCADE_FREQUENCY).getAsInt();
			if(txptree.getHeaviestBranchLength() >= maxlen) {
				
				//Current Tip
				TxPoWTreeNode tip = txptree.getTip();
				
				//get the new root..
				TxPoWTreeNode newroot = tip.getPastNode(tip.getTxPoW().getBlockNumber().sub(GlobalParams.MINIMA_CASCADE_START_DEPTH).increment()); 
				
				//Now copy all the MMR Coins.. 
				newroot.copyParentRelevantCoins();
				
				//NOW - Shrink it..
				ArrayList<TxPoWTreeNode> cascade = txptree.setLength(GlobalParams.MINIMA_CASCADE_START_DEPTH.getAsInt());
				
				//Add these node to the cascade;
				for(TxPoWTreeNode txpnode : cascade) {
					
					//Get the TxPoW
					TxPoW txpow = txpnode.getTxPoW();
					
					//These TxPoW are now in the cascade - that can NEVER change..
					txpdb.setInCascade(txpow.getTxPoWID());
					for(String txpid : txpow.getTransactions()) {
						txpdb.setInCascade(txpid);
					}
					
					//Store in the ArchiveManager
					arch.saveBlock(txpnode.getTxBlock());
					
					//And add to the cascade
					cascdb.addToTip(txpnode.getTxPoW());
				}
				
				//And finally..
				cascdb.cascadeChain();
			}
		
			//And now set all the onchain txns so not used again in a new TxPoW
			txpdb.clearMainChainTxns();
			
			//Current Tip
			TxPoWTreeNode tip = txptree.getTip();
			while(tip != null) {
				
				//Get the TxPoW
				TxPoW txpow = tip.getTxPoW();
				
				//Set as onchain..
				txpdb.setOnMainChain(txpow.getTxPoWID());
				
				//Set all the txns in the block
				ArrayList<String> txns = txpow.getTransactions();
				for(String txn : txns) {
					txpdb.setOnMainChain(txn);
				}
				
				//And move back up the chain
				tip = tip.getParent();
			}
		
			//Has the Tip changed..
			TxPoWTreeNode newtipnode 	= txptree.getTip();
			
			//Has the tip changed..
			if(currenttip!=null && newtipnode!=null) {
				if(!currenttip.getTxPoW().getTxPoWIDData().isEqual(newtipnode.getTxPoW().getTxPoWIDData())) {
					Main.getInstance().PostMessage(new Message(Main.MAIN_NEWBLOCK).addObject("txpow", newtipnode.getTxPoW()));
				}
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Unlock..
		MinimaDB.getDB().writeLock(false);
		
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Are we shutting down..
		if(Main.getInstance().isShuttingDown()) {
			return;
		}
		
		if(zMessage.isMessageType(TXPOWPROCESSOR_PROCESSTXPOW)) {
			//Get the TxPoW
			TxPoW txp = (TxPoW) zMessage.getObject("txpow");
			
			//Process it..
			processTxPoW(txp);
			
		}else if(zMessage.isMessageType(TXPOWPROCESSOR_PROCESS_IBD)) {
			
			//Get the client - in case is invalid..
			String uid = zMessage.getString("uid");
			
			//Get the IBD
			IBD ibd = (IBD) zMessage.getObject("ibd");
			
			//Does it seem Valid..
			if(!ibd.checkValidData()) {
				return;
			}
			
			//Does it have a cascade
			if(ibd.hasCascade()) {
				
				//Do we.. ?
				if(MinimaDB.getDB().getCascade().getTip() == null) {
					
					//Check is valid for out chain..
					boolean ignore = false;
					TxPoWTreeNode root = MinimaDB.getDB().getTxPoWTree().getRoot();
					if(root != null) {
						
						MiniNumber rootblock = root.getBlockNumber();
						MiniNumber casctip 	 = ibd.getCascade().getTip().getTxPoW().getBlockNumber();
						
						if(!casctip.isEqual(rootblock.decrement())) {
							
							ignore = true;
							MinimaLogger.log("[!] IGNORE IDB - My Tree Root : "+rootblock+" IBD TIP : "+casctip);
						
						}else{
							
							MiniData rootparent = root.getTxPoW().getParentID();
							MiniData casctipid	= ibd.getCascade().getTip().getTxPoW().getTxPoWIDData();
							
							if(!rootparent.isEqual(casctipid)) {
								ignore = true;
								MinimaLogger.log("[!] IGNORE IDB - Invalid Hash for parents");
							}
						}
					}
					
					if(!ignore){
						
						//Process.. Need to LOCK DB
						MinimaDB.getDB().writeLock(true);
						
						//Set this cascade
						try {
							
							//Set this for us
							MinimaDB.getDB().setIBDCascade(ibd.getCascade());
							
						}catch(Exception exc) {
							MinimaLogger.log(exc);
						}
						
						//Unlock..
						MinimaDB.getDB().writeLock(false);
					}
					
				}else {
					//Received a cascade when we already have one.. ignore..
					MinimaLogger.log("WARNING Received cascade when already have one from "+uid);
				}
			}
			
			//Now process the SyncBlocks
			TxPowTree txptree 		= MinimaDB.getDB().getTxPoWTree();
			MiniNumber timenow 		= new MiniNumber(System.currentTimeMillis());
			
			//If our chain is up to date (within 3 hrs) we don't accept TxBlock at all.. only full blocks
			if(txptree.getTip() != null && ibd.getTxBlocks().size()>0) {
				MiniNumber notxblocktimediff = new MiniNumber(1000 * 60 * 180);
				if(GeneralParams.TEST_PARAMS) {
					notxblocktimediff = new MiniNumber(1000 * 60 * 5);
				}
				if(txptree.getTip().getTxPoW().getTimeMilli().sub(timenow).abs().isLess(notxblocktimediff)) {
					MinimaLogger.log("Your chain tip is up to date - no TxBlocks accepted - only FULL TxPoW");
					
					//Ask to sync the TxBlocks
					askToSyncTxBlocks(uid);
					
					return;
				}
			}
			
			//How many blocks have we added
			int additions = 0;
			
			//Cycle and add..
			ArrayList<TxBlock> blocks = ibd.getTxBlocks();
			for(TxBlock block : blocks) {
				
				try {
						
					//Process it..
					processSyncBlock(block);	
					additions++;
				
					//If we've added a lot of blocks..
					if(additions > 1000) {
						
						//recalculate the Tree..
						recalculateTree();
						
						//Reset these
						additions = 0;
					}
					
				}catch(Exception exc) {
					MinimaLogger.log(exc.toString());
					
					//Something funny going on.. disconnect
					Main.getInstance().getNIOManager().disconnect(uid);
					
					break;
				}
			}
			
			//And now recalculate tree
			recalculateTree();
			
			//Ask to sync the TxBlocks
			askToSyncTxBlocks(uid);
		
		}else if(zMessage.isMessageType(TXPOWPROCESSOR_PROCESS_SYNCIBD)) {
			
			//Get the client - in case is invalid..
			String uid = zMessage.getString("uid");
			
			//Get the IBD
			IBD ibd = (IBD) zMessage.getObject("ibd");
			
			//Get the sync blocks
			ArrayList<TxBlock> blocks = ibd.getTxBlocks();
			if(blocks.size() == 0) {
				//No sync blocks
				return;
			}
			
			//Get the ArchiveDB
			ArchiveManager arch = MinimaDB.getDB().getArchive();
			
			//Get the last block I have..
			TxBlock lastblock 	= arch.loadLastBlock();
			TxPoW lastpow 		= null;
			if(lastblock == null) {
				//Use the TxPoWTree
				lastpow = MinimaDB.getDB().getTxPoWTree().getRoot().getTxPoW();
			}else {
				lastpow = lastblock.getTxPoW();
			}
			
			//Cycle through and add..
			for(TxBlock block : blocks) {
				
				//What is the last current block number we know of..
				MiniNumber lastnum = lastpow.getBlockNumber();
				
				//Check the block number is correct.. could be an asynchronous miss-alignment
				if(block.getTxPoW().getBlockNumber().isEqual(lastnum.decrement())) {
				
					//Check the parent hash is correct
					if(block.getTxPoW().getTxPoWIDData().isEqual(lastpow.getParentID())) {
						//We can store it..
						arch.saveBlock(block);
					}else {
						MinimaLogger.log("[-] Invalid block parent in TxBlock sync.. @ "+block.getTxPoW().getBlockNumber()+" from "+uid);
						return;
					}
				}
				
				//we have a new last pow..
				lastpow = block.getTxPoW();
			}
			
			//Ask to sync the TxBlocks
			askToSyncTxBlocks(uid);
		
		}else if(zMessage.isMessageType(TXPOWPROCESSOR_PROCESS_ARCHIVEIBD)) {
			
			//Get the IBD archive data
			IBD arch 			= (IBD) zMessage.getObject("ibd");
			String uid 			= zMessage.getString("uid");
			
			//How many blocks have we added
			int additions = 0;
			
			//Cycle and add..
			ArrayList<TxBlock> blocks = arch.getTxBlocks();
			if(blocks.size() > 0) {
				MinimaLogger.log("Processing Archive IBD length:"+blocks.size()+" start:"+blocks.get(0).getTxPoW().getBlockNumber());	
			}
			
			for(TxBlock block : blocks) {
				
				try {
					
					//Process it..
					processSyncBlock(block);	
					additions++;
				
					//If we've added a lot of blocks..
					if(additions > 1000) {
						
						//recalculate the Tree..
						recalculateTree();
						
						//Reset these
						additions = 0;
					}
					
				}catch(Exception exc) {
					MinimaLogger.log(exc.toString());
					
					//Something funny going on.. disconnect
					Main.getInstance().getNIOManager().disconnect(uid);
					
					break;
				}
			}
			
			//And now recalculate tree
			recalculateTree();
		}
	}
	
	/**
	 * Send a SYNC TxBlock message
	 */
	public void askToSyncTxBlocks(String zClientID) {
		if(!GeneralParams.NO_SYNC_IBD) {
			Message synctxblock = new Message(NIOManager.NIO_SYNCTXBLOCK);
			synctxblock.addString("client", zClientID);
			Main.getInstance().getNetworkManager().getNIOManager().PostMessage(synctxblock);
		}
	}
}
