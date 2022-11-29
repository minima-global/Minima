package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.database.archive.MySQLConnect;
import org.minima.database.cascade.Cascade;
import org.minima.database.cascade.CascadeNode;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class IBD implements Streamable {

	/**
	 * The back end Cascade - only sent for a new user - can be null
	 */
	Cascade mCascade;
	
	/**
	 * List of Sync blocks
	 */
	ArrayList<TxBlock> mTxBlocks;
	
	public IBD() {
		mCascade	= null;
		mTxBlocks 	= new ArrayList<>();
	}
	
	public void createIBD(Greeting zGreeting) {
		
		//The tree..
		TxPowTree txptree = MinimaDB.getDB().getTxPoWTree();
		
		//Are we a fresh user..
		if(txptree.getTip() == null) {
			//We have nothing.. !
			return;
		}
		
		//Lock the DB - cascade and tree tip / root cannot change while doing this..
		MinimaDB.getDB().readLock(true);
		
		try {
			//The greet top
			MiniNumber greettip 	= zGreeting.getTopBlock();
			MiniNumber greetroot 	= zGreeting.getRootBlock();
			
			//Is this a fresh install user
			boolean fresh = greettip.isEqual(MiniNumber.MINUSONE);
			
			//Complete IBD ?
			if(fresh) {
				createCompleteIBD();
				
			}else {
				MiniNumber myroot 		= txptree.getRoot().getTxBlock().getTxPoW().getBlockNumber();
				TxPoWTreeNode tip 		= txptree.getTip();
				MiniNumber mytip 		= tip.getTxBlock().getTxPoW().getBlockNumber();
				
				if(greettip.isLess(myroot)) {
					//Their chain is behind our cascade.. Will need to send him Archived Sync Blocks! AND the full chain
					
					//Find a block in archive that we have..
					MiniNumber found = MiniNumber.MINUSONE;
					int counter=0;
					for(MiniData current : zGreeting.getChain()) {
						
						//Only check every 20 blocks.. just send duplicates as this much faster
						if(counter % 20 == 0) {
							
							//Look in DB
							found = MinimaDB.getDB().getArchive().exists(current.to0xString());
							if(!found.isEqual(MiniNumber.MINUSONE)) {
								break;
							}
						}
						
						//increment the counter..
						counter++;
					}
					
					//Check the very last one.. just in case we skipped it..
					if(found.isEqual(MiniNumber.MINUSONE)) {
						int size = zGreeting.getChain().size();
						if(size>0) {
							found = MinimaDB.getDB().getArchive().exists(zGreeting.getChain().get(size-1).to0xString());
						}
					}
					
					//Did we find a block..
					if(!found.isEqual(MiniNumber.MINUSONE)) {
						
						//Add the whole tree first
						while(tip != null) {
							mTxBlocks.add(0,tip.getTxBlock());
							tip = tip.getParent();
						}
						
						//And NOW - Load the range..
						ArrayList<TxBlock> blocks = MinimaDB.getDB().getArchive().loadBlockRange(found, myroot);
						for(TxBlock block : blocks) {
							mTxBlocks.add(0,block);
						}
					}else {
						MinimaLogger.log("No Archive blocks found to match New User.. ");
						createCompleteIBD();
					}
					
				}else if(greetroot.isMore(mytip)) {
					//We are behind their cascade! - Nothing to send!
					MinimaLogger.log("We are Too old to sync new user! greetroot"+greetroot+" mytip:"+mytip);
					createCompleteIBD();
					
				}else {
					boolean found 			= false;
					String foundblockID 	= null;
					
					//We overlap somewhere.. 
					ArrayList<String> greetblocks 	= new ArrayList<>();
					for(MiniData block : zGreeting.getChain()) {
						greetblocks.add(block.to0xString());
					}
					
					//Create a string array of our blocks..
					HashSet<String> myblocks = new HashSet<>();
					while(tip != null) {
						myblocks.add(tip.getTxPoW().getTxPoWID());
						tip = tip.getParent();
					}
					
					//Now check for intersection
					for(String block : greetblocks) {
						if(myblocks.contains(block)) {
							//Found one!
							found 			= true;
							foundblockID 	= block;
							break;
						}
					}
					
					//Did we find it.. ?
					if(found) {
						
						MinimaLogger.log("Crossover found @ "+foundblockID);
						
						//Send from then onwards as SyncBlocks..
						tip = MinimaDB.getDB().getTxPoWTree().getTip();
						while(tip != null) {
							String currentid = tip.getTxPoW().getTxPoWID();
							
							if(!currentid.equals(foundblockID)) {
								mTxBlocks.add(0,tip.getTxBlock());
							}else {
								//That's all of them..
								break;
							}
							
							//Move back..
							tip = tip.getParent();
						}
					}else {
						MinimaLogger.log("[!] No Crossover found whilst syncing with new node. They are on a different chain. Please check you are on the correct chain");
						createCompleteIBD();
					}
				}
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Unlock..
		MinimaDB.getDB().readLock(false);
	}
	
	public void createCompleteIBD() throws IOException {
		//First copy the current Cascade
		mCascade = MinimaDB.getDB().getCascade().deepCopy();
	
		//And now add all the blocks.. root will be first
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		while(tip != null) {
			mTxBlocks.add(0,tip.getTxBlock());
			tip = tip.getParent();
		}
	}
	
	public void createSyncIBD(TxPoW zLastBlock) {
		
		//No cascade
		mCascade = null;
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		//Lock the DB - cascade and tree tip / root cannot change while doing this..
		MinimaDB.getDB().readLock(true);
		
		try {
		
			//Are we shutting down..
			if(Main.getInstance().isShuttingDown()) {
				return;
			}
			
			//Load the block range..
			ArrayList<TxBlock> blocks = arch.loadSyncBlockRange(zLastBlock.getBlockNumber());
			for(TxBlock block : blocks) {
				mTxBlocks.add(block);
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Unlock..
		MinimaDB.getDB().readLock(false);
		
	}
	
	public void createArchiveIBD(MiniNumber zFirstBlock) {
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
				
		//Are we storing Archive Data
		if(arch.isStoreMySQL()) {
			
			//Get the SQL Connect
			MySQLConnect mySQLConnect = arch.getMySQLCOnnect();
			
			//Lock the DB - cascade and tree tip / root cannot change while doing this..
			MinimaDB.getDB().readLock(true);
			
			try {
				if(zFirstBlock.isEqual(MiniNumber.ZERO)) {
					//Load cascade if there is one
					mCascade = mySQLConnect.loadCascade();
				}
				
				//Was therea cascade
				MiniNumber startcount = zFirstBlock;
				if(mCascade != null) {
					startcount = mCascade.getTip().getTxPoW().getBlockNumber();
				}
				
				//Load the block range..
				ArrayList<TxBlock> blocks = mySQLConnect.loadBlockRange(zFirstBlock);
				for(TxBlock block : blocks) {
					mTxBlocks.add(block);
				}
				
			}catch(Exception exc) {
				MinimaLogger.log(exc);
			}
			
			//Unlock..
			MinimaDB.getDB().readLock(false);
		}
	}
	
	/**
	 * This will be a copy - not the original
	 */
	public void setCascade(Cascade zCascade) {
		mCascade = zCascade;
	}
	
	public Cascade getCascade() {
		return mCascade;
	}
	
	public boolean hasCascade() {
		return mCascade!=null;
	}
	
	public boolean hasCascadeWithBlocks() {
		return hasCascade() && getCascade().getLength()>0;
	}
	
	public ArrayList<TxBlock> getTxBlocks(){
		return mTxBlocks;
	}
	
	//Check this IBD at least seems right..
	public boolean checkValidData() {
		
		//Do we have a cascade
		if(hasCascadeWithBlocks()) {
			
			//Any Blocks..
			if(getTxBlocks().size()==0) {
				
				//Something wrong..
				MinimaLogger.log("[!] Received INVALID IBD no blocks.. with a cascade");
				
				return false;
			
			}else{
				
				//Check the Tip is one less than the tree..
				MiniNumber casctip 		= getCascade().getTip().getTxPoW().getBlockNumber();
				MiniNumber treestart 	= mTxBlocks.get(0).getTxPoW().getBlockNumber();
				
				if(!treestart.isEqual(casctip.increment())) {
					
					//Something wrong..
					MinimaLogger.log("[!] Received INVALID IBD with cascade tip:"+casctip+" and tree start:"+treestart);
					
					return false;
				}
			}
		}
		
		return true;
	}
	
	public BigInteger getTotalWeight(){
		
		//The total weight of the chain
		BigDecimal total  = BigDecimal.ZERO;
		for(TxBlock block : mTxBlocks) {
			total = total.add(block.getTxPoW().getWeight());
		}
		BigInteger chainweight 	= total.toBigInteger();
		
		//The weight of the cascade
		BigInteger cascweight = BigInteger.ZERO;
		if(getCascade() != null) {
			cascweight 	= getCascade().getTotalWeight().toBigInteger();
		}
			
		return cascweight.add(chainweight);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Cascade can be null
		if(hasCascade()) {
			MiniByte.TRUE.writeDataStream(zOut);
			mCascade.writeDataStream(zOut);
		}else {
			MiniByte.FALSE.writeDataStream(zOut);
		}
		
		//And now write all the sync blocks.. if any
		int len = mTxBlocks.size();
		MiniNumber.WriteToStream(zOut, len);
		for(TxBlock block : mTxBlocks) {
			block.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//Is there a cascade
		if(MiniByte.ReadFromStream(zIn).isTrue()) {
			//Read in the cascade
			mCascade = new Cascade();
			mCascade.readDataStream(zIn);
		}else {
			mCascade = null;
		}
		
		//And now all the sync blocks
		mTxBlocks = new ArrayList<>();
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mTxBlocks.add(TxBlock.ReadFromStream(zIn));
		}
	}
	
	public static IBD ReadFromStream(DataInputStream zIn) throws IOException {
		IBD ibd = new IBD();
		ibd.readDataStream(zIn);
		return ibd;
	}
	
	
	/**
	 * Check the weight of this IBD with our Own!..
	 */
	public static boolean checkOurChainHeavier(IBD zIBD) throws IOException {
		
		//First get OUR own complete IBD..
		IBD current = new IBD();
		current.createCompleteIBD();
		
		//Now compare this to the provided IBD.. find the cross over..
		boolean found 			= false;
		String foundblockID 	= null;
		
		//We overlap somewhere.. 
		ArrayList<String> greetblocks 	= new ArrayList<>();
		for(TxBlock block : zIBD.getTxBlocks()) {
			greetblocks.add(0, block.getTxPoW().getTxPoWID());
		}
		
		ArrayList<String> greetcascblocks 	= new ArrayList<>();
		CascadeNode tip = zIBD.getCascade().getTip();
		while(tip != null) {
			greetcascblocks.add(tip.getTxPoW().getTxPoWID());
			tip = tip.getParent();
		}
		
		//Create a set of OUR blocks..
		HashSet<String> myblocks = new HashSet<>();
		for(TxBlock block : current.getTxBlocks()) {
			myblocks.add(block.getTxPoW().getTxPoWID());
		}
		
		//Create a set of OUR Cascade blocks..
		HashSet<String> mycascblocks = new HashSet<>();
		tip = current.getCascade().getTip();
		while(tip != null) {
			mycascblocks.add(tip.getTxPoW().getTxPoWID());
			tip = tip.getParent();
		}
		
		//Now check for intersection
		for(String block : greetblocks) {
			if(myblocks.contains(block)) {
				//Found one!
				found 			= true;
				foundblockID 	= block;
				break;
			}
		}
		
		//Now check for intersection
		if(!found) {
			for(String block : greetblocks) {
				if(mycascblocks.contains(block)) {
					//Found one!
					found 			= true;
					foundblockID 	= block;
					break;
				}
			}
		}
		
		//Now check for intersection
		if(!found) {
			for(String block : greetcascblocks) {
				if(myblocks.contains(block)) {
					//Found one!
					found 			= true;
					foundblockID 	= block;
					break;
				}
			}
		}
		
		//Now check for intersection
		if(!found) {
			for(String block : greetcascblocks) {
				if(mycascblocks.contains(block)) {
					//Found one!
					found 			= true;
					foundblockID 	= block;
					break;
				}
			}
		}
		
		//Now create 2 new IBD.. up to and including the intersection block
		BigInteger myweight;
		BigInteger theirweight;
		if(found) {
			MinimaLogger.log("Intersection of chains found @ "+foundblockID);
			
			IBD mynew 		= createShortenedIBD(current, foundblockID);
			myweight 		= mynew.getTotalWeight();
			
			IBD theirnew 	= createShortenedIBD(zIBD, foundblockID);
			theirweight		= theirnew.getTotalWeight();
			
		}else {
			//Current total weights..
			myweight 	= current.getTotalWeight();
			theirweight = zIBD.getTotalWeight();
		}
		
		boolean heavier = myweight.compareTo(theirweight) >= 0;
		
		if(!heavier) {
			MinimaLogger.log("YOUR  WEIGHT : "+myweight);
			MinimaLogger.log("THEIR WEIGHT : "+theirweight);
		}
		
		//Now see if we are Heavier..
		return heavier;
	}
	
	public static IBD createShortenedIBD(IBD zIBD, String zTxPOWID) {
		
		IBD ibd = new IBD();
		ibd.setCascade(new Cascade());
		
		//Cycle through the blocks and add..
		boolean found = false;
		ArrayList<TxBlock> blocks = zIBD.getTxBlocks();
		
		ArrayList<TxBlock> revblocks = new ArrayList<>();
		for(TxBlock block : blocks) {
			revblocks.add(0, block);
		}
		
		for(TxBlock block : revblocks) {
			
			//Add this block to the new IBD
			ibd.getTxBlocks().add(0,block);
			
			//Have we found the intersection 
			if(block.getTxPoW().getTxPoWID().equals(zTxPOWID)) {
				found = true;
				break;
			}
		}
		
		if(!found) {
			
			//Add the cascade blocks..
			ArrayList<TxPoW> txps = new ArrayList<>();
			CascadeNode tip = zIBD.getCascade().getTip();
			while(tip != null) {
				txps.add(0,tip.getTxPoW());
				
				if(tip.getTxPoW().getTxPoWID().equals(zTxPOWID)) {
					break;
				}
				
				tip = tip.getParent();
			}
			
			//Now add those to the IBD
			for(TxPoW txp : txps) {
				ibd.getCascade().addToTip(txp);	
			}
			
			ibd.getCascade().cascadeChain();
		}
		
		return ibd;
	}
	
	public static void printIBD(IBD zIBD) {
		System.out.println("Cascade:");
		System.out.println(zIBD.getCascade().printCascade());
		
		//And the blocks..
		System.out.println("Blocks:");
		ArrayList<TxBlock> blox = zIBD.getTxBlocks();
		for(TxBlock block : blox) {
			TxPoW txp = block.getTxPoW();
			System.out.println(txp.getBlockNumber()+") "+txp.getTxPoWID());
		}
		
		//Total Weight..
		System.out.println("Total Weight : "+zIBD.getTotalWeight());
		System.out.println();
	}
}
