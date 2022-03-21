package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
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
		mTxBlocks = new ArrayList<>();
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
				//First copy the current Cascade
				mCascade = MinimaDB.getDB().getCascade().deepCopy();
			
				//And now add all the blocks.. root will be first
				TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				while(tip != null) {
					mTxBlocks.add(0,tip.getTxBlock());
					tip = tip.getParent();
				}
				
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
						
						//And the whole tree first
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
					}
					
				}else if(greetroot.isMore(mytip)) {
					//We are behind their cascade! - Nothing to send!
//					MinimaLogger.log("We are Too old to sync new user! greetroot"+greetroot+" mytip:"+mytip);
					
				}else {
					boolean found 			= false;
					String foundblockID 	= null;
					
					//We overlap somewhere.. 
					ArrayList<String> greetblocks 	= new ArrayList<>();
					for(MiniData block : zGreeting.getChain()) {
						greetblocks.add(block.to0xString());
					}
					
					//Create a string array of our blocks..
					ArrayList<String> myblocks = new ArrayList<>();
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
					}
				}
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Unlock..
		MinimaDB.getDB().readLock(false);
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
	
	public ArrayList<TxBlock> getTxBlocks(){
		return mTxBlocks;
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
}
