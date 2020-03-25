package org.minima.system.backup;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniByte;
import org.minima.utils.Streamable;

/**
 * When Syncing up this is all the info you need..
 * @author spartacusrex
 */

public class SyncPacket implements Streamable {

	/**
	 * The original TxPOW that made this block
	 */
	TxPOW mTxPOW;
	
	/**
	 * Is this a cascade Node ? No MMR required then
	 */
	boolean mCascadeNode;
	
	/**
	 * The MMRSet for that VALID block.
	 * (Once a block is valid that can never change, and the MMR is fixed - for that block)
	 */
	MMRSet mMMR;
	
	public SyncPacket() {}
	
	public SyncPacket(BlockTreeNode zNode) {
		this(zNode, false);
	}
	
	public SyncPacket(BlockTreeNode zNode, boolean zClearMMR) {
		mTxPOW       = zNode.getTxPow();
		mCascadeNode = zNode.isCascade();
		if(zClearMMR) {
			mMMR = null;	
		}else {
			mMMR = zNode.getMMRSet();
		}
	}
	
	public MMRSet getMMRSet() {
		return mMMR;
	}
	
	public TxPOW getTxPOW() {
		return mTxPOW;
	}
	
	public boolean isCascade() {
		return mCascadeNode;
	}
	
	@Override
	public String toString() {
		return "MMR:"+(!mCascadeNode)+" "+mTxPOW;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//First write out the TXPOW
		mTxPOW.writeDataStream(zOut);
		
		//Is it a cascade node..
		if(mCascadeNode) {
			//Write this and then the MMR
			MiniByte.TRUE.writeDataStream(zOut);
		}else {
			//Not a cascade
			MiniByte.FALSE.writeDataStream(zOut);
		}
		
		//Do we have an MMR
		if(mMMR == null) {
			MiniByte.FALSE.writeDataStream(zOut);
		}else {
			MiniByte.TRUE.writeDataStream(zOut);
			//Write the MMR
			mMMR.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//First the TXPOW
		mTxPOW = new TxPOW();
		mTxPOW.readDataStream(zIn);
		
		//Is it a cascader..
		MiniByte casc = MiniByte.ReadFromStream(zIn);
		mCascadeNode  = casc.isTrue();
	
		//Is there an MMR
		MiniByte mmr = MiniByte.ReadFromStream(zIn);
		
		mMMR = null;
		if(mmr.isTrue()) {
			//Read in the MMR
			mMMR = new MMRSet();
			mMMR.readDataStream(zIn);
//			mMMR.finalizeSet();
		}
	}
	
}
