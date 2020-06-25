package org.minima.system.backup;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.CascadeTree;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class SyncPackage implements Streamable{

	/**
	 * The Current SyncPackage Version..
	 */
	MiniNumber mCascadeNode = MiniNumber.ZERO;
	
	ArrayList<SyncPacket> mNodes = new ArrayList<>();
	
	public SyncPackage() {}
	
	public void setCascadeNode(MiniNumber zNumber) {
		mCascadeNode = zNumber;
	}
	
	public MiniNumber getCascadeNode() {
		return mCascadeNode;
	}
	
	public ArrayList<SyncPacket> getAllNodes(){
		return mNodes;
	}

	public BigInteger calculateWeight() {
		//Create a Tree and add all these blocks.. then calculate the weight..
		BlockTree blktree = new BlockTree();
		
		//Is it empty..
		if(mNodes.size() == 0) {
			return BigInteger.ZERO;
		}
		
		//Drill down 
		for(SyncPacket spack : mNodes) {
			TxPoW txpow = spack.getTxPOW();
			MMRSet mmr  = spack.getMMRSet();
			boolean cascade = spack.isCascade();
			
			BlockTreeNode node = new BlockTreeNode(txpow);
			node.setCascade(cascade);
			node.setState(BlockTreeNode.BLOCKSTATE_VALID);
			
			//Sort the MMR..
			node.setMMRset(mmr);

			//Add it..
			blktree.hardAddNode(node, false);
			
			//Is this the cascade block
			if(txpow.getBlockNumber().isEqual(getCascadeNode())) {
				blktree.hardSetCascadeNode(node);
			}
		}
		
		//Now reset..
		CascadeTree casc = new CascadeTree(blktree);
		casc.cascadedTree();
		
		//Get the cascaded version..
		BlockTree newtree = casc.getCascadeTree();
		
		//Whats the weight..
		BigInteger totweight = newtree.getChainRoot().getTotalWeight();
		
		return totweight;
	}
	
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Write the details..
		MiniNumber len =  new MiniNumber(mNodes.size());
		len.writeDataStream(zOut);
		for(SyncPacket node : mNodes) {
			node.writeDataStream(zOut);
		}
		mCascadeNode.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mNodes = new ArrayList<>();
		MiniNumber nodelen = MiniNumber.ReadFromStream(zIn);
		int len = nodelen.getAsInt();
		for(int i=0;i<len;i++) {
			SyncPacket node = new SyncPacket();
			node.readDataStream(zIn);
			mNodes.add(node);
		}
		
		mCascadeNode = MiniNumber.ReadFromStream(zIn);
	}
	
	@Override
	public String toString() {
		String ret = "";
		for(SyncPacket node : mNodes) {
			ret += node+",";
		}
		return ret;
	}
}
