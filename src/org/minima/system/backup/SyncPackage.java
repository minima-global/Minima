package org.minima.system.backup;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.MultiLevelCascadeTree;
import org.minima.database.txpowtree.SimpleBlockTreePrinter;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class SyncPackage implements Streamable{

	MiniNumber mCascadeNode;
	
	ArrayList<SyncPacket> mNodes;
	
	public SyncPackage() {
		mNodes = new ArrayList<>();
		mCascadeNode = MiniNumber.ZERO;
	}
	
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
		
		//Drill down 
		for(SyncPacket spack : mNodes) {
			TxPOW txpow = spack.getTxPOW();
			MMRSet mmr  = spack.getMMRSet();
			boolean cascade = spack.isCascade();
			
			BlockTreeNode node = new BlockTreeNode(txpow);
			node.setCascade(cascade);
			node.setState(BlockTreeNode.BLOCKSTATE_VALID);
			
			//Sort the MMR..
			node.setMMRset(mmr);

			//Add it..
			blktree.hardAddNode(node, true);
			
			//Is this the cascade block
			if(txpow.getBlockNumber().isEqual(getCascadeNode())) {
				blktree.hardSetCascadeNode(node);
			}
		}
		
		//Now reset..
		MultiLevelCascadeTree casc = new MultiLevelCascadeTree(blktree);
		casc.cascadedTree();
		
		//Get the cascaded version..
		BlockTree newtree = casc.getCascadeTree();
		
//		SimpleBlockTreePrinter print = new SimpleBlockTreePrinter(newtree);
//		String tp = print.printtree();
//		System.out.println(tp);
		
		//Whats the weight..
		BigInteger totweight = newtree.getChainRoot().getTotalWeight();
		
		return totweight;
	}
	
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		int len = mNodes.size();
		zOut.writeInt(len);
		for(SyncPacket node : mNodes) {
			node.writeDataStream(zOut);
		}
		mCascadeNode.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		int len = zIn.readInt();
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
