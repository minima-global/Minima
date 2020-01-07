package org.minima.database.txpowtree;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData32;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class BlockTreeStore implements Streamable{

	BlockTree mTree;
	
	public BlockTreeStore(BlockTree zTree) {
		mTree = zTree;
	}

	public BlockTree getTree() {
		return mTree;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Write the whole block tree in compressed format..
		BlockTreeNode root    = mTree.getChainRoot();
		
		//Do it!
		recursivewrite(root,zOut);
	}
	
	private void recursivewrite(BlockTreeNode zNode, DataOutputStream zOut) throws IOException {
		//Is this a cascade node..
		if(zNode.isCascade()) {
			MiniByte.TRUE.writeDataStream(zOut);
		}else {
			MiniByte.FALSE.writeDataStream(zOut);
		}
		
		//Now the TXPOW ID
		zNode.getTxPowID().writeDataStream(zOut);
		
		//And finally the MMR!
		//..
		
		//Now all the children..
		ArrayList<BlockTreeNode> children = zNode.getChildren();
		int len = children.size();
		zOut.writeInt(len);
		for(BlockTreeNode child : children) {
			recursivewrite(child, zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//read in the whole tree
		mTree = new BlockTree();
		
		//Load it all..
		readRecursicve(null,zIn);
	}
	
	private void readRecursicve(BlockTreeNode zParent, DataInputStream zIn) throws IOException {
//		//Create a blank node..
//		BlockTreeNode tnode = new BlockTreeNode();
//		
//		//Start.. from root
//		MiniByte casc = MiniByte.ReadFromStream(zIn);
//		tnode.setCascade(casc.isTrue());
//		
//		//Get the id..
//		MiniData32 txid = MiniData32.ReadFromStream(zIn);
//		tnode.setTxPowIDReadIn(txid);
//	
//		//And the MMR!
//		//..
//		
//		if(zParent == null) {
//			//It's root!
//			mTree.setTreeRoot(tnode);
//		}else {
//			zParent.addChild(tnode);
//		}
//		
//		//Load the children
//		int childlen = zIn.readInt();
//		for(int i=0;i<childlen;i++) {
//			readRecursicve(tnode, zIn);	
//		}
	}
	
}
