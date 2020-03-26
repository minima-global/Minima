package org.minima.database.txpowtree;

import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Maths;
import org.minima.utils.MinimaLogger;
import org.minima.utils.bretty.TreeNode;
import org.minima.utils.bretty.TreePrinter;

public class SimpleBlockTreePrinter {

	BlockTree mTree;
	
	long mCascadeNode=0;
	
	MiniHash mTipID;
	
	boolean mSimple = true;
	
	public SimpleBlockTreePrinter(BlockTree zTree, boolean zSimple) {
		mTree = zTree;
		mSimple = zSimple;
	}
	
	public String printtree() {
		//The root node
		BlockTreeNode root = mTree.getChainRoot();
		
		if(root == null) {
			return "No tree root..";
		}
		
		//Which node is the cascader
		mCascadeNode = mTree.getCascadeNode().getTxPow().getBlockNumber().getAsLong();
		
		//Which block is the tip..
		mTipID = mTree.getChainTip().getTxPowID();
		
		//Now construct a tree..
		TreeNode mRoot = new TreeNode(convertNodeToString(root));
		
		//Drill it..
		drillNode(root, mRoot);
		
		//And finally print it..
		String output = "\n"+TreePrinter.toString(mRoot);

//		BigInteger avgdiff = mTree.getAvgChainDifficulty();
//		MiniHash avghash = new MiniHash("0x"+avgdiff.toString(16));
//		
//		output += "\n\nSpeed              : "+mTree.getChainSpeed()+" blocks / sec";
//		output += "\nCurrent Difficulty : "+mTree.getChainTip().getTxPow().getBlockDifficulty().to0xString();
//		output += "\nTotal Weight       : "+mTree.getChainRoot().getTotalWeight();

		return output;
	}
	
	private String convertNodeToString(BlockTreeNode zNode) {
		int slev 	= zNode.getSuperBlockLevel();
		int clev 	= zNode.getCurrentLevel();
		String weight= "{WEIGHT:"+zNode.getWeight()+"/"+zNode.getTotalWeight()+"} ";
		
		TxPOW txpow = zNode.getTxPow();
		MiniHash parent  = txpow.getSuperParent(clev);
		MiniHash parent2 = txpow.getSuperParent(clev+1);
				
		String parents = "[blk]"+zNode.getTxPowID().toShort0xString(16)+" "
						 +"[parent:"+clev+"]"+parent.toShort0xString(16)+" "
						 +"[parent:"+(clev+1)+"]"+parent2.toShort0xString(16);
								
		String add = parents +" ["+getStarString(slev)+"] - "+getStarString(clev);
		
		if(mCascadeNode == zNode.getTxPow().getBlockNumber().getAsLong()) {
			add += " [++CASCADING++]";
		}
		
		if(zNode.getTxPowID().isExactlyEqual(mTipID)) {
			add += " [++THE TIP++]";
		}

		return weight + zNode.getTxPow().getBlockNumber()+" "+"["+clev+" / "+slev+"] "+add;
	}
	
	private String getStarString(int zLen) {
		String ret = "";
		for(int i=0;i<zLen;i++) {
			ret += "*";
		}
		return ret;
	}
	
	private void drillNode(BlockTreeNode zNode, TreeNode zTreeNode) {
		//And all the children..
		ArrayList<BlockTreeNode> children = zNode.getChildren();
		
		//now do it..
		for(BlockTreeNode child : children) {
			//Create a tree node..
			TreeNode chilnode = new TreeNode(convertNodeToString(child));
			
			//Add to the Root node
			zTreeNode.addChild(chilnode);

			//And drill the child
			drillNode(child, chilnode);
		}
	}
	
	
	public static void clearScreen() {  
	    System.out.print("\033[H\033[2J");  
	    System.out.flush();  
	}

}
