package org.minima.database.txpowtree;

import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Maths;
import org.minima.utils.MinimaLogger;
import org.minima.utils.bretty.TreeNode;
import org.minima.utils.bretty.TreePrinter;

public class BlockTreePrinter2 {

	BlockTree mTree;
	
	long mCascadeNode=0;
	
	MiniHash mTipID;
	
	boolean mSimple = true;
	
	public BlockTreePrinter2(BlockTree zTree, boolean zSimple) {
		mTree = zTree;
		mSimple = zSimple;
	}
	
	public String printtree() {
//		MinimaLogger.log("---------");
//		MinimaLogger.log("Full Tree");
//		MinimaLogger.log("---------");
		
		//The root node
		BlockTreeNode root = mTree.getChainRoot();
		
		if(root == null) {
//			MinimaLogger.log("No tree root..");
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
		String output = TreePrinter.toString(mRoot);

		BigInteger avgdiff = mTree.getAvgChainDifficulty();
		MiniHash avghash = new MiniHash("0x"+avgdiff.toString(16));
		
		output += "\n\nSpeed              : "+mTree.getChainSpeed()+" blocks / sec";
		output += "\nAVG Difficulty     : "+avgdiff;
		output += "\nAVG HASH           : "+avghash;
		output += "\nCurrent Difficulty : "+mTree.getChainTip().getTxPow().getBlockDifficulty().to0xString();
		
//		MinimaLogger.log("Speed     : "+mTree.getChainSpeed()+" blocks / sec");

		//And out it goes..
//		MinimaLogger.log(output);
	
//		//And some added details..
//		MinimaLogger.log("Total POW : "+mTree.getChainRoot().getTotalWeight());
//		MinimaLogger.log("Root      : "+mTree.getChainRoot());
//		MinimaLogger.log("Tip       : "+mTree.getChainTip());
//		MinimaLogger.log("Length    : "+mTree.getAsList().size());
//		MinimaLogger.log("Cascade   : "+mCascadeNode);
//		MinimaLogger.log("Speed     : "+mTree.getChainSpeed()+" blocks / sec");
//		
//		MiniNumber diff 	= mTree.getAvgChainDifficulty();
//		BigInteger diffbi 	= diff.getAsBigInteger();
//		
//		double log = Maths.log2BI(diffbi);
//		MinimaLogger.log("AVG Diff  : "+diff);
//		MinimaLogger.log("LOG Diff  : "+log);
		
		return output;
	}
	
	private String convertNodeToString(BlockTreeNode zNode) {
		int slev 	= zNode.getSuperBlockLevel();
		int clev 	= zNode.getCurrentLevel();
		
		String weight= "{WEIGHT:"+zNode.getWeight()+"/"+zNode.getTotalWeight()+"} ";
		
		String ss = zNode.toString();
		
		String add = zNode.getTxPowID().toShort0xString()+" "
					+zNode.getTxPow().getBlockDifficulty().toShort0xString()+" "
					+getStarString(slev);
		
		if(mCascadeNode == zNode.getTxPow().getBlockNumber().getAsLong()) {
			add += " [++CASCADING++]";
		}
		
		if(zNode.getTxPowID().isExactlyEqual(mTipID)) {
			add += " [++THE TIP++]";
		}

		if(mSimple) {
			return weight + zNode.getTxPow().getBlockNumber()+" "+"["+clev+" / "+slev+"] "+add;
		}
		
		return weight + ss +" "+getStarString(slev)+" "+add;
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
