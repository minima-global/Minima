package org.minima.database.txpowtree;

import java.math.BigInteger;
import java.util.ArrayList;

import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Maths;
import org.minima.utils.MinimaLogger;
import org.minima.utils.bretty.TreeNode;
import org.minima.utils.bretty.TreePrinter;

public class BlockTreePrinter {

	BlockTree mTree;
	
	long mCascadeNode=0;
	
	MiniHash mTipID;
	
	boolean mSimple = false;
	
	public BlockTreePrinter(BlockTree zTree, boolean zSimple) {
		mTree = zTree;
		mSimple = zSimple;
	}
	
	public void printtree() {
		MinimaLogger.log("---------");
		MinimaLogger.log("Full Tree");
		MinimaLogger.log("---------");
		
		//The root node
		BlockTreeNode root = mTree.getChainRoot();
		
		if(root == null) {
			MinimaLogger.log("No tree root..");
			return;
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

		//And out it goes..
		MinimaLogger.log(output);
	
		//And some added details..
		MinimaLogger.log("Total POW : "+mTree.getChainRoot().getTotalWeight());
		MinimaLogger.log("Root      : "+mTree.getChainRoot());
		MinimaLogger.log("Tip       : "+mTree.getChainTip());
		MinimaLogger.log("Length    : "+mTree.getAsList().size());
		MinimaLogger.log("Cascade   : "+mCascadeNode);
		MinimaLogger.log("Speed     : "+mTree.getChainSpeed()+" blocks / sec");
		
		MiniNumber diff 	= MiniNumber.ZERO;//mTree.getAvgChainDifficulty();
		BigInteger diffbi 	= diff.getAsBigInteger();
		
		double log = Maths.log2BI(diffbi);
		MinimaLogger.log("AVG Diff  : "+diff);
		MinimaLogger.log("LOG Diff  : "+log);
	}
	
	private String convertNodeToString(BlockTreeNode zNode) {
		int slev 	= zNode.getSuperBlockLevel();
		
		String weight= "{WEIGHT:"+zNode.getWeight()+"/"+zNode.getTotalWeight()+"} ";
		
		String ss = zNode.toString();
		
		String add = "";
		if(mCascadeNode == zNode.getTxPow().getBlockNumber().getAsLong()) {
			add += " [++CASCADING++]";
		}
		
		if(zNode.getTxPowID().isNumericallyEqual(mTipID)) {
			add += " [++THE TIP++]";
		}

		if(mSimple) {
//			return weight + zNode.getTxPow().getBlockNumber() +" "+getStarString(slev)+" "+add;
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
