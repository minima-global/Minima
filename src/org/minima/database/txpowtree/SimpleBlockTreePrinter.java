package org.minima.database.txpowtree;

import java.util.ArrayList;

import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniHash;
import org.minima.utils.bretty.TreeNode;
import org.minima.utils.bretty.TreePrinter;

public class SimpleBlockTreePrinter {

	BlockTree mTree;
	
	long mCascadeNode=0;
	
	MiniHash mTipID;
	
	public SimpleBlockTreePrinter(BlockTree zTree) {
		mTree = zTree;
	}
	
	public String printtree() {
		//The root node
		BlockTreeNode root = mTree.getChainRoot();
		
		if(root == null) {
			return "No tree root..";
		}
		
		//Which node is the cascade
		mCascadeNode = mTree.getCascadeNode().getTxPow().getBlockNumber().getAsLong();
		
		//Which block is the tip..
		mTipID = mTree.getChainTip().getTxPowID();
		
		//Now construct a tree..
		TreeNode mRoot = new TreeNode(convertNodeToString(root));
		
		//Drill it..
		drillNode(root, mRoot, 1);
		
		//And finally print it..
		String output = "\n"+TreePrinter.toString(mRoot);

		return output;
	}
	
	private String convertNodeToString(BlockTreeNode zNode) {
		int slev 	= zNode.getSuperBlockLevel();
		int clev 	= zNode.getCurrentLevel();
		String weight= "{WEIGHT:"+zNode.getWeight()+"/"+zNode.getTotalWeight()+"} ";
		
		TxPOW txpow = zNode.getTxPow();
		MiniHash parent  = txpow.getSuperParent(clev);
		MiniHash parent2 = txpow.getSuperParent(clev+1);
				
		String parents = "[blk:"+zNode.getTxPow().getBlockNumber()+"] "
						 +"diff:"+zNode.getTxPow().getBlockDifficulty().toShort0xString(16)+" "
					     +"txpowid:"+zNode.getTxPowID().toShort0xString(16)+" "
						 +"[parent:"+clev+"]"+parent.toShort0xString(16)+" "
						 +"[parent:"+(clev+1)+"]"+parent2.toShort0xString(16);
								
		String add = parents +" ["+getStarString(slev)+"] - "+getStarString(clev);
		
		if(mCascadeNode == zNode.getTxPow().getBlockNumber().getAsLong()) {
			add += " [++CASCADING++]";
		}
		
		if(zNode.getTxPowID().isExactlyEqual(mTipID)) {
			add += " [++THE TIP++]";
		}

		return weight+"["+clev+" / "+slev+"] "+add;
	}
	
	private String getStarString(int zLen) {
		String ret = "";
		for(int i=0;i<zLen;i++) {
			ret += "*";
		}
		return ret;
	}
	
	private void drillNode(BlockTreeNode zNode, TreeNode zTreeNode, int zLevel) {
		//And all the children..
		ArrayList<BlockTreeNode> children = zNode.getChildren();
		
		//now do it..
		for(BlockTreeNode child : children) {
			//Create a tree node..
			TreeNode chilnode = new TreeNode(convertNodeToString(child));
			
			//Add to the Root node
			zTreeNode.addChild(chilnode);

			//And drill the child
			drillNode(child, chilnode, child.getCurrentLevel());
		}
	}
	
	
//	private void drillNode(BlockTreeNode zNode, TreeNode zTreeNode, int zLevelNum) {
//		//The Current level
//		int clev = zNode.getCurrentLevel();
//		
//		//And all the children..
//		ArrayList<BlockTreeNode> children = zNode.getChildren();
//		
//		//now do it..
//		for(BlockTreeNode child : children) {
//			//What level is the Child..
//			int childclev = child.getCurrentLevel();
//			
//			if(childclev == 0) {
//				//Create a tree node..
//				TreeNode chilnode = new TreeNode(convertNodeToString(child));
//				
//				//Add to the Root node
//				zTreeNode.addChild(chilnode);
//			
//				//And drill the child
//				drillNode(child, chilnode,0);
//			
//			}else if(childclev == clev) {
//				//add it to the Current
//				int totlevel = zLevelNum+1;
//				
//				//Create a tree node..
//				TreeNode chilnode = new TreeNode(convertNodeToString(child));
//				
//				//Add to the Root node
//				zTreeNode.addChild(chilnode);
//				
////				if(totlevel < 2) {
////					//And drill the child
////					drillNode(child, chilnode, totlevel);
////				}else {
//					//And drill the child
//					drillNode(child, zTreeNode,totlevel);
////				}
//			
//			}else{
//				//Add it start again..
//				TreeNode chilnode = new TreeNode(convertNodeToString(child));
//				
//				//Add to the Root node
//				zTreeNode.addChild(chilnode);
//			
//				//And drill the child
//				drillNode(child, chilnode,0);
//				
//			}
//		}
//	}
	
	
	public static void clearScreen() {  
	    System.out.print("\033[H\033[2J");  
	    System.out.flush();  
	}

}
