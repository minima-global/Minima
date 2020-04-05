package org.minima.database.txpowtree;

import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.bretty.TreeNode;
import org.minima.utils.bretty.TreePrinter;

public class SimpleBlockTreePrinter {

	BlockTree mTree;
	
	long mCascadeNode=0;
	
	MiniData mTipID;
	
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
		mTipID               = mTree.getChainTip().getTxPowID();
		MiniNumber tip       = mTree.getChainTip().getTxPow().getBlockNumber();
		MiniNumber starttree = tip.sub(MiniNumber.THIRTYTWO);
		if(starttree.isLess(MiniNumber.ZERO)) {
			starttree = MiniNumber.ZERO;
		}
		
		int[] alltots = new int[GlobalParams.MINIMA_CASCADE_LEVELS];
		
		//Now construct a tree..
		TreeNode rootnode = new TreeNode(convertNodeToString(root));
		TreeNode treenode = rootnode;
		TreeNode newnode = null;
		
		BlockTreeNode current = root;
		int currentlev = current.getCurrentLevel();
		int tot     = 1;
		alltots[current.getSuperBlockLevel()]++;
		
		while(current.getTxPow().getBlockNumber().isLess(starttree)) {
			//Get the child..
			if(current.getChildren().size()<1) {
				//Add the last
				break;
			}
			
			//Get the child..
			BlockTreeNode child = current.getChild(0);
			
			//Child level
			int clev = child.getCurrentLevel();
			
			if(clev == currentlev) {
				tot++;
				alltots[child.getSuperBlockLevel()]++;
				
			}else {
				//Make the string..
				String all = "";
				for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
					if(alltots[i] != 0) {
						all +=alltots[i]+"@"+i+" ";
					}
					alltots[i] = 0;
				}
				
				//Add the last
				newnode = new TreeNode(tot+" @ "+currentlev+" Super:"+all);
				treenode.addChild(newnode);
				treenode = newnode;
				
				//And add the first of the next level..
				newnode = new TreeNode(convertNodeToString(child));
				treenode.addChild(newnode);
				treenode = newnode;
				
				alltots[child.getSuperBlockLevel()]++;
				tot = 1;
			}
			
			currentlev = clev;
			current    = child;
		}
		
		//Make the string..
		String all = "";
		for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
			if(alltots[i] != 0) {
				all +=alltots[i]+"@"+i+" ";
			}
			alltots[i] = 0;
		}
		
		//Add the last
		newnode = new TreeNode(tot+" @ "+currentlev+" Super:"+all);
		treenode.addChild(newnode);
		treenode = newnode;
		
		//Drill the rest..
		drillNode(current , treenode, 1);
		
		String output = "\n"+TreePrinter.toString(rootnode);
		
//		//Now construct a tree..
//		TreeNode mRoot = new TreeNode(convertNodeToString(root));
//				
//		//Drill it..
//		drillNode(root , mRoot, 1);
//		
//		//And finally print it..
//		String output = "\n"+TreePrinter.toString(mRoot);

		return output;
	}
	
	private String convertNodeToString(BlockTreeNode zNode) {
		int slev 	= zNode.getSuperBlockLevel();
		int clev 	= zNode.getCurrentLevel();
		String weight= "{WEIGHT:"+zNode.getWeight()+"/"+zNode.getTotalWeight()+"} ";
		
		TxPOW txpow = zNode.getTxPow();
		MiniData parent  = txpow.getSuperParent(clev);
		MiniData parent2 = txpow.getSuperParent(clev+1);
				
		String parents = "[blk:"+txpow.getBlockNumber()+"] "
//						 +"diff:"+txpow.getBlockDifficulty().toShort0xString(16)+" "
					     +"txpowid:"+zNode.getTxPowID().to0xString(16)+" "
						 +"[parent:"+clev+"]"+parent.to0xString(16)+" "
						 +"[parent:"+(clev+1)+"]"+parent2.to0xString(16)
						 +"[txns:"+txpow.getBlockTxns().size()+"]";
								
		String add = parents +" ["+getStarString(slev)+"] - "+getStarString(clev);
		
//		if(mCascadeNode == zNode.getTxPow().getBlockNumber().getAsLong()) {
//			add += " [++CASCADING++]";
//		}
//		
//		if(zNode.getTxPowID().isEqual(mTipID)) {
//			add += " [++THE TIP++]";
//		}

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
