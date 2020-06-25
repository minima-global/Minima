package org.minima.database.txpowtree;

import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.utils.bretty.TreeNode;
import org.minima.utils.bretty.TreePrinter;

public class BlockTreePrinter {

	public static int NORMAL_NODE_COUNT = 32;
	
	BlockTree mTree;
	
	public BlockTreePrinter(BlockTree zTree) {
		mTree = zTree;
	}
	
	public String printtree() {
		//The root node
		BlockTreeNode root    = mTree.getChainRoot();
		BlockTreeNode cascade = mTree.getCascadeNode();
		BlockTreeNode tip     = mTree.getChainTip();
		
		if(root == null) {
			return "No tree root..";
		}
		
		//First get the whole list
		BlockTreeNode current = tip;
		
		//Now go down 32 blocks..
		int counter = 0;
		BlockTreeNode fulltree = null;
		while(counter<NORMAL_NODE_COUNT && current!=null) {
			//Keep it..
			fulltree = current;
			counter++;
			
			//Is there a valid parent
			current = current.getParent();
		}
		
		//Add the rest of the tree
		ArrayList<BlockTreeNode> rootlist = new ArrayList<>();
		while(current != null) {
			rootlist.add(0,current);
			current = current.getParent();
		}
		
		//The ROOT of the whole tree
		TreeNode roottreenode    = new TreeNode("MINIMA CASCADING TREE");
		TreeNode currenttreenode = roottreenode;
		int[] alltots = new int[GlobalParams.MINIMA_CASCADE_LEVELS];
		
		//Cycle through the super blocks..
		boolean foundcascade = false;
		if(rootlist.size()>0) {
			int clev = -1;
			int tot  = 0;
			for(BlockTreeNode supblk : rootlist) {
				int lev  = supblk.getCurrentLevel();
				int slev = supblk.getSuperBlockLevel();
				
				//Have we reached the cascade node
				if(!foundcascade && supblk.getBlockNumber().isEqual(cascade.getBlockNumber())) {
					//OK - Just normal blocks now..
					foundcascade = true;
				
					if(clev!=-1) {
						tot++;
						alltots[slev]++;
						
						String all = "";
						for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
							if(alltots[i] != 0) {
								all +=alltots[i]+"@"+i+" ";
							}
							alltots[i] = 0;
						}
						
						TreeNode newnode = new TreeNode(tot+" @ LEVEL:"+clev+" "+all);
						currenttreenode.addChild(newnode);
						currenttreenode = newnode;	
					}
					
					//Reset the params
					clev = lev;
					tot  = 1;
					alltots[slev]++;
					
					//Add a base..
					TreeNode newnode = new TreeNode(convertNodeToString(supblk));
					currenttreenode.addChild(newnode);
					currenttreenode = newnode;
				}else {
				
					if(lev != clev) {
						//Start a new node..
						if(clev != -1) {
							String all = "";
							for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
								if(alltots[i] != 0) {
									all +=alltots[i]+"@"+i+" ";
								}
								alltots[i] = 0;
							}
							
							TreeNode newnode = new TreeNode(tot+" @ LEVEL:"+clev+" "+all);
							currenttreenode.addChild(newnode);
							currenttreenode = newnode;
						}
						
						//Reset the params
						clev = lev;
						tot  = 1;
						alltots[slev]++;
						
						//Add a base..
						TreeNode newnode = new TreeNode(convertNodeToString(supblk));
						currenttreenode.addChild(newnode);
						currenttreenode = newnode;
					}else {
						tot++;
						alltots[slev]++;
					}
				}
			}
			
			String all = "";
			for(int i=0;i<GlobalParams.MINIMA_CASCADE_LEVELS;i++) {
				if(alltots[i] != 0) {
					all +=alltots[i]+"@"+i+" ";
				}
				alltots[i] = 0;
			}
			
			//Last node..
			TreeNode newnode = new TreeNode(tot+" @ LEVEL:"+clev+" "+all);
			currenttreenode.addChild(newnode);
			currenttreenode = newnode;
		}
		
		//Now add the rest of the list in full
		TreeNode fulltreenode = new TreeNode(convertNodeToString(fulltree));
		currenttreenode.addChild(fulltreenode);
		
		drillNode(fulltree, fulltreenode);
		
		//Now create the visual tree..
		String output = TreePrinter.toString(roottreenode);
		
		return "\n"+output;		
	}
	
	private String convertNodeToString(BlockTreeNode zNode) {
		int slev 	= zNode.getSuperBlockLevel();
		int clev 	= zNode.getCurrentLevel();
		String weight= "WEIGHT:"+zNode.getWeight()+"/"+zNode.getTotalWeight()+" ";
		
		TxPoW txpow = zNode.getTxPow();
		MiniData parent  = txpow.getSuperParent(clev);
		
		int parent2lev = clev+1;
		if(parent2lev>=GlobalParams.MINIMA_CASCADE_LEVELS) {
			parent2lev = GlobalParams.MINIMA_CASCADE_LEVELS-1;
		}
		MiniData parent2 = txpow.getSuperParent(parent2lev);
			
		int transnum = -1;
		if(txpow.hasBody()) {
			transnum = txpow.getBlockTransactions().size();
		}
		
		String parents = "[blk:"+txpow.getBlockNumber()+"] "
					     +"txpowid:"+zNode.getTxPowID().to0xString()+" "
						 +"[parent:"+clev+"]"+parent.to0xString(16)+" "
						 +"[parent:"+(clev+1)+"]"+parent2.to0xString(16)
						 +"[txns:"+transnum+"]";
								
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
