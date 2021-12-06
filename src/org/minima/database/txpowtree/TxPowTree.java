package org.minima.database.txpowtree;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Date;
import java.util.Hashtable;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Stack;
import org.minima.utils.Streamable;

public class TxPowTree implements Streamable {

	/**
	 * Root node of the whole tree
	 */
	TxPoWTreeNode mRoot;
	
	/**
	 * Tip of the heaviest branch on the tree based on GHOST
	 */
	TxPoWTreeNode mTip;
	
	/**
	 * The current length of the heaviest Branch
	 */
	int mLength;
	
	/**
	 * How many nodes in total
	 */
	int mTotalNodes;
	
	/**
	 * A hashtable o(1) look up table to find nodes in the tree
	 */
	Hashtable<String, TxPoWTreeNode> mFastLink;
	
	/**
	 * The PULSE list.. a ready to use list of TxPoWID from tip..
	 * Updated whenever we recalculste the tree
	 */
	ArrayList<MiniData> mPulseList = new ArrayList<>();
	
	/**
	 * Main Constructor
	 */
	public TxPowTree() {
		mRoot 		= null;
		mTip 		= null;
		mFastLink 	= new Hashtable<>();
		mLength		= 0;
	}
	
	public TxPoWTreeNode findNode(String zTxPoWID) {
		//Use the fast link table..
		return mFastLink.get(zTxPoWID);
		
//		//Create a find action
//		NodeAction finder = new NodeAction(zTxPoWID) {
//			@Override
//			public void runAction(TxPoWTreeNode zNode) {
//				if(zNode.getTxPoW().getTxPoWID().equals(getExtraData())) {
//					setReturnObject(zNode);
//				}
//			}
//		}; 
//
//		//Now run that over the whole tree..
//		return NodeTraverse.traverseTree(this, finder);
	}

	public void setRoot(TxPoWTreeNode zTreeNode) {
		mRoot = zTreeNode;
		mRoot.clearParent();
		
		recalculateTree();
	}

	public TxPoWTreeNode getRoot() {
		return mRoot;
	}

	public TxPoWTreeNode getTip() {
		return mTip;
	}
	
	public int getSize() {
		return mFastLink.size();
	}

	public void addFastLink(TxPoWTreeNode zNode) {
		mFastLink.put(zNode.getTxPoW().getTxPoWID(), zNode);
	}
	
	public void recalculateTree() {
		//New Fast Link..
		mFastLink = new Hashtable<>();
		
		//Create a complete list of all nodes in the tree
		ArrayList<TxPoWTreeNode> allnodes = new ArrayList<>();
		
		//First set all the weights to their base weight
		TxPoWTreeNodeAction reset = new TxPoWTreeNodeAction() {
			@Override
			public void runAction(TxPoWTreeNode zNode) {
				//Get the TxPoW weight
				BigDecimal weight = zNode.getTxPoW().getWeight();
				
				//This is the base weight
				zNode.setTotalWeight(weight);
				
				//Add to our list
				allnodes.add(zNode);
				
				//And add to our fast link table..
				mFastLink.put(zNode.getTxPoW().getTxPoWID(), zNode);
			}
		}; 
		
		//Traverse the tree
		traverseTree(reset);
	
		//Reset total nodes
		mTotalNodes = mFastLink.size();
		
		//Now organise all nodes into descending order
		allnodes.sort(new Comparator<TxPoWTreeNode>() {
			@Override
			public int compare(TxPoWTreeNode o1, TxPoWTreeNode o2) {
				return o2.getTxPoW().getBlockNumber().compareTo(o1.getTxPoW().getBlockNumber());
			}
		});
		
		//Now cycle through and add the weight of the children to the parents
		for(TxPoWTreeNode node : allnodes) {
			ArrayList<TxPoWTreeNode> children = node.getChildren();
			for(TxPoWTreeNode child : children) {
				node.addToTotalWeight(child.getTotalWeight());
			}
		}
		
		//And find the heaviest branch tip..
		mTip 	= getRoot();
		mLength = 0;
		
		//If null return null
		while(mTip != null ) {
			//Increase length
			mLength++;
			
			//Get the heaviest child branch
			ArrayList<TxPoWTreeNode> children = mTip.getChildren();
			if(children.size() == 0) {
				break;
			}
			
			//Only keep the heaviest
			boolean firstchild = true;
			for(TxPoWTreeNode child : children) {
				if(firstchild) {
					firstchild = false;
					mTip = child;
				}else if(child.getTotalWeight().compareTo(mTip.getTotalWeight()) > 0) {
					mTip = child;
				}
			}
		}
		
		//Do this once.. used every time you receive a pulse
		calculatePulseList();
	}

	public ArrayList<MiniData> getPulseList(){
		return mPulseList;
	}
	
	/**
	 * Get the Top 256 blocks.. 
	 */
	private void calculatePulseList() {
		ArrayList<MiniData> blocklist = new ArrayList<>();
		TxPoWTreeNode current = getTip();
		int counter = 0;
		while(current!=null && counter<256) {
			blocklist.add(current.getTxPoW().getTxPoWIDData());
			current = current.getParent();
			counter++;
		}
		
		//And switch..
		mPulseList = blocklist;
	}
	
	public ArrayList<TxPoWTreeNode> getHeaviestBranch() {
		//Cycle back from the tip
		ArrayList<TxPoWTreeNode> hbranch = new ArrayList<>();
		TxPoWTreeNode current = getTip();
		while(current != null) {
			hbranch.add(current);
			current = current.getParent();
		}
		
		return hbranch;
	}	
	
	public int getHeaviestBranchLength() {
		return mLength;
	}
	
	public ArrayList<TxPoWTreeNode> setLength(int zMaxLength) {
		
		//The section of the longest branch that is removed
		ArrayList<TxPoWTreeNode> removed  = new ArrayList<>();
		
		int counter=0;
		TxPoWTreeNode newroot = null;
		TxPoWTreeNode current = getTip();
		while(current != null) {
			newroot = current;
			counter++;
			if(counter>=zMaxLength) {
				break;
			}
			
			current = current.getParent();
		}
		
		//Get the parent
		TxPoWTreeNode cascade = newroot.getParent();
				
		//Set the new root.. will make the parent null and recalculate the whole tree..
		setRoot(newroot);
		
		//Invert the list - so starts at root and moves up
		while(cascade != null) {
			//Add to the front of the list
			removed.add(0,cascade);
			
			//Move back
			cascade = cascade.getParent();
		}
		
		//Return what is being removed..
		return removed;
	}
	
	public String printTree(int zDepth) {
		StringBuffer treestr = new StringBuffer();
		
		//What is the root block..
		TxPoWTreeNode tip = getTip();
		if(tip == null) {
			return "";
		}
		
		//Cycle back..
		for(int i=0;i<zDepth-1;i++) {
			if(tip.getParent() != null) {
				tip = tip.getParent();
			}
		}
		
		MiniNumber rootblock = tip.getTxPoW().getBlockNumber();
		
		//First set all the weights to their base weight
		TxPoWTreeNodeAction printer = new TxPoWTreeNodeAction() {
			@Override
			public void runAction(TxPoWTreeNode zNode) {
				TxPoW txp 		= zNode.getTxPoW();
				
				BigDecimal weight	= txp.getWeight();
				String ID			= txp.getTxPoWID();
				MiniNumber block 	= txp.getBlockNumber();
				int sblk			= txp.getSuperLevel();
				
				//How much to indent..
				MiniNumber indent 	= block.sub(rootblock); 
				int ind 			= indent.getAsInt();
				for(int i=0;i<ind;i++) {
					if(i==ind-1) {
						treestr.append("-->");
					}else {
						treestr.append("   ");
					}
				}
				
				//Number of Transactions
				int numtxns = txp.getTransactions().size();
				if(txp.isTransaction()) {
					numtxns++;
				}
				
				//Block details
				treestr.append(" "+block+" [0/"+txp.getSuperLevel()+"] "+ID+" txns:"+numtxns+"  weight:"+weight+"/"+zNode.getTotalWeight()+" @ "+new Date(txp.getTimeMilli().getAsLong()).toString()+"\n");
			}
		}; 
		
		//Traverse the tree
		traverseTree(printer, tip);
		
		//And return the final string
		return treestr.toString();
	}
	
	private TxPoWTreeNode traverseTree(TxPoWTreeNodeAction zNodeAction) {
		return traverseTree(zNodeAction, getRoot());
	}
	
	private TxPoWTreeNode traverseTree(TxPoWTreeNodeAction zNodeAction, TxPoWTreeNode zRoot) {
		//Create a STACK..
		Stack stack = new Stack();
		
		//If nothing on chain return nothing
		if(zRoot == null) {return null;}
				
		//Push the root on the stack
		stack.push(zRoot);
		
		//Now cycle..
		while(!stack.isEmpty()) {
			//Get the top stack item
			TxPoWTreeNode node = (TxPoWTreeNode) stack.pop();
			
			//Do the action..
			zNodeAction.runAction(node);
			
    		//Have we found what we were looking for..
    		if(zNodeAction.isFinished()) {
    			return zNodeAction.getReturnNode();
    		}
    		
    		//Get any children..
    		ArrayList<TxPoWTreeNode> children = node.getChildren();
			for(TxPoWTreeNode child : children) {
				//Push on the stack
				stack.push(child);	
			}
		}
		
        return null;
	}

	public void loadDB(File zFile) {
		MiniFile.loadObject(zFile, this);
	}
	
	public void saveDB(File zFile) {
		MiniFile.saveObject(zFile, this);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//How many nodes..
		MiniNumber.WriteToStream(zOut, mTotalNodes);
		
		//Cycle through and output the whole tree
		TxPoWTreeNodeAction writeout = new TxPoWTreeNodeAction() {
			@Override
			public void runAction(TxPoWTreeNode zNode) {
				//Write it out..
				try {
					zNode.writeDataStream(zOut);
				} catch (IOException e) {
					MinimaLogger.log(e);
				}
			}
		};
		
		//Traverse the tree
		traverseTree(writeout);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		//Reset params
		mRoot 		= null;
		mTip 		= null;
		mFastLink 	= new Hashtable<>();
		mLength		= 0;
		
		//Get the TxPowDB
		TxPoWDB txpdb =  MinimaDB.getDB().getTxPoWDB();
		
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			//Read in a node..
			TxPoWTreeNode node = TxPoWTreeNode.ReadFromStream(zIn);
			
			//Add to the fast link table..
			addFastLink(node);
			
			//Add the TxPoW to the main TxPoWDB - just SQL not Mempool..
			txpdb.addSQLTxPoW(node.getTxPoW());
			
			//Add it to the tree..
			if(mRoot == null) {
				mRoot 	= node;
				mRoot.clearParent();
				mTip	= node;
			}else {
				//Find the parent.. ALWAYS here
				TxPoWTreeNode parent = findNode(node.getTxPoW().getParentID().to0xString()); 
				
				//Add to the parent
				parent.addChildNode(node);
			}
		}
		
		//Recalculate the tree
		recalculateTree();
	}
	
	
}
