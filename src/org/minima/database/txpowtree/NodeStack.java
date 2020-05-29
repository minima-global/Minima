package org.minima.database.txpowtree;

import java.util.ArrayList;

public class NodeStack {
	
	ArrayList<BlockTreeNode> mStack;
	
	public NodeStack() {
		mStack = new ArrayList<>();
	}
	
	/**
	 * Add a node to the top of the Stack
	 * 
	 * @param zNode
	 */
	public void push(BlockTreeNode zNode) {
		mStack.add(zNode);
	}
	
	/**
	 * Pop a node off the top of the stack
	 * @return
	 */
	public BlockTreeNode pop() {
		if(isEmpty()) {
			return null;
		}
		
		return mStack.remove(mStack.size()-1);
	}
	
	/**
	 * Peek at the top node
	 * @return
	 */
	public BlockTreeNode peek() {
		return mStack.get(mStack.size()-1);
	}

	/**
	 * Empty Stack 
	 * @return
	 */
	public boolean isEmpty() {
		return mStack.size() == 0;
	}
}
