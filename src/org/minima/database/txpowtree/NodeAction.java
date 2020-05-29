package org.minima.database.txpowtree;

import org.minima.objects.base.MiniData;

public abstract class NodeAction {

	/**
	 * Looking for something..
	 */
	BlockTreeNode mReturnNode;
	
	/**
	 * Extra Data
	 */
	MiniData mExtraData;
	
	public NodeAction() {
		this(null);
	}
	
	public NodeAction(MiniData zExtraData) {
		mReturnNode = null;
		mExtraData  = zExtraData;
	}
	
	public boolean returnObject() {
		return mReturnNode != null;
	}
	
	public BlockTreeNode getObject() {
		return mReturnNode;
	}
	
	public void setReturnObject(BlockTreeNode zNode) {
		mReturnNode = zNode;
	}
	
	public MiniData getExtraData() {
		return mExtraData;
	}
	
	public abstract void runAction(BlockTreeNode zNode);
}
