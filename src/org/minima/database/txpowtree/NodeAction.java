package org.minima.database.txpowtree;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;

public abstract class NodeAction {

	/**
	 * Looking for something..
	 */
	BlockTreeNode mReturnNode = null;
	
	/**
	 * Extra Data
	 */
	MiniData mExtraData = null;
	
	/**
	 * The Minima Main DB
	 */
	MinimaDB mDB = null;
	
	public NodeAction() {
	}
	
	public NodeAction(MiniData zExtraData) {
		mReturnNode = null;
		mExtraData  = zExtraData;
	}
	
	public NodeAction(MinimaDB zDB) {
		mReturnNode = null;
		mExtraData  = null;
		mDB = zDB;
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
	
	public MinimaDB getDB() {
		return mDB;
	}
	
	public abstract void runAction(BlockTreeNode zNode);
}
