package org.minima.database.txpowtree;

public abstract class TxPoWTreeNodeAction {

	/**
	 * Looking for something..
	 */
	TxPoWTreeNode mReturnNode = null;
	
	/**
	 * Extra Data
	 */
	String mExtraData = null;
	
	public TxPoWTreeNodeAction() {}
	
	public TxPoWTreeNodeAction(String zExtraData) {
		mReturnNode = null;
		mExtraData  = zExtraData;
	}
	
	public boolean isFinished() {
		return mReturnNode != null;
	}
	
	public TxPoWTreeNode getReturnNode() {
		return mReturnNode;
	}
	
	public void setReturnObject(TxPoWTreeNode zNode) {
		mReturnNode = zNode;
	}
	
	public String getExtraData() {
		return mExtraData;
	}
	
	/**
	 * Main Recurse function..
	 * 
	 * @param zNode
	 * @throws Exception
	 */
	public abstract void runAction(TxPoWTreeNode zNode);
}
