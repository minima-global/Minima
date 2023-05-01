package org.minima.database.archive;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.concurrent.ConcurrentHashMap;

import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;

public class TxBlockDB {
	
	ConcurrentHashMap<String, TxBlock> mTxBlockDB;
	
	public TxBlockDB() {
		mTxBlockDB = new ConcurrentHashMap<>();
	}
	
	public synchronized void addTxBlock(TxBlock zTxBlock) {
		mTxBlockDB.put(zTxBlock.getTxPoW().getTxPoWID(), zTxBlock);
	}
	
	public synchronized TxBlock findTxBlock(String zTxPowID) {
		return mTxBlockDB.get(zTxPowID);
	}
	
	public synchronized ArrayList<TxBlock> getChildBlocks(String zTxPowID){
		
		ArrayList<TxBlock> ret = new ArrayList<>();
		
		//Cycle through the blocks..
		Enumeration<TxBlock> allblocks = mTxBlockDB.elements();
		while(allblocks.hasMoreElements()) {
			
			TxBlock txblock = allblocks.nextElement();
			
			//Is it a child..
			if(txblock.getTxPoW().getParentID().to0xString().equals(zTxPowID)) {
				ret.add(txblock);
			}
		}
		
		return ret;
	}
	
	public synchronized void clearAll() {
		mTxBlockDB.clear();
	}
	
	public synchronized void clearOld(MiniNumber zMinBlock) {
	
		ConcurrentHashMap<String, TxBlock> newDB = new ConcurrentHashMap();
		
		Enumeration<TxBlock> allblocks = mTxBlockDB.elements();
		while(allblocks.hasMoreElements()) {
			
			TxBlock txblock = allblocks.nextElement();
			
			if(txblock.getTxPoW().getBlockNumber().isMoreEqual(zMinBlock)) {
				newDB.put(txblock.getTxPoW().getTxPoWID(), txblock);
			}
		}
		
		mTxBlockDB = newDB;
	}
}
