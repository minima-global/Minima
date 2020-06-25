package org.minima.database.txpowdb.java;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.TxPowDB;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class FastJavaDB implements TxPowDB {

	private Hashtable<String,JavaDBRow> mTxPoWRows;
	
	public FastJavaDB() {
		mTxPoWRows = new Hashtable<>();
	}
	
	@Override
	public TxPOWDBRow findTxPOWDBRow(MiniData zTxPOWID) {
		return mTxPoWRows.get(zTxPOWID.to0xString());
	}
	
	@Override
	public TxPOWDBRow addTxPOWDBRow(TxPoW zTxPOW) {
		String search = zTxPOW.getTxPowID().to0xString();
		
		//Is it already in there
		JavaDBRow row = mTxPoWRows.get(search);
		if(row != null) {
			return row;
		}
		
		//Create it
		row = new JavaDBRow(zTxPOW);
				
		//Add it..
		mTxPoWRows.put(search, row);
		
		return row;
	}

	@Override
	public ArrayList<TxPOWDBRow> getAllTxPOWDBRow() {
		ArrayList<TxPOWDBRow> copy = new ArrayList<>();
		Enumeration<JavaDBRow> allrows = mTxPoWRows.elements();
		while(allrows.hasMoreElements()) {
			JavaDBRow row = allrows.nextElement();
			copy.add(row);
		}
		return copy;
	}

	@Override
	public void resetAllInBlocks() {
		Enumeration<JavaDBRow> allrows = mTxPoWRows.elements();
		while(allrows.hasMoreElements()) {
			JavaDBRow row = allrows.nextElement();
			row.setIsInBlock(false);
			row.setOnChainBlock(false);
		}
	}

	@Override
	public void resetBlocksFromOnwards(MiniNumber zFromBlock) {
		Enumeration<JavaDBRow> allrows = mTxPoWRows.elements();
		while(allrows.hasMoreElements()) {
			JavaDBRow row = allrows.nextElement();
			if(row.isInBlock() && row.getInBlockNumber().isMoreEqual(zFromBlock)) {
				row.setIsInBlock(false);
				row.setOnChainBlock(false);
			}
		}
	}

	@Override
	public void removeTxPOW(MiniData zTxPOWID) {
		mTxPoWRows.remove(zTxPOWID.to0xString());
	}

	@Override
	public ArrayList<TxPOWDBRow> removeTxPOWInBlockLessThan(MiniNumber zCascade) {
		Hashtable<String,JavaDBRow> newtable = new Hashtable<>();
		ArrayList<TxPOWDBRow> removed = new ArrayList<>();
		
		//The minimum block before its too late for a USED TxPoW
		MiniNumber minused = zCascade.sub(MiniNumber.SIXTYFOUR);
		
		//The minimum block before its too late for an UNUSED TxPoW
		MiniNumber minunused = zCascade.add(MiniNumber.TWOFIVESIX);
				
		Enumeration<JavaDBRow> allrows = mTxPoWRows.elements();
		while(allrows.hasMoreElements()) {
			JavaDBRow row  = allrows.nextElement();
			TxPoW rowtxpow = row.getTxPOW();
			
				//It's a main block
			if(row.isOnChainBlock()) {
				newtable.put(rowtxpow.getTxPowID().to0xString(),row);
				
				//It's a transaction on the main chain
			}else if(row.isInBlock() && row.getInBlockNumber().isMoreEqual(minused)) {
				newtable.put(rowtxpow.getTxPowID().to0xString(),row);
			
				//It's a transaction but not that old
			}else if(rowtxpow.isTransaction() && !row.isInBlock() && row.getTxPOW().getBlockNumber().isMoreEqual(minunused)) {
				newtable.put(rowtxpow.getTxPowID().to0xString(),row);
			
				//It's a block but not past the cascade
			}else if(rowtxpow.isBlock() && !row.isOnChainBlock() && row.getTxPOW().getBlockNumber().isMoreEqual(minused)) {
				newtable.put(rowtxpow.getTxPowID().to0xString(),row);
				
			}else {
				//Remove it..
				removed.add(row);
			}		
		}
		
		//Switch to the new table..
		mTxPoWRows = newtable;
		
		return removed;
	}

	@Override
	public ArrayList<TxPOWDBRow> getAllUnusedTxPOW() {
		ArrayList<TxPOWDBRow> ret = new ArrayList<>();
		Enumeration<JavaDBRow> allrows = mTxPoWRows.elements();
		while(allrows.hasMoreElements()) {
			JavaDBRow row = allrows.nextElement();	
			if(!row.isInBlock()) {
				ret.add(row);
			}
		}
		return ret;
	}

	@Override
	public ArrayList<TxPOWDBRow> getChildBlocksTxPOW(MiniData zParent) {
		ArrayList<TxPOWDBRow> ret = new ArrayList<>();
		Enumeration<JavaDBRow> allrows = mTxPoWRows.elements();
		while(allrows.hasMoreElements()) {
			JavaDBRow row = allrows.nextElement();
			if(row.getTxPOW().isBlock() && row.getTxPOW().getParentID().isEqual(zParent)) {
				ret.add(row);
			}
		}
		
		return ret;
	}

	@Override
	public ArrayList<TxPOWDBRow> getAllBlocksMissingTransactions() {
		ArrayList<TxPOWDBRow> ret = new ArrayList<>();
		Enumeration<JavaDBRow> allrows = mTxPoWRows.elements();
		while(allrows.hasMoreElements()) {
			JavaDBRow row = allrows.nextElement();
			if(row.getTxPOW().isBlock() && row.getBlockState() == TxPOWDBRow.TXPOWDBROW_STATE_BASIC) {
				ret.add(row);
			}
		}
		
		return ret;
	}
	
	@Override
	public int getSize() {
		return mTxPoWRows.size();
	}

	@Override
	public void ClearDB() {
		mTxPoWRows.clear();
	}
}
