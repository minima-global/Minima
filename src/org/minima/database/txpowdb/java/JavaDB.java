package org.minima.database.txpowdb.java;

import java.util.ArrayList;

import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.TxPowDB;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;

public class JavaDB implements TxPowDB{

	private ArrayList<JavaDBRow> mRows;
	
	public JavaDB() {
		mRows = new ArrayList<>();
	}

	@Override
	public TxPOWDBRow addTxPOWDBRow(TxPOW zTxPOW) {
		//Only add it once!
		TxPOWDBRow prev = findTxPOWDBRow(zTxPOW.getTxPowID());
		if(prev!=null) {
			return prev;
		} 
		
		//Create a new row
		JavaDBRow row = new JavaDBRow(zTxPOW);
		
		//Add it
		mRows.add(row);
		
		return row;
	}

	@Override
	public TxPOWDBRow findTxPOWDBRow(MiniHash zTxPOWID) {
		for(JavaDBRow row : mRows) {
			if(row.getTxPOW().getTxPowID().isExactlyEqual(zTxPOWID)) {
				return row;
			}
		}
		return null;
	}

	@Override
	public ArrayList<TxPOWDBRow> removeTxPOWInBlockLessThan(MiniNumber zBlockNumber) {
		ArrayList<TxPOWDBRow> removed = new ArrayList<>();
		ArrayList<JavaDBRow> newRows = new ArrayList<>();
		
		for(JavaDBRow row : mRows) {
			//Kepp ALL the onchain blocks
			if(row.isOnChainBlock()) {
				newRows.add(row);
				continue;
			}
			
			//First check.. is it used?
			if(!row.isInBlock()) {
				//is the proof still valid ? - will run out. need a new txpow..
//				MiniNumber prrofblk = row.getTxPOW()
				
				//Keep it until we add it..
				newRows.add(row);
				
			}else if(row.getInBlockNumber().isMoreEqual(zBlockNumber)) {
				newRows.add(row);
			
			}else {
				removed.add(row);
			}
		}
		
		//re-assign
		mRows = newRows;
		
		//Return the removed..
		return removed;
	}

//	@Override
//	public void resetTxPOWRowsInBlockAfter(long zBlockNumber) {
//		for(JavaDBRow row : mRows) {
//			if(row.isInBlock() && row.getInBlockNumber()>=zBlockNumber) {
//				row.setIsInBlock(false);
//			}
//		}
//	}

	@Override
	public ArrayList<TxPOWDBRow> getAllUnusedTxPOW() {
		ArrayList<TxPOWDBRow> ret = new ArrayList<>();
		
		for(JavaDBRow row : mRows) {
			if(!row.isInBlock()) {
				ret.add(row);
			}
		}
		
		return ret;
	}

	@Override
	public int getSize() {
		return mRows.size();
	}

	@Override
	public void removeTxPOW(MiniHash zTxPOWID) {
		ArrayList<JavaDBRow> newRows = new ArrayList<>();
		
		for(JavaDBRow row : mRows) {
			if( !row.getTxPOW().getTxPowID().isExactlyEqual(zTxPOWID)) {
				newRows.add(row);
			}
		}
		
		//re-assign
		mRows = newRows;
	}

	@Override
	public ArrayList<TxPOWDBRow> getChildBlocksTxPOW(MiniHash zParent) {
		ArrayList<TxPOWDBRow> ret = new ArrayList<>();
		
		for(JavaDBRow row : mRows) {
			if(row.getTxPOW().isBlock() && row.getTxPOW().getParentID().isExactlyEqual(zParent)) {
				ret.add(row);
			}
		}
		
		return ret;
	}

	@Override
	public ArrayList<TxPOWDBRow> getAllTxPOWDBRow() {
		ArrayList<TxPOWDBRow> copy = new ArrayList<>();
		for(TxPOWDBRow row : mRows) {
			copy.add(row);
		}
		return copy;
	}

	@Override
	public void resetAllInBlocks() {
		for(TxPOWDBRow row : mRows) {
			row.setIsInBlock(false);
			row.setOnChainBlock(false);
		}
	}

	@Override
	public ArrayList<TxPOWDBRow> getAllBlocksMissingTransactions() {
		ArrayList<TxPOWDBRow> ret = new ArrayList<>();
		
		for(JavaDBRow row : mRows) {
			if( row.getTxPOW().isBlock() && row.getBlockState() == TxPOWDBRow.TXPOWDBROW_STATE_BASIC ) {
				ret.add(row);
			}
		}
		
		return ret;
	}

	@Override
	public void ClearDB() {
		mRows = new ArrayList<>();
	}
	
}
