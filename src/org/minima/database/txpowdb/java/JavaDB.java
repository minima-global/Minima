package org.minima.database.txpowdb.java;

import java.util.ArrayList;

import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.TxPowDB;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class JavaDB implements TxPowDB{

	private ArrayList<JavaDBRow> mRows;
	private ArrayList<JavaDBRow> mDeletedRows;
	
	public JavaDB() {
		mRows = new ArrayList<>();
		mDeletedRows = new ArrayList<>();
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

	/**
	 * Searched the Deleted Rows TOO!
	 */
	@Override
	public TxPOWDBRow findTxPOWDBRow(MiniData zTxPOWID) {
		for(JavaDBRow row : mRows) {
			if(row.getTxPOW().getTxPowID().isEqual(zTxPOWID)) {
				return row;
			}
		}
		
		for(JavaDBRow row : mDeletedRows) {
			if(row.getTxPOW().getTxPowID().isEqual(zTxPOWID)) {
				return row;
			}
		}
		
		return null;
	}

	@Override
	public ArrayList<TxPOWDBRow> removeTxPOWInBlockLessThan(MiniNumber zBlockNumber) {
		ArrayList<TxPOWDBRow> removed = new ArrayList<>();
		ArrayList<JavaDBRow> newRows = new ArrayList<>();
		
		//The minimum block before its too late - TODO!
		MiniNumber minblock = zBlockNumber.add(MiniNumber.TEN);
		
		for(JavaDBRow row : mRows) {
			
			if(row.isOnChainBlock()) {
				newRows.add(row);
				
				//Other wise the proofs are too old..
			}else if(!row.isInBlock() && row.getTxPOW().getBlockNumber().isMore(minblock)) {
				newRows.add(row);
			
				//It's in the chain
			}else if(row.isInBlock() && row.getInBlockNumber().isMoreEqual(zBlockNumber)) {
				newRows.add(row);
			
			}else {
//				if(!row.isInBlock()) {
//					System.out.println("UNUSED TXPoW Removed.. too old "
//						+row.getTxPOW().getBlockNumber()+" "+minblock+" "+row.getTxPOW().toJSON());	
//				}
				
				//Remove it..
				removed.add(row);
				
				//Add to the deleted rows
				row.deleteRow();
				mDeletedRows.add(row);
			}
		}
		
		//re-assign
		mRows = newRows;
		
		//Remove the deleted.. called periodically
		removeDeleted();
		
		//Return the removed..
		return removed;
	}

	private ArrayList<TxPOWDBRow> removeDeleted() {
		ArrayList<TxPOWDBRow> removed = new ArrayList<>();
		ArrayList<JavaDBRow> newDeletedRows = new ArrayList<>();

		//Keep for 1 HR in the past
		long timedelete = System.currentTimeMillis() - 1000*60*60;
		
		for(JavaDBRow row : mDeletedRows) {
			if(row.getDeleteTime() == 0) {
				newDeletedRows.add(row);
			}else if(row.getDeleteTime() > timedelete) {
				newDeletedRows.add(row);
			}else {
				removed.add(row);
			}
		}
		
		//Reset
		mDeletedRows = newDeletedRows;
		
		return removed;
	}

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
	public int getCompleteSize() {
		return mRows.size()+ mDeletedRows.size();
	}

	@Override
	public void removeTxPOW(MiniData zTxPOWID) {
		ArrayList<JavaDBRow> newRows = new ArrayList<>();
		
		for(JavaDBRow row : mRows) {
			if( !row.getTxPOW().getTxPowID().isEqual(zTxPOWID)) {
				newRows.add(row);
				
				//Add to the deleted rows..
				row.deleteRow();
				mDeletedRows.add(row);
			}
		}
		
		//re-assign
		mRows = newRows;
	}

	@Override
	public ArrayList<TxPOWDBRow> getChildBlocksTxPOW(MiniData zParent) {
		ArrayList<TxPOWDBRow> ret = new ArrayList<>();
		
		for(JavaDBRow row : mRows) {
			if(row.getTxPOW().isBlock() && row.getTxPOW().getParentID().isEqual(zParent)) {
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
		mDeletedRows = new ArrayList<>();
	}
	
}
