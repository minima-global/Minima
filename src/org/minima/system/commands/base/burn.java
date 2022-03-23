package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class burn extends Command {

	MiniNumber mMinburn 	= MiniNumber.BILLION;
	MiniNumber mMaxburn 	= MiniNumber.ZERO;
	MiniNumber mBurnTot 	= MiniNumber.ZERO;
	MiniNumber mBurnCount	= MiniNumber.ZERO;
	ArrayList<MiniNumber> mValues = new ArrayList<>();
	
	MiniNumber mMinburn10 	= MiniNumber.BILLION;
	MiniNumber mMaxburn10 	= MiniNumber.ZERO;
	MiniNumber mBurnTot10 	= MiniNumber.ZERO;
	MiniNumber mBurnCount10	= MiniNumber.ZERO;
	ArrayList<MiniNumber> mValues10 = new ArrayList<>();
	
	MiniNumber mMinburn50 	= MiniNumber.BILLION;
	MiniNumber mMaxburn50 	= MiniNumber.ZERO;
	MiniNumber mBurnTot50 	= MiniNumber.ZERO;
	MiniNumber mBurnCount50	= MiniNumber.ZERO;
	ArrayList<MiniNumber> mValues50 = new ArrayList<>();
	
	public burn() {
		super("burn","View Burn metrics");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "list");
		
		JSONObject response = new JSONObject();
		
		TxPoWDB txpdb = MinimaDB.getDB().getTxPoWDB();
		
		if(action.equals("list")) {
			
			//Get the tip..
			TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
			
			//Cycle back through..
			int counter=0;
			while(tip != null && counter<50) {
				
				//Get the block
				TxPoW tiptxpow = tip.getTxPoW();
				
				//Adjust Burn values..
				checkBurn(tip.getTxPoW(), counter);
				
				//And now all the transaction in the block
				ArrayList<MiniData> txns = tiptxpow.getBlockTransactions();
				for(MiniData txn : txns) {
					
					//Get the Transaction
					TxPoW txp = txpdb.getTxPoW(txn.to0xString());
					
					//Adjust Burn values..
					checkBurn(txp, counter);
				}
				
				//And check the parent
				tip = tip.getParent();
				counter++;
			}
			
			if(mBurnCount.isEqual(MiniNumber.ZERO)) {
				mMinburn = MiniNumber.ZERO;
			}
			if(mBurnCount10.isEqual(MiniNumber.ZERO)) {
				mMinburn10 = MiniNumber.ZERO;
			}
			if(mBurnCount50.isEqual(MiniNumber.ZERO)) {
				mMinburn50 = MiniNumber.ZERO;
			}
			
			JSONObject block1 = new JSONObject();
			block1.put("txns", mBurnCount);
			block1.put("max", mMaxburn);
			block1.put("med", getMediaValue(mValues));
			if(mBurnCount.isMore(MiniNumber.ZERO)) {
				block1.put("avg", mBurnTot.div(mBurnCount));
			}else {
				block1.put("avg", MiniNumber.ZERO);
			}
			block1.put("min", mMinburn);
			
			JSONObject block10 = new JSONObject();
			block10.put("txns", mBurnCount10);
			block10.put("max", mMaxburn10);
			block10.put("med", getMediaValue(mValues10));
			if(mBurnCount10.isMore(MiniNumber.ZERO)) {
				block10.put("avg", mBurnTot10.div(mBurnCount10));
			}else {
				block10.put("avg", MiniNumber.ZERO);
			}
			block10.put("min", mMinburn10);
			
			JSONObject block50 = new JSONObject();
			block50.put("txns", mBurnCount50);
			block50.put("max", mMaxburn50);
			block50.put("med", getMediaValue(mValues50));
			if(mBurnCount50.isMore(MiniNumber.ZERO)) {
				block50.put("avg", mBurnTot50.div(mBurnCount50));
			}else {
				block50.put("avg", MiniNumber.ZERO);
			}
			block50.put("min", mMinburn50);
			
			response.put("1block",block1);
			response.put("10block",block10);
			response.put("50block",block50);
		}
		
		ret.put("response", response);
		
		return ret;
	}

	public void checkBurn(TxPoW zTxPoW, int counter) {
		if(!zTxPoW.isTransaction()) {
			return;
		}
		
		MiniNumber burn = zTxPoW.getBurn();
		
		if(counter == 0) {
			if(burn.isMore(mMaxburn)) {
				mMaxburn = burn;
			}
			if(burn.isLess(mMinburn)) {
				mMinburn = burn;
			}
			
			mBurnCount 	= mBurnCount.increment();
			mBurnTot	= mBurnTot.add(burn);
			mValues.add(burn);
		}
		
		//And the Total for the last 10
		if(counter < 10) {
			if(burn.isMore(mMaxburn10)) {
				mMaxburn10 = burn;
			}
			if(burn.isLess(mMinburn10)) {
				mMinburn10 = burn;
			}
			
			mBurnCount10 	= mBurnCount10.increment();
			mBurnTot10		= mBurnTot10.add(burn);
			mValues10.add(burn);
		}
		
		//And the last 50
		if(burn.isMore(mMaxburn50)) {
			mMaxburn50 = burn;
		}
		if(burn.isLess(mMinburn50)) {
			mMinburn50 = burn;
		}
		
		mBurnCount50 	= mBurnCount50.increment();
		mBurnTot50		= mBurnTot50.add(burn);
		mValues50.add(burn);
	}

	public MiniNumber getMediaValue(ArrayList<MiniNumber> zValues) {
		
		if(zValues.size()==0) {
			return MiniNumber.ZERO;
		}
		
		//Sort them
		Collections.sort(zValues, new Comparator<MiniNumber>() {
			@Override
			public int compare(MiniNumber o1, MiniNumber o2) {
				// TODO Auto-generated method stub
				return o2.compareTo(o1);
			}
		});
		
		//Get the median..
		int size = zValues.size();
		
		return zValues.get(size/2);
	}
	
	@Override
	public Command getFunction() {
		return new burn();
	}

}
