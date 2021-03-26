package org.minima.objects.greet;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.GlobalParams;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class Greeting implements Streamable {
		
	/**
	 * What version are we..
	 */
	MiniString mVersion = new MiniString(GlobalParams.MINIMA_VERSION);
	
	/**
	 * The Top block in our list
	 */
	MiniNumber mTopBlock;
	
	/**
	 * The Top block in our list
	 */
	MiniNumber mStartBlock;
	
	/**
	 * A complete list of all the hashes in our blockchain 
	 */
	ArrayList<MiniData> mTxPowList = new ArrayList<>(); 
	
	/**
	 * A JSONObject of extra information
	 */
	JSONObject mDetails = new JSONObject();
	
	public Greeting() {
		mTopBlock   = MiniNumber.MINUSONE;
		mStartBlock = MiniNumber.MINUSONE;
	}
	
	public void setTopBlock(MiniNumber zTopBlock) {
		mTopBlock = zTopBlock;
	}
	
	public MiniNumber getTopBlock() {
		return mTopBlock;
	}
	
	public MiniNumber getFirstBlock() {
		return mStartBlock;
	}
	
	public void addBlock(TxPoW zBlock){
		MiniNumber blknum = zBlock.getBlockNumber();
		
		//Check..
		if(mTopBlock.isEqual(MiniNumber.MINUSONE)) {
			mTopBlock = blknum;
		}else if(blknum.isMore(mTopBlock)) {
			mTopBlock = blknum;
		}
		
		if(mStartBlock.isEqual(MiniNumber.MINUSONE)) {
			mStartBlock = blknum;
		}else if(blknum.isLess(mStartBlock)) {
			mStartBlock = blknum;
		}
		
		mTxPowList.add(zBlock.getTxPowID());
	}
	
	public ArrayList<MiniData> getList() {
		return mTxPowList;
	}
	
	public String getVersion() {
		return mVersion.toString();
	}
	
	public JSONObject getDetails() {
		return mDetails;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//First the version.. 
		mVersion.writeDataStream(zOut);
		
		//The Top Block
		mTopBlock.writeDataStream(zOut);
		mStartBlock.writeDataStream(zOut);
		
		//Next the details..
		int len = mTxPowList.size();
		MiniNumber size = new MiniNumber(len);
		size.writeDataStream(zOut);
		
		//Write it all out..
		for(MiniData txpowid : mTxPowList) {
			txpowid.writeDataStream(zOut);
		}
		
		//Write out the details..
		MiniString str  = new MiniString(mDetails.toString());
		str.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mVersion = MiniString.ReadFromStream(zIn);
		
		mTopBlock   = MiniNumber.ReadFromStream(zIn);
		mStartBlock = MiniNumber.ReadFromStream(zIn);
		
		MiniNumber minlen = MiniNumber.ReadFromStream(zIn);
	
		mTxPowList = new ArrayList<>();
		int len = minlen.getAsInt();
		for(int i=0;i<len;i++) {
			mTxPowList.add(MiniData.ReadFromStream(zIn));
		}
		
		//Read the details..
		MiniString json = MiniString.ReadFromStream(zIn);
		try {
			mDetails = (JSONObject) new JSONParser().parse(json.toString());
		} catch (ParseException e) {
			MinimaLogger.log(e);
			mDetails = new JSONObject();
		} 
	}
	
	public static Greeting ReadFromStream(DataInputStream zIn) throws IOException {
		Greeting greet = new Greeting();
		greet.readDataStream(zIn);
		return greet;
	}
}
