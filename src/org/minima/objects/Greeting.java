package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class Greeting implements Streamable {

	/**
	 * What version of Minima
	 */
	MiniString mVersion = new MiniString(GlobalParams.MINIMA_VERSION);
	
	/**
	 * Extra information sent in the greeting
	 */
	JSONObject mExtraData = new JSONObject();
	
	/**
	 * The block number of the top block
	 */
	MiniNumber mTopBlock = MiniNumber.ZERO;
	
	/**
	 * The hash chain of the txpow in the current chain - from top down to root of Tree
	 */
	ArrayList<MiniData> mChain = new ArrayList<>();
	
	public Greeting() {}
	
	/**
	 * Create the complete greeting message
	 */
	public Greeting createGreeting() {
		//Lock the DB
		MinimaDB.getDB().readLock(true);
		
		try {
			//Add some extra info
			getExtraData().put("welcome", MinimaDB.getDB().getUserDB().getWelcome());
			
			//What is my Host / Port
			if(GeneralParams.IS_HOST_SET) {
				getExtraData().put("host",GeneralParams.MINIMA_HOST);
			}
			getExtraData().put("port",""+GeneralParams.MINIMA_PORT);
			
			//Add the chain..
			TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
			if(tip == null) {
				//First time user
				setTopBlock(MiniNumber.MINUSONE);
			}else {
				setTopBlock(tip.getTxPoW().getBlockNumber());
			}
			
			//Add all the chain
			while(tip != null) {
				mChain.add(tip.getTxPoW().getTxPoWIDData());
				tip = tip.getParent();
			}
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Unlock..
		MinimaDB.getDB().readLock(false);
		
		return this;
	}
	
	public JSONObject getExtraData() {
		return mExtraData;
	}
	
	public String getExtraDataValue(String zKey) {
		return (String) mExtraData.get(zKey);
	}
	
	public void setTopBlock(MiniNumber zTopBlock) {
		mTopBlock = zTopBlock;
	}
	
	public MiniNumber getTopBlock() {
		return mTopBlock;
	}
	
	public MiniString getVersion() {
		return mVersion;
	}
	
	public MiniNumber getRootBlock() {
		if(mTopBlock.isEqual(MiniNumber.MINUSONE)) {
			return MiniNumber.MINUSONE;
		}
		
		return mTopBlock.sub(new MiniNumber(mChain.size()-1));
	}
	
	public ArrayList<MiniData> getChain(){
		return mChain;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mVersion.writeDataStream(zOut);
		
		MiniString json = new MiniString(mExtraData.toString());
		json.writeDataStream(zOut);
		
		mTopBlock.writeDataStream(zOut);
		
		int len = mChain.size();
		MiniNumber.WriteToStream(zOut, len);
		for(MiniData txpowid : mChain) {
			txpowid.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mVersion = MiniString.ReadFromStream(zIn);
		
		MiniString json = MiniString.ReadFromStream(zIn);
		try {
			mExtraData = (JSONObject)new JSONParser().parse(json.toString());
		} catch (ParseException e) {
			mExtraData = new JSONObject();
		}  		
		
		mTopBlock = MiniNumber.ReadFromStream(zIn);
		
		mChain = new ArrayList<>();
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mChain.add(MiniData.ReadFromStream(zIn));
		}
	}

	public static Greeting ReadFromStream(DataInputStream zIn) throws IOException{
		Greeting greet = new Greeting();
		greet.readDataStream(zIn);
		return greet;
	}
	
}
