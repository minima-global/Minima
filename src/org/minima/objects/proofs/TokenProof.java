package org.minima.objects.proofs;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.kissvm.Contract;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniScript;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class TokenProof implements Streamable{

	/**
	 * The CoinID used when creating the token initially
	 */
	MiniData  mCoinID;
	
	/**
	 * The Scale of the Token vs the amount
	 */
	MiniNumber mTokenScale;
	
	/**
	 * The total amount of Minima Used
	 */
	MiniNumber mTokenTotalAmount;
	
	/**
	 * The Token Name
	 */
	MiniScript mTokenName;
	
	/**
	 * The Token Script
	 */
	MiniScript mTokenScript;
	
	/**
	 * TTokenID created after all the details are set
	 */
	MiniData mTokenID;
	
	/**
	 * Blank Constructor for ReadDataStream
	 */
	private TokenProof() {}
	
	/**
	 * The Only Public Constructor
	 * @param zCoindID
	 * @param zScale
	 * @param zAmount
	 * @param zName
	 */
	public TokenProof(MiniData zCoindID, MiniNumber zScale, MiniNumber zAmount, MiniScript zName, MiniScript zTokenScript) {
				
		mTokenScale 		= zScale;
		mTokenTotalAmount 	= zAmount;
		mTokenName 			= zName;
		mCoinID 			= zCoindID;
		mTokenScript        = new MiniScript(zTokenScript.toString()) ;
		
		calculateTokenID();
	}
	
	public MiniNumber getScaleFactor() {
		return MiniNumber.TEN.pow(mTokenScale.getAsInt());
	}
	
	public MiniNumber getScale() {
		return mTokenScale;
	}
	
	public MiniNumber getAmount() {
		return mTokenTotalAmount;
	}
	
	public MiniScript getName() {
		return mTokenName;
	}
	
	public MiniScript getTokenScript() {
		return mTokenScript;
	}
	
	public MiniData getCoinID() {
		return mCoinID;
	}
	
	public MiniData getTokenID() {
		return mTokenID;
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		obj.put("tokenid", mTokenID.to0xString());
		obj.put("token", mTokenName.toString());
		
		MiniNumber total = mTokenTotalAmount.mult(getScaleFactor());
		obj.put("total", total);
		
		obj.put("script", mTokenScript.toString());
		
		obj.put("coinid", mCoinID.to0xString());
		obj.put("totalamount", mTokenTotalAmount.toString());
		obj.put("scale", mTokenScale.toString());
		
		
		return obj;
	}
	
	private void calculateTokenID() {
		try {
			//Make it the HASH ( CoinID | Total Amount )
			ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
			DataOutputStream daos 		= new DataOutputStream(baos);
			
			//Write the details to the stream
			writeDataStream(daos);
			
			//Push It
			daos.flush();
			
			//Create a MiniData..
			MiniData tokdat = new MiniData(baos.toByteArray());
			
			//Now Hash it..
			mTokenID = Crypto.getInstance().hashObject(tokdat);
			
			//Clean up
			daos.close();
			baos.close();
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mCoinID.writeDataStream(zOut);
		mTokenScript.writeDataStream(zOut);
		mTokenScale.writeDataStream(zOut);
		mTokenTotalAmount.writeDataStream(zOut);
		mTokenName.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCoinID 			= MiniData.ReadFromStream(zIn);
		mTokenScript        = MiniScript.ReadFromStream(zIn);
		mTokenScale 		= MiniNumber.ReadFromStream(zIn);
		mTokenTotalAmount	= MiniNumber.ReadFromStream(zIn);
		mTokenName 			= MiniScript.ReadFromStream(zIn);
		
		calculateTokenID();
	}
	
	public static TokenProof ReadFromStream(DataInputStream zIn){
		TokenProof td = new TokenProof();
		
		try {
			td.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		
		return td;
	}
}
