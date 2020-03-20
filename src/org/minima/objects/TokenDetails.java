package org.minima.objects;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class TokenDetails implements Streamable{

	/**
	 * The CoinID used when creating the token initially
	 */
	MiniHash  mCoinID;
	
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
	MiniString mTokenName;
	
	/**
	 * TTokenID created after all the details are set
	 */
	MiniHash mTokenID;
	
	/**
	 * Blank Constructor for ReadDataStream
	 */
	private TokenDetails() {}
	
	/**
	 * The Only Public Constructor
	 * @param zCoindID
	 * @param zScale
	 * @param zAmount
	 * @param zName
	 */
	public TokenDetails(MiniHash zCoindID, MiniNumber zScale, MiniNumber zAmount, MiniString zName) {
		mTokenScale 		= zScale;
		mTokenTotalAmount 	= zAmount;
		mTokenName 			= zName;
		mCoinID 			= zCoindID;
		
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
	
	public MiniString getName() {
		return mTokenName;
	}
	
	public MiniHash getCoinID() {
		return mCoinID;
	}
	
	public MiniHash getTokenID() {
		return mTokenID;
	}
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		obj.put("coinid", mCoinID.to0xString());
		obj.put("name", mTokenName.toString());
		obj.put("scale", mTokenScale.toString());
		obj.put("totalamount", mTokenTotalAmount.toString());
		
		obj.put("tokenid", mTokenID.to0xString());
		
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
		mTokenScale.writeDataStream(zOut);
		mTokenTotalAmount.writeDataStream(zOut);
		mTokenName.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCoinID 			= MiniHash.ReadFromStream(zIn);
		mTokenScale 		= MiniNumber.ReadFromStream(zIn);
		mTokenTotalAmount	= MiniNumber.ReadFromStream(zIn);
		mTokenName 			= MiniString.ReadFromStream(zIn);
		
		calculateTokenID();
	}
	
	public static TokenDetails ReadFromStream(DataInputStream zIn){
		TokenDetails td = new TokenDetails();
		
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
