package org.minima.objects.proofs;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

public class Token implements Streamable{

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
	MiniNumber mTokenMinimaAmount;
	
	/**
	 * The Token Name
	 */
	MiniString mTokenName;
	
	/**
	 * The Token Script
	 */
	MiniString mTokenScript;
	
	/**
	 * TokenID created after all the details are set
	 */
	MiniData mTokenID;
	
	/**
	 * Blank Constructor for ReadDataStream
	 */
	private Token() {}
	
	/**
	 * The Only Public Constructor
	 * @param zCoindID
	 * @param zScale
	 * @param zMinimaAmount
	 * @param zName
	 */
	public Token(MiniData zCoindID, MiniNumber zScale, MiniNumber zMinimaAmount, MiniString zName, MiniString zTokenScript) {
				
		mCoinID 			= zCoindID;
		mTokenName 			= zName;
		mTokenScale 		= zScale;
		mTokenMinimaAmount 	= zMinimaAmount;
		mTokenScript        = new MiniString(zTokenScript.toString()) ;
		
		calculateTokenID();
	}
	
//	public MiniNumber getScaleFactor() {
//		return MiniNumber.TEN.pow(mTokenScale.getAsInt());
//	}
	
	public MiniNumber getScaledTokenAmount(MiniNumber zMinimaAmount) {
		int scale = mTokenScale.getAsInt();
		MiniNumber current = zMinimaAmount;
		for(int i=0;i<scale;i++){
			current = current.mult(MiniNumber.TEN);
		}
		return current;
	}
	
	public MiniNumber getScaledMinimaAmount(MiniNumber zTokenAmount) {
		int scale = mTokenScale.getAsInt();
		MiniNumber current = zTokenAmount;
		for(int i=0;i<scale;i++){
			current = current.div(MiniNumber.TEN);
		}
		return current;
	}
	
	public MiniNumber getScale() {
		return mTokenScale;
	}
	
	public MiniNumber getAmount() {
		return mTokenMinimaAmount;
	}
	
	public MiniNumber getTotalTokens() {
		return getScaledTokenAmount(mTokenMinimaAmount);
	}
	
	public MiniNumber getDecimalPlaces() {
		return new MiniNumber(MiniNumber.MAX_DECIMAL_PLACES - getScale().getAsInt());
	}
	
	public MiniString getName() {
		return mTokenName;
	}
	
	public String getShowName() {
		if(mTokenName.toString().startsWith("{")) {
			//Break it down..
			try {
				//Get the JSON..
				return getNameJSON().get("name").toString();
				
			}catch(ParseException exc){
				return getName().toString();
			}
		}else {
			return getName().toString();
		}
	}
	
	public JSONObject getNameJSON() throws ParseException {
		return (JSONObject) new JSONParser().parse(mTokenName.toString());
	}
	
	public MiniString getTokenScript() {
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
		
		//The big boy first
		obj.put("tokenid", mTokenID.to0xString());
		
		//Check if the name is a JSON..
		String name = mTokenName.toString().trim();
		
		if(name.startsWith("{")) {
			//Break it down..
			try {
				//Get the JSON..
				JSONObject tokjson = getNameJSON();
			
				//Get the name..
				obj.put("token", tokjson.get("name").toString());
				
				//Defaults
				obj.put("description", "");
				obj.put("icon", "");
				obj.put("proof", "");
				
				//Get the rest if they exist..
				if(tokjson.containsKey("description")) {
					obj.put("description", tokjson.get("description").toString());	
				}
				
				if(tokjson.containsKey("icon")) {
					obj.put("icon", tokjson.get("icon").toString());	
				}
				
				if(tokjson.containsKey("proof")) {
					obj.put("proof", tokjson.get("proof").toString());	
				}
				
			} catch (ParseException e) {
				//Incorrectly formed JSON
				obj.put("token", name);
				obj.put("description", "");
				obj.put("icon", "");
				obj.put("proof", "");
			}
				
		}else {
			obj.put("token", name);
			obj.put("description", "");
			obj.put("icon", "");
			obj.put("proof", "");	
		}
		
		obj.put("total", getTotalTokens().toString());
		obj.put("decimals", getDecimalPlaces().toString());
		obj.put("script", mTokenScript.toString());
		obj.put("coinid", mCoinID.to0xString());
		obj.put("totalamount", mTokenMinimaAmount.toString());
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
		mCoinID.writeHashToStream(zOut);
		mTokenScript.writeDataStream(zOut);
		mTokenScale.writeDataStream(zOut);
		mTokenMinimaAmount.writeDataStream(zOut);
		mTokenName.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mCoinID 			= MiniData.ReadHashFromStream(zIn);
		mTokenScript        = MiniString.ReadFromStream(zIn);
		mTokenScale 		= MiniNumber.ReadFromStream(zIn);
		mTokenMinimaAmount	= MiniNumber.ReadFromStream(zIn);
		mTokenName 			= MiniString.ReadFromStream(zIn);
		
		calculateTokenID();
	}
	
	public static Token ReadFromStream(DataInputStream zIn) throws IOException{
		Token td = new Token();
		td.readDataStream(zIn);
		return td;
	}
}
