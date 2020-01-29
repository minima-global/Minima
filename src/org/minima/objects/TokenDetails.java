package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class TokenDetails implements Streamable{

	/**
	 * The Scale of the Token vs the amount
	 */
	public MiniNumber mTokenScale 			= new MiniNumber();
	
	/**
	 * The total amount of Minima Used
	 */
	public MiniNumber mTokenTotalAmount 	= new MiniNumber();
	
	/**
	 * The Token Name
	 */
	public MiniString mTokenName 			= new MiniString("");
	
	public TokenDetails() {}
	
	public void setScale(MiniNumber zScale){
		mTokenScale = zScale;
	}
	
	public void setAMount(MiniNumber zAmount){
		mTokenTotalAmount = zAmount;
	}
	
	public void setName(MiniString zName){
		mTokenName = zName;
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
	
	public JSONObject toJSON() {
		JSONObject obj = new JSONObject();
		
		obj.put("name", mTokenName);
		obj.put("scale", mTokenScale);
		obj.put("amount", mTokenTotalAmount);
		
		return obj;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mTokenScale.writeDataStream(zOut);
		mTokenTotalAmount.writeDataStream(zOut);
		mTokenName.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTokenScale.readDataStream(zIn);
		mTokenTotalAmount.readDataStream(zIn);
		mTokenName.readDataStream(zIn);
	}
}
