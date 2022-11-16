package org.minima.database.cascade;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.MathContext;
import java.util.Date;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class CascadeNode implements Streamable{

	/**
	 * Needed to calculate the weight
	 */
	private static final BigDecimal BIGDECIMAL_TWO = new BigDecimal(2);
	
	/**
	 * Just the Header - no body
	 * @return
	 */
	TxPoW mTxPoW;
	
	int mCurrentLevel;
	int mSuperLevel;
	
	CascadeNode mParent;

	private CascadeNode() {}
	
	public CascadeNode(TxPoW zTxPoW) {
		//Use a copy
		mTxPoW = zTxPoW.deepCopy();
		
		//Remove the body
		mTxPoW.clearBody();
		
		//Zero level
		mCurrentLevel = 0;
		mSuperLevel	  = zTxPoW.getSuperLevel();
	}
	
	public TxPoW getTxPoW() {
		return mTxPoW;
	}
	
	public int getLevel() {
		return mCurrentLevel;
	}
	
	public void setLevel(int zLevel) {
		mCurrentLevel = zLevel;
	}
	
	public int getSuperLevel() {
		return mSuperLevel;
	}
	
	public void setParent(CascadeNode zCascadeNode) {
		mParent = zCascadeNode;
	}
	
	public CascadeNode getParent() {
		return mParent;
	}
	
	public BigDecimal getCurrentWeight() {
		//Calculate the weight
		BigDecimal weight 	= getTxPoW().getWeight();
		BigDecimal factor 	= BIGDECIMAL_TWO.pow(getLevel());
		return  weight.multiply(factor, MathContext.DECIMAL32);
	}
	
	@Override
	public String toString() {
		return "["+getLevel()+"/"+getSuperLevel()+"] "+mTxPoW.getTxPoWID()
					+" @ "+mTxPoW.getBlockNumber()+" weight:"+getCurrentWeight()
					+" @ "+new Date(getTxPoW().getTimeMilli().getAsLong()).toString();
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mTxPoW.writeDataStream(zOut);
		MiniNumber.WriteToStream(zOut, mCurrentLevel);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTxPoW 			= TxPoW.ReadFromStream(zIn);
		mCurrentLevel 	= MiniNumber.ReadFromStream(zIn).getAsInt();
		mSuperLevel		= mTxPoW.getSuperLevel();
	}
	
	public static CascadeNode ReadFromStream(DataInputStream zIn) throws IOException {
		CascadeNode casc = new CascadeNode();
		casc.readDataStream(zIn);
		return casc;
	}
}
