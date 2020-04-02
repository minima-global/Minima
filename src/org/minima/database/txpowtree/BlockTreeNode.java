package org.minima.database.txpowtree;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.ArrayList;
import java.util.Collections;

import org.minima.database.mmr.MMRSet;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.utils.Crypto;

public class BlockTreeNode implements Comparable<BlockTreeNode> {

	public static final BigInteger BIG_TWO    = new BigInteger("2");
	public static final BigDecimal MAX_VALDEC = new BigDecimal(Crypto.MAX_HASH.getDataValue()); 
	
	/**
	 * Block States
	 */
	public static final int BLOCKSTATE_BASIC   = 10;
	public static final int BLOCKSTATE_VALID   = 11;
	public static final int BLOCKSTATE_INVALID = 12;
	
	//The state of this Block
	private int mBlockState = BLOCKSTATE_BASIC;
	
	//The TxPOW
	private TxPOW mTXPOW;
	
	//What Super Block Level is this TxPOW
	private int mSuperBlockLevel;
	
	//What current level is this chain node
	private int mCurrentLevel;
	
	//The Children.. of the niiiight..
	private ArrayList<BlockTreeNode> mChildren = new ArrayList<>();
	
	//Parent Node..
	private BlockTreeNode mParent = null;
	
	//Is this a cascade node..
	private boolean mCascade = false;
	
	/**
	 * When calculating the heaviest Branch - what is the weight of this block
	 */
	private BigInteger		mTotalWeight		= BigInteger.ZERO;
	private BigInteger		mWeight				= BigInteger.ZERO;
	private BigDecimal		mRealWeight		    = BigDecimal.ZERO;
	
	/**
	 * The Finalized MMRset for this block
	 */
	MMRSet mMMRSet = new MMRSet();
	
	/**
	 * When loading from bloc ctore just use the TxpowID
	 */
	public BlockTreeNode() {}
	
	/**
	 * Main Constructor of this immutable object
	 * 
	 * @param zTxPowDBRow
	 */
	public BlockTreeNode(TxPOW zTxPow) {
		init(zTxPow);
	}

	private void init(TxPOW zTxPow) {
		//Store the TXPOW
		mTXPOW 	= zTxPow;
		
		//What Super Level Block Is This..
		mSuperBlockLevel = mTXPOW.getSuperLevel();
		
		//Start at the block difficulty
		mCurrentLevel	 = 0;
		
		//Get the Block Difficulty
		mRealWeight = MAX_VALDEC.divide(getTxPow().getBlockDifficulty().getDataValueDecimal(), MathContext.DECIMAL128);
				
		//Set the current weight
		resetCurrentWeight();
	}
	
	public BlockTreeNode(BlockTreeNode zNode) {
		//Store the TXPOW
		mTXPOW = zNode.getTxPow();
		
		//What Level Block Is This..
		mSuperBlockLevel = zNode.getSuperBlockLevel();
		
		//Start at the block difficulty
		mCurrentLevel	 = zNode.getCurrentLevel();
		
		//Get the Block Difficulty
		mRealWeight = MAX_VALDEC.divide(getTxPow().getBlockDifficulty().getDataValueDecimal(), MathContext.DECIMAL128);
				
		//Set the correct MMR
		setMMRset(zNode.getMMRSet());
		
		//Set if it is a cascader
		setCascade(zNode.isCascade());
		
		//Set the state
		setState(zNode.getState());
		
		//Set the Weight
		resetCurrentWeight();
	}
	
	public void setState(int zState) {
		mBlockState = zState;
	}
	
	public int getState() {
		return mBlockState;
	}
	
	public void setMMRset(MMRSet zMMRSet) {
		mMMRSet = zMMRSet;
	}
	
	public MMRSet getMMRSet() {
		return mMMRSet;
	}
	
	public void setCascade(boolean zCascade) {
		mCascade = zCascade;
	}
	
	public boolean isCascade() {
		return mCascade;
	}
	
	public void resetCurrentWeight() {
		//Now multiply by the current level pow 2
		BigDecimal factor = new BigDecimal(BIG_TWO.pow(getCurrentLevel()), MathContext.DECIMAL128) ;
		
		//Set the Weight
		mWeight = mRealWeight.multiply(factor, MathContext.DECIMAL128).toBigInteger();
		
		//Reset the total weight..
		mTotalWeight = mWeight;
	}
	
	public BigInteger getWeight() {
		return mWeight;
	}
	public void addToTotalWeight(BigInteger zTotal) {
		mTotalWeight = mTotalWeight.add(zTotal);
	}
	
	public BigInteger getTotalWeight() {
		return mTotalWeight;
	}
	
	public TxPOW getTxPow() {
		return mTXPOW;
	}
		
	public int getSuperBlockLevel() {
		return mSuperBlockLevel;
	}
	
	public int getCurrentLevel() {
		return mCurrentLevel;
	}
	
	public void setCurrentLevel(int zLevel) {
		mCurrentLevel = zLevel;
		resetCurrentWeight();
	}
	
	public MiniData getTxPowID() {
		return getTxPow().getTxPowID();
	}
	
	public void setParent(BlockTreeNode zParent) {
		mParent = zParent;
	}
	
	public BlockTreeNode getParent() {
		return mParent;
	}
	
	public void addChild(BlockTreeNode zChild) {
		//Add child to this Block
		mChildren.add(zChild);
		
		//Set Parent for child
		zChild.setParent(this);
		
		// Order the child nodes.. This way it looks the same in 
		// the TREE on different devices
		Collections.sort(mChildren);		
	}
	
	public ArrayList<BlockTreeNode> getChildren() {
		return mChildren;
	}
	
	public int getNumberChildren(){
		return mChildren.size();
	}
	
	public BlockTreeNode getChild(int zChild) {
		return mChildren.get(zChild);
	}

	@Override
	public int compareTo(BlockTreeNode o) {
		return o.getTxPowID().compare(getTxPowID());
	}
	
	@Override
	public String toString() {
		return "["+getCurrentLevel()+"/"+getSuperBlockLevel()+"] casc:"+isCascade()+" state:"+getState()+" "+mTXPOW.toString();
	}
	
}

