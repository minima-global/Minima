package org.minima.objects;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

/**
 * A transaction is a very simple structure. A list of inputs and a list of outputs. 
 * All the Witness data is Segregated..  So that there is no TXN malleability possible.
 * The CoinID of an output is the HASH ( TXN hash + Output Num ), which is ALWAYS Globally Unique.
 * 
 * @author spartacus
 *
 */
public class Transaction implements Streamable {

	/**
	 * The Hash of a prior transaction if this is a burn transaction
	 * MUST Be 0x00 to be a normal transaction.
	 */
	protected MiniData mLinkHash = new MiniData("0x00");
	
	/**
	 * The Inputs that make up the Transaction
	 */
	protected ArrayList<Coin> mInputs  = new ArrayList<>();
	
	/**
	 * All the Outputs
	 */
	protected ArrayList<Coin> mOutputs = new ArrayList<>();
	
	/**
	 * The State values of the Transaction
	 */
	protected ArrayList<StateVariable> mState = new ArrayList<>();
	
	/**
	 * If you are generating a TOKEN.. here are the details..
	 * Needs to be here instead of witness so no-one can alter it - you sign this.
	 */
	protected TokenProof mTokenGenDetails = null;
	
	/**
	 * Constructor
	 */
	public Transaction() {}
	
	public void addInput(Coin zCoin) {
		mInputs.add(zCoin);
	}
	
	public void addInput(Coin zCoin, int zPosition) {
		mInputs.add(zPosition, zCoin);
	}
	
	public void addOutput(Coin zCoin) {
		mOutputs.add(zCoin);
	}
	
	public void addOutput(Coin zCoin, int zPosition) {
		mOutputs.add(zPosition, zCoin);
	}
	
	public boolean isEmpty() {
		return mInputs.size() == 0 && mOutputs.size() == 0;
	}
	
	public ArrayList<Coin> getAllInputs(){
		return mInputs;
	}
	
	public ArrayList<Coin> getAllOutputs(){
		return mOutputs;
	}
	
	public MiniNumber sumInputs() {
		MiniNumber tot = MiniNumber.ZERO;
		for(Coin cc : mInputs) {
			tot = tot.add(cc.mAmount);
		}
		return tot;
	}
	
	public MiniNumber sumInputs(MiniData zTokenID) {
		MiniNumber tot = MiniNumber.ZERO;
		for(Coin cc : mInputs) {
			if(cc.getTokenID().isEqual(zTokenID)) {
				tot = tot.add(cc.mAmount);	
			}
		}
		return tot;
	}
	
	public MiniNumber sumOutputs() {
		MiniNumber tot = MiniNumber.ZERO;
		for(Coin cc : mOutputs) {
			tot = tot.add(cc.mAmount);
		}
		return tot;
	}
	
	public MiniNumber sumOutputs(MiniData zTokenID) {
		MiniNumber tot = MiniNumber.ZERO;
		for(Coin cc : mOutputs) {
			if(cc.getTokenID().isEqual(zTokenID)) {
				tot = tot.add(cc.mAmount);	
			}
		}
		return tot;
	}
	
	/**
	 * Get the Remainder Output Coin for a specific token..
	 */
	public Coin getRemainderCoin(MiniData zTokenID) {
		for(Coin cc : mOutputs) {
			if(cc.isRemainder() && cc.getTokenID().isEqual(zTokenID)) {
				return cc;
			}
		}
		return null;
	}
	
	/**
	 * You only need to check that there are enough Inputs for the Outputs.
	 * The rest is BURN..
	 * @return
	 */
	public boolean checkValidInOutPerToken(){
		//First get a list of all the Ouput tokens..
		ArrayList<String> tokens = new ArrayList<>();
		for(Coin cc : mOutputs) {
			MiniData tokenhash = cc.getTokenID();
			if(tokenhash.isEqual(Coin.TOKENID_CREATE)){
				tokenhash = Coin.MINIMA_TOKENID;
			}
			
			String tok = tokenhash.to0xString();
			if(!tokens.contains(tok)) {
				tokens.add(tok);	
			}
		}
		
		//Now get all the Output Amounts...
		Hashtable<String, MiniNumber> outamounts = new Hashtable<>();
		for(String token : tokens) {
			outamounts.put(token, sumOutputs(new MiniData(token)));
		}
		
		//Now cycle through and check there is enough inputs..
		Enumeration<String> keys = outamounts.keys();
		while(keys.hasMoreElements()) {
			//The token
			String tok = keys.nextElement();
			
			//The output total amount
			MiniNumber outamt = outamounts.get(tok);
			
			//The input total amount
			MiniNumber inamt = sumInputs(new MiniData(tok));
			
			//Do the check..
			if(inamt.isLess(outamt)) {
				return false;	
			}
		}
		
		return true;
	}
	
	/**
	 * Set a state value from 0-255 to a certain value
	 * @param zStateNum
	 * @param zValue
	 */
	public void addStateVariable(StateVariable zValue) {
		//If it exists overwrite it..
		StateVariable sv = getStateValue(zValue.getPort());
		if(sv != null) {
			sv.resetData(zValue.getData());
		}else {
			mState.add(zValue);
		}
		
		//Order the state
		Collections.sort(mState,new Comparator<StateVariable>() {
			@Override
			public int compare(StateVariable o1, StateVariable o2) {
				int s1 = o1.getPort();
				int s2 = o2.getPort();
				return Integer.compare(s1, s2);
			}
		});
	}
	
	/**
	 * @param zStateNum
	 * @return
	 */
	public StateVariable getStateValue(int zStateNum) {
		for(StateVariable sv : mState) {
			if(sv.getPort() == zStateNum){
				return sv;
			}
		}
		
		return null;
	}
	
	/**
	 * Check exists
	 * @param zStateNum
	 * @return
	 */
	public boolean stateExists(int zStateNum) {
		for(StateVariable sv : mState) {
			if(sv.getPort() == zStateNum){
				return true;
			}
		}
		
		return false;
	}
	
	/**
	 * Clear all the state values
	 */
	public void clearState() {
		mState.clear();
	}
	
	/**
	 * Required to cycle..
	 * @return
	 */
	public ArrayList<StateVariable> getCompleteState(){
		return mState;
	}
	
	/**
	 * The Link Hash - for the Burn Transaction
	 */
	public MiniData getLinkHash() {
		return mLinkHash;
	}
	
	/**
	 * Token Generation
	 */
	public void setTokenGenerationDetails(TokenProof zTokenDetails) {
		mTokenGenDetails = zTokenDetails;
	}
	
	public TokenProof getTokenGenerationDetails() {
		return mTokenGenDetails;
	}
	
	@Override
	public String toString() {
		return toJSON().toString();
	}
	
	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		//Inputs
		JSONArray ins = new JSONArray();
		for(Coin in : mInputs) {
			ins.add(in.toJSON());
		}
		ret.put("inputs", ins);
		
		//Outputs
		JSONArray outs = new JSONArray();
		for(Coin out : mOutputs) {
			outs.add(out.toJSON());
		}
		ret.put("outputs", outs);
		
		//State
		outs = new JSONArray();
		for(StateVariable sv : mState) {
			outs.add(sv.toJSON());
		}
		ret.put("state", outs);
		
		//Token Generation..
		if(mTokenGenDetails != null) {
			ret.put("tokengen", mTokenGenDetails.toJSON());
		}
		
		ret.put("linkhash", mLinkHash.to0xString());
		
		return ret;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Max 255 inputs or outputs
		MiniByte ins = new MiniByte(mInputs.size());
		ins.writeDataStream(zOut);
		for(Coin coin : mInputs) {
			coin.writeDataStream(zOut);
		}
		
		//Max 255 inputs or outputs
		MiniByte outs = new MiniByte(mOutputs.size());
		outs.writeDataStream(zOut);
		for(Coin coin : mOutputs) {
			coin.writeDataStream(zOut);
		}
		
		//How many state variables..
		int len = mState.size();
		zOut.writeInt(len);
		for(StateVariable sv : mState) {
			sv.writeDataStream(zOut);
		}
		
		//Token generation
		if(mTokenGenDetails == null) {
			MiniByte.FALSE.writeDataStream(zOut);
		}else {
			MiniByte.TRUE.writeDataStream(zOut);
			mTokenGenDetails.writeDataStream(zOut);
		}
	
		//The Link Hash
		mLinkHash.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mInputs  = new ArrayList<>();
		mOutputs = new ArrayList<>();
		mState 	 = new  ArrayList<>();
		
		//Inputs
		MiniByte ins = MiniByte.ReadFromStream(zIn);
		
		int len = ins.getValue();
		for(int i=0;i<len;i++) {
			Coin coin = Coin.ReadFromStream(zIn);
			mInputs.add(coin);
		}
		
		//Outputs
		MiniByte outs = MiniByte.ReadFromStream(zIn);
		len = outs.getValue();
		for(int i=0;i<len;i++) {
			Coin coin = Coin.ReadFromStream(zIn);
			mOutputs.add(coin);
		}
		
		//State Variables
		len = zIn.readInt();
		for(int i=0;i<len;i++){
			StateVariable sv = StateVariable.ReadFromStream(zIn);
			mState.add(sv);
		}
		
		//Token generation
		MiniByte tokgen = MiniByte.ReadFromStream(zIn);
		if(tokgen.isTrue()) {
			mTokenGenDetails = TokenProof.ReadFromStream(zIn);
		}else {
			mTokenGenDetails = null;
		}
		
		mLinkHash = MiniData.ReadFromStream(zIn);
	}
	
	/**
	 * Get a DEEP copy of this transaction
	 */
	public Transaction deepCopy() {
		try {
			//First write transaction out to a byte array
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			writeDataStream(dos);
			dos.flush();
			dos.close();
			
			//Now read it into a new transaction..
			byte[] transbytes = baos.toByteArray();
			ByteArrayInputStream bais = new ByteArrayInputStream(transbytes);
			DataInputStream dis = new DataInputStream(bais);
			
			Transaction deepcopy = new Transaction();
			deepcopy.readDataStream(dis);
			
			dis.close();
			
			return deepcopy;

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
	}
}
