package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniNumber;
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
	 * The Inputs that make up the Transaction
	 */
	ArrayList<Coin> mInputs  = new ArrayList<>();
	
	/**
	 * All the Outputs
	 */
	ArrayList<Coin> mOutputs = new ArrayList<>();
	
	/**
	 * The State values of the Transaction
	 */
	ArrayList<StateVariable> mState = new ArrayList<>();
	
	/**
	 * Constructor
	 */
	public Transaction() {}
	
	public void addInput(Coin zCoin) {
		mInputs.add(zCoin);
	}
	
	public boolean isEmpty() {
		return mInputs.size() == 0 && mOutputs.size() == 0;
	}
	
	public void addOutput(Coin zCoin) {
		mOutputs.add(zCoin);
	}
	
	public ArrayList<Coin> getAllInputs(){
		return mInputs;
	}
	
	public ArrayList<Coin> getAllOutputs(){
		return mOutputs;
	}
	
	public MiniNumber sumInputs() {
		MiniNumber tot = new MiniNumber();
		for(Coin cc : mInputs) {
			tot = tot.add(cc.mAmount);
		}
		return tot;
	}
	
	public MiniNumber sumOutputs() {
		MiniNumber tot = new MiniNumber();
		for(Coin cc : mOutputs) {
			tot = tot.add(cc.mAmount);
		}
		return tot;
	}

	/**
	 * Set a state value from 0-255 to a certain value
	 * @param zStateNum
	 * @param zValue
	 */
	public void addStateVariable(StateVariable zValue) {
		mState.add(zValue);
	}
	
	/**
	 * @param zStateNum
	 * @return
	 */
	public StateVariable getStateValue(MiniNumber zStateNum) {
		for(StateVariable sv : mState) {
			if(sv.getPort().isEqual(zStateNum)){
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
	public boolean stateExists(MiniNumber zStateNum) {
		for(StateVariable sv : mState) {
			if(sv.getPort().isEqual(zStateNum)){
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
		
		return ret;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Max 255 inputs or outputs
		MiniByte ins = new MiniByte(mInputs.size());

		//Write it out..
		ins.writeDataStream(zOut);
		
		//Now the coins
		for(Coin coin : mInputs) {
			coin.writeDataStream(zOut);
		}
		
		//Max 255 inputs or outputs
		MiniByte outs = new MiniByte(mOutputs.size());

		//Write it out..
		outs.writeDataStream(zOut);
		
		//Now the coins
		for(Coin coin : mOutputs) {
			coin.writeDataStream(zOut);
		}
		
		//How many state variables..
		int len = mState.size();
		zOut.writeInt(len);
		
		//Now the state
		for(StateVariable sv : mState) {
			sv.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mInputs  = new ArrayList<>();
		mOutputs = new ArrayList<>();
		mState 	 = new  ArrayList<>();
		
		//Inputs
		MiniByte ins = new MiniByte();
		ins.readDataStream(zIn);
		
		int len = ins.getValue();
		for(int i=0;i<len;i++) {
			Coin coin = Coin.ReadFromStream(zIn);
			mInputs.add(coin);
		}
		
		//Outputs
		MiniByte outs = new MiniByte();
		outs.readDataStream(zIn);
		
		len = outs.getValue();
		for(int i=0;i<len;i++) {
			Coin coin = Coin.ReadFromStream(zIn);
			mOutputs.add(coin);
		}
		
		//State Variables
		int sl = zIn.readInt();
		for(int i=0;i<sl;i++){
			StateVariable sv = StateVariable.ReadFromStream(zIn);
			
			//Add it..
			mState.add(sv);
		}
	}
}
