package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.Crypto;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class ScriptProof implements Streamable {

	/**
	 * The Script
	 */
	MiniString mScript;
	
	/**
	 * The merkle proof chain -the root is equal to the Address
	 */
	MMRProof mProof;
	
	/**
	 * The Address
	 */
	Address mAddress;
	
	private ScriptProof() {}
	
	public ScriptProof(String zScript) {
	
		//DON'T NEED TO DO THIS FOR 1 SCRIPT BUT SHOWS HOW MULTIPLE SCRIPTS COULD BE ADDED
		
		//Store the script
		mScript = new MiniString(zScript);
		
		//Hash it..
		MiniData hash = Crypto.getInstance().hashObject(mScript);
		
		//Create an MMR proof..
		MMR mmr = new MMR();
		
		//Create a new piece of data to add
		MMRData scriptdata = new MMRData(hash);
		
		//Add to the MMR
		MMREntry entry = mmr.addEntry(scriptdata);
		
		//Get the MMRProof
		mProof = mmr.getProof(entry.getEntryNumber());
		
		//Calculate the root address
		calculateAddress();
	}
	
	public ScriptProof(String zScript, MMRProof zProof) {
	
		//Store the script
		mScript = new MiniString(zScript);
		
		//The proof..
		mProof = zProof;
		
		//Calculate the root address
		calculateAddress();
	}
	
	/**
	 * The final address this represents
	 */
	private void calculateAddress() {
		
		//Hash it..
		MiniData hash = Crypto.getInstance().hashObject(mScript);
		
		//And calulate the finsl root..
		MMRData root = mProof.calculateProof(new MMRData(hash));
				
		//The address is the final hash
		mAddress = new Address(root.getData()); 
	}
	
	public MiniString getScript() {
		return mScript;
	}
	
	public MMRProof getProof() {
		return mProof;
	}
	
	public Address getAddress() {
		return mAddress;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		json.put("script", mScript.toString());
		json.put("address", mAddress.getAddressData().to0xString());
		json.put("proof", mProof.toJSON());
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mScript.writeDataStream(zOut);
		mProof.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mScript = MiniString.ReadFromStream(zIn);
		mProof	= MMRProof.ReadFromStream(zIn);
		
		calculateAddress();
	}
	
	public static ScriptProof ReadFromStream(DataInputStream zIn) throws IOException {
		ScriptProof pscr = new ScriptProof();
		pscr.readDataStream(zIn);
		return pscr;
	}
	
}
