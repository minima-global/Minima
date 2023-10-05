package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntryNumber;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class mmrcreate extends Command {

	public class mmrleafnode{
		
		public int mEntry;
		public String mInput;
		public String mData;
		public MMRData mLeafData;
		public mmrleafnode() {}
	}
	
	public mmrcreate() {
		super("mmrcreate","[nodes:[]] - Create an MMR Tree of data. Nodes can be STRING / HEX");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmmrcreate\n"
				+ "\n"
				+ "Create an MMR Tree of data. Can be used in MAST contracts.\n"
				+ "\n"
				+ "Must specify a JSON array of string/HEX data for the leaf nodes.\n"
				+ "\n"
				+ "They could be a list of public keys.. and then in a script you can check if the given key is one in the set.\n"
				+ "\n"
				+ "OR you can have different scripts.. and then you can execute any number of scripts from the same UTXO.\n"
				+ "\n"
				+ "Returns the MMR data and proof for each leaf node and the MMR root hash.\n"
				+ "\n"
				+ "nodes:\n"
				+ "    JSON array of string/HEX data for the leaf nodes.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "mmrcreate nodes:[\"RETURN TRUE\",\"RETURN FALSE\"]\n"
				+ "\n"
				+ "mmrcreate nodes:[\"0xFF..\",\"0xEE..\"]\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"nodes"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		JSONArray mmrdata = getJSONArrayParam("nodes");
		
		//Create an MMR
		MMR mmrtree = new MMR();
		
		//Add the Signatures
		int counter=0;
		ArrayList<mmrleafnode> leafnodes = new ArrayList<>();
		for(Object data : mmrdata) {
			
			//The String sig
			String fulldata = (String)data;
			
			//The data and SUM value..
			String strdata 	= null;
			MiniNumber mnum	= MiniNumber.ZERO;
			
			//Is there a sumvalue..
			int index = fulldata.indexOf(":");
			if(index!=-1) {
				strdata = fulldata.substring(0, index);
				mnum	= new MiniNumber(fulldata.substring(index+1));
			}else {
				strdata = fulldata;
			}
			
			MMRData leaf = null;
			
			//Is it HEX
			if(strdata.startsWith("0x")) {
				leaf = MMRData.CreateMMRDataLeafNode(new MiniData(strdata), mnum);
			}else {
				leaf = MMRData.CreateMMRDataLeafNode(new MiniString(strdata), mnum);
			}
						
			//Create leafnode
			mmrleafnode leafnode = new mmrleafnode();
			leafnode.mEntry 	= counter;
			leafnode.mInput 	= strdata;
			leafnode.mLeafData 	= leaf;
			
			//Add them to our list
			leafnodes.add(leafnode);
			
			//Increment
			counter++;
			
			//Add to thew MMR
			mmrtree.addEntry(leaf);
		}
		
		//Get the root..
		MMRData root = mmrtree.getRoot();
		
		//Now create the output
		JSONArray leafdata = new JSONArray();
		for(mmrleafnode leaf : leafnodes) {
			
			JSONObject jobj = new JSONObject();
			jobj.put("entry", leaf.mEntry);
			jobj.put("data", leaf.mInput);
			jobj.put("value", leaf.mLeafData.getValue().toString());
			//jobj.put("leafnode", leaf.mLeafData.toJSON());	
			
			//Get the proof..
			MMRProof proof = mmrtree.getProof(new MMREntryNumber(leaf.mEntry));
			
			//Get as data string
			MiniData dataproof = MiniData.getMiniDataVersion(proof); 
			
			//Add this proof
			jobj.put("proof", dataproof.to0xString());
			
			//Add to the total
			leafdata.add(jobj);
		}
		
		JSONObject jsonmmr = new JSONObject();
		jsonmmr.put("nodes", leafdata);
		jsonmmr.put("total", leafnodes.size());
		jsonmmr.put("root", root.toJSON());
		
		//Add balance..
		ret.put("response", jsonmmr);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new mmrcreate();
	}

}
