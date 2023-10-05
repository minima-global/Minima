package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class mmrproof extends Command {

	public class mmrleafnode{
		
		public int mEntry;
		public String mData;
		public MiniData mHash;
		
		public mmrleafnode() {}
	}
	
	public mmrproof() {
		super("mmrproof","[data:] [proof:] [root:] - Check an MMR proof");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmmrproof\n"
				+ "\n"
				+ "Check an MMR Proof.\n"
				+ "\n"
				+ "Can be used to check MMR Proof of coins, scripts or a custom MMR tree created with the 'mmrcreate' command.\n"
				+ "\n"
				+ "Returns true if the proof is valid or false if not.\n"
				+ "\n"
				+ "data:\n"
				+ "    String/HEX data of an MMR leaf node.\n"
				+ "\n"
				+ "proof:\n"
				+ "    The MMR proof of the data from the 'mmrcreate' command.\n"
				+ "\n"
				+ "root:\n"
				+ "    The root hash of the MMR tree from the 'mmrcreate' command.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "mmrproof data:0xCD34.. proof:0xFED5.. root:0xDAE6..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"data","proof","root"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		if(!existsParam("root") || !existsParam("proof") || !existsParam("data")) {
			throw new CommandException("MUST Specify data, root and proof");
		}
		
		//Get the details
		String checkdata 	= getParam("data");
		MiniNumber mnum = MiniNumber.ZERO;
		String strdata 	= checkdata;
		int index = checkdata.indexOf(":");
		if(index!=-1) {
			strdata = checkdata.substring(0, index).trim();
			mnum	= new MiniNumber(checkdata.substring(index+1).trim());
		}
		
		String fullrootstr 	= getParam("root");
		MiniNumber rootnum = MiniNumber.ZERO;
		String rootstr 	= fullrootstr;
		index = fullrootstr.indexOf(":");
		if(index!=-1) {
			rootstr = fullrootstr.substring(0, index).trim();
			rootnum	= new MiniNumber(fullrootstr.substring(index+1).trim());
		}
		
		String proofstr = getParam("proof");
		
		//Is it HEX
		MMRData mmrdata = null;
		if(strdata.startsWith("0x")) {
			mmrdata = MMRData.CreateMMRDataLeafNode(new MiniData(strdata), mnum);
		}else {
			mmrdata = MMRData.CreateMMRDataLeafNode(new MiniString(strdata), mnum);
		}
		
		//Create the MMRdata
		MMRData root 	= new MMRData( new MiniData(rootstr), rootnum);
		MiniData proof 	= new MiniData(proofstr);
		
		//Create an MMR Proof..
		MMRProof prf = MMRProof.convertMiniDataVersion(proof);
		
		//And calculate the final root value..
		MMRData prfcalc = prf.calculateProof(mmrdata);
		
		JSONObject resp = new JSONObject();
		resp.put("input", strdata);
		resp.put("leaf", mmrdata.toJSON());
		resp.put("finaldata", prfcalc.toJSON());
		resp.put("valid", prfcalc.isEqual(root));
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new mmrproof();
	}

}
