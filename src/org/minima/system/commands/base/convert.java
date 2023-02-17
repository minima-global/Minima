package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRData;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class convert extends Command {

	public convert() {
		super("convert","[from:] [to:] [data:] - Convert between different data types");
	}
	
	@Override
	public String getFullHelp() {
		return "\ncoincheck\n"
				+ "\n"
				+ "Check a coin exists and is valid. Can only check unspent coins.\n"
				+ "\n"
				+ "Returns the coin details and whether the MMR proof is valid.\n"
				+ "\n"
				+ "data:\n"
				+ "    The data of a coin. Can be found using the 'coinexport' command.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "coincheck data:0x00000..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"from","to","data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String from = getParam("from").toLowerCase();
		String to 	= getParam("to").toLowerCase();
		String data = getParam("data");
		
		JSONObject resp = new JSONObject();
		
		//First get the initial..
		MiniData fromdata = null; 
		if(from.equals("hex")) {
			fromdata = new MiniData(data);
		}else if(from.equals("mx")) {
			fromdata = Address.convertMinimaAddress(data);
		}else if(from.equals("string")) {
			fromdata = new MiniData(new MiniString(data).getData());
		}else if(from.equals("base64")) {
			fromdata = new MiniData(Base64.getDecoder().decode(data));
		}else {
			throw new CommandException("Invalid FROM type : "+from);
		}
		
		String tomx = null;
		if(to.equals("hex")) {
			tomx = fromdata.to0xString();
		}else if(to.equals("mx")) {
			tomx = Address.makeMinimaAddress(fromdata);
		}else if(to.equals("string")) {
			tomx = new MiniString(fromdata.getBytes()).toString();
		}else if(to.equals("base64")) {
			tomx = Base64.getEncoder().encodeToString(fromdata.getBytes());
		}else {
			throw new CommandException("Invalid TO type : "+to);
		}
		
		//Add to response
		resp.put("conversion", tomx);
		ret.put("response", resp);
				
		return ret;
	}

	@Override
	public Command getFunction() {
		return new convert();
	}

}
