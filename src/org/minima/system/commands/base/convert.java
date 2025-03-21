package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class convert extends Command {

	public convert() {
		super("convert","[from:] [to:] [data:] - Convert between different data types (String, HEX, Mx, Base64)");
	}
	
	@Override
	public String getFullHelp() {
		return "\nconvert\n"
				+ "\n"
				+ "Convert between different data types\n"
				+ "\n"
				+ "Returns converted data.\n"
				+ "\n"
				+ "from:\n"
				+ "    The type of the data param.\n"
				+ "\n"
				+ "to:\n"
				+ "    The type you want to convert to.\n"
				+ "\n"
				+ "data:\n"
				+ "    The the data to convert.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "convert from:String to:HEX data:hello\n"
				+ "\n"
				+ "convert from:HEX to:Mx data:0XFFFF\n"
				+ "\n"
				+ "convert from:String to:Base64 data:hello\n";
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
		
		String data = null;
		if(isParamJSONObject("data")) {
			data = getJSONObjectParam("data").toString();
		}else if(isParamJSONArray("data")){
			data = getJSONArrayParam("data").toString();
		}else{
			data = getParam("data");
		}
		
		JSONObject resp 	= new JSONObject();
		MiniData fromdata 	= null;
		String todata 		= null;
		
		try {
			
			//First get the initial..
			 
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
			
			if(to.equals("hex")) {
				todata = fromdata.to0xString();
			}else if(to.equals("mx")) {
				todata = Address.makeMinimaAddress(fromdata);
			}else if(to.equals("string")) {
				todata = new MiniString(fromdata.getBytes()).toString();
			}else if(to.equals("base64")) {
				todata = Base64.getEncoder().encodeToString(fromdata.getBytes());
			}else {
				throw new CommandException("Invalid TO type : "+to);
			}
			
		}catch(CommandException cexc) {
			throw cexc;
			
		}catch(Exception exc) {
			throw new CommandException(exc.toString());
		}
		
		//Add to response
		resp.put("conversion", todata);
		ret.put("response", resp);
				
		return ret;
	}

	@Override
	public Command getFunction() {
		return new convert();
	}

}
