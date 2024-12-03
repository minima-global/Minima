package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.json.JSONObject;

public class maxmessage extends Command {
	
	public maxmessage() {
		super("maxmessage","Create a 128bit Public and Private RSA key pair. Can use with maxsign and maxverify.");
	}
	
	/*@Override
	public String getFullHelp() {
		return "\nmaxcreate\n"
				+ "\n"
				+ "Create a 128 bit RSA public and private key. You can use them with maxsign and maxverify.\n"
				+ "\n"
				+ "Returns the public amd private key HEX data.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxcreate\n";
	}*/
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","data","signedby","encryptfor","decryptwith"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		JSONObject resp = new JSONObject();
		
		String action = getParam("action");
		
		if(action.equals("create")) {
			
			//Whats the message
			MiniData data = getDataParam("data");
			
			//Who is signing..
			MiniData signedby;
			if(existsParam("signedby")) {
				signedby = getDataParam("signedby");
				
			}else {
				signedby = Main.getInstance().getMaxima().getPrivateKey();
			}
			
			//Who is it encrypted for
			MiniData encrypt = getDataParam("encryptfor");
			
			//Now create a Max Message Object
			
			
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new maxmessage();
	}

}
