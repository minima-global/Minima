package org.minima.system.commands.all;

import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class help extends Command {

	public help() {
		super("help","Show Help. () are optional. [] are required. Chain multiple commands with ;");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		JSONObject details = new JSONObject();
		
		addCommand(details, new help());
		
		addCommand(details, new status());
		addCommand(details, new printtree());
		addCommand(details, new trace());
		addCommand(details, new automine());
		addCommand(details, new hashtest());
//		addCommand(details, new debugflag());
		
		addCommand(details, new txpow());
		addCommand(details, new coins());
		
		addCommand(details, new newaddress());
		addCommand(details, new send());
		addCommand(details, new balance());
		
		addCommand(details, new tokens());
		addCommand(details, new tokencreate());
		
		addCommand(details, new runscript());
		addCommand(details, new tutorial());
		
		addCommand(details, new mmrcreate());
		addCommand(details, new mmrproof());
		
		addCommand(details, new network());
		addCommand(details, new message());
		addCommand(details, new connect());
		addCommand(details, new disconnect());
		addCommand(details, new rpc());
		addCommand(details, new webhooks());
		
		addCommand(details, new backup());
		addCommand(details, new restore());
		
		addCommand(details, new incentivecash());
		
		addCommand(details, new quit());
		
		ret.put("response", details);
		
		return ret;
	}

	private void addCommand(JSONObject zDetails, Command zCommand) {
		zDetails.put(getStrOfLength(15,zCommand.getname()), zCommand.getHelp());
	}
	
	public String getStrOfLength(int zDesiredLen, String zString) {
		String ret = new String(zString);
		int len    = ret.length();
		
		//The same or longer
		if(len >= zDesiredLen) {
			return ret.substring(0, zDesiredLen);
		}
		
		//If Shorter add zeros
		for(int i=0;i< zDesiredLen-len;i++) {
			ret = ret.concat(" ");
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new help();
	}

}
