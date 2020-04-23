package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.input.functions.transfer.exportcoin;
import org.minima.system.input.functions.transfer.exportkey;
import org.minima.system.input.functions.transfer.importcoin;
import org.minima.system.input.functions.transfer.importkey;
import org.minima.system.input.functions.txns.txnauto;
import org.minima.system.input.functions.txns.txncreate;
import org.minima.system.input.functions.txns.txndelete;
import org.minima.system.input.functions.txns.txnexport;
import org.minima.system.input.functions.txns.txnimport;
import org.minima.system.input.functions.txns.txninput;
import org.minima.system.input.functions.txns.txnlist;
import org.minima.system.input.functions.txns.txnoutput;
import org.minima.system.input.functions.txns.txnpost;
import org.minima.system.input.functions.txns.txnreminput;
import org.minima.system.input.functions.txns.txnremoutput;
import org.minima.system.input.functions.txns.txnscript;
import org.minima.system.input.functions.txns.txnsign;
import org.minima.system.input.functions.txns.txnsignauto;
import org.minima.system.input.functions.txns.txnstate;
import org.minima.system.input.functions.txns.txnvalidate;

public class help extends CommandFunction{

	public help() {
		super("help");
		
		setHelp("{function}",
				"Show the help for all or a specified function", 
				"If you are unsure how to use a function you can type help function, "
				+ "and a longer description with a complete org.minima.datadog.example will be shown.\n\nhelp send");
	}
	
	@Override
	public void doFunction(String[] zInput) {
		//Print all or a specific help..
		if(zInput.length>1) {
			//Get the function
			String func = zInput[1];
			
			//Get the function
			CommandFunction found = CommandFunction.getFunction(func);
			
			//Do it..
			if(found==null) {
				getResponseStream().endStatus(false, "Function not found : "+func);
			} else {
				String desc = found.getDescription().trim();
				if(desc.equals("")) {
					getResponseStream().getDataJSON().put("description", found.getParams()+" - "+found.getSimple());
				}else {
					getResponseStream().getDataJSON().put("description", found.getParams()+" - "+desc);	
				}
				
				getResponseStream().endStatus(true, "");
			}
			
		}else {
			addJSONDesc(new help());
			addJSONDesc(new tutorial());
			
			addJSONDesc(new status());
			addJSONDesc(new history());
			addJSONDesc(new backup());
			addJSONDesc(new flushmempool());
			addJSONDesc(new check());
			
//			addJSONDesc(new printchain());
			addJSONDesc(new printtree());
			addJSONDesc(new automine());
			addJSONDesc(new trace());
			
			addJSONDesc(new network());
			addJSONDesc(new connect());
			addJSONDesc(new disconnect());
			addJSONDesc(new reconnect());
			addJSONDesc(new weblink());
			
			addJSONDesc(new gimme50());
			addJSONDesc(new send());
			addJSONDesc(new newaddress());
			addJSONDesc(new balance());
			
			addJSONDesc(new keys());
			addJSONDesc(new exportkey());
			addJSONDesc(new importkey());
			
			addJSONDesc(new coins());
			addJSONDesc(new coinsimple());
			addJSONDesc(new exportcoin());
			addJSONDesc(new importcoin());
			addJSONDesc(new keepcoin());
			addJSONDesc(new unkeepcoin());
			
			addJSONDesc(new search());
			addJSONDesc(new txpowsearch());
			addJSONDesc(new txpowinfo());
			
			addJSONDesc(new scripts());
			addJSONDesc(new newscript());
			addJSONDesc(new extrascript());
			addJSONDesc(new cleanscript());
			addJSONDesc(new runscript());
			
			addJSONDesc(new createtoken());
			addJSONDesc(new tokens());
			
			addJSONDesc(new sign());
			addJSONDesc(new chainsha());
			addJSONDesc(new random());
			
			addJSONDesc(new txnlist());
			addJSONDesc(new txncreate());
			addJSONDesc(new txndelete());
			addJSONDesc(new txnexport());
			addJSONDesc(new txnimport());
			addJSONDesc(new txninput());
			addJSONDesc(new txnoutput());
			addJSONDesc(new txnreminput());
			addJSONDesc(new txnremoutput());
			addJSONDesc(new txnstate());
			addJSONDesc(new txnscript());
			addJSONDesc(new txnsign());
			addJSONDesc(new txnauto());
			addJSONDesc(new txnsignauto());
			addJSONDesc(new txnvalidate());
			addJSONDesc(new txnpost());
			
			addJSONDesc(new quit());
			
			//It's worked
			getResponseStream().endStatus(true, "");
		}
	}

	public void addJSONDesc(CommandFunction zFunc) {
		//Need to HACK swap {} for () .. JSON..
		String params = zFunc.getParams().replaceAll("\\{", "\\(").replaceAll("\\}", "\\)").trim();
		
		//The Name.. same length for better reading
		String name = getStrOfLength(14, zFunc.getName());
		
		//Auto fill
		if(params.equals("")) {
			getResponseStream().getDataJSON().put(name, zFunc.getSimple());
		}else {
			getResponseStream().getDataJSON().put(name, params+ " - " + zFunc.getSimple());
		}
	}
	
	public void addBlankLine() {
		getResponseStream().getDataJSON().put("---", "");
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
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new help();
	}
}
