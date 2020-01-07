package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.input.functions.transfer.exportkey;
import org.minima.system.input.functions.transfer.importkey;
import org.minima.system.input.functions.txns.txncreate;
import org.minima.system.input.functions.txns.txndelete;
import org.minima.system.input.functions.txns.txninput;
import org.minima.system.input.functions.txns.txnlist;
import org.minima.system.input.functions.txns.txnoutput;
import org.minima.system.input.functions.txns.txnpost;
import org.minima.system.input.functions.txns.txnsign;
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
					getResponseStream().getDataJSON().put("description", found.getSimple());
				}else {
					getResponseStream().getDataJSON().put("description", desc);	
				}
				
				getResponseStream().endStatus(true, "");
			}
			
		}else {
			addJSONDesc(new help());
			addJSONDesc(new tutorial());
			
			addJSONDesc(new status());
			addJSONDesc(new printchain());

			//			addJSONDesc(new trace());
			addJSONDesc(new minetrans());
			addJSONDesc(new backup());
			
			addJSONDesc(new connect());
			addJSONDesc(new disconnect());
			addJSONDesc(new reconnect());
			addJSONDesc(new weblink());
			
			addJSONDesc(new gimme50());
			addJSONDesc(new send());
			addJSONDesc(new balance());
			addJSONDesc(new coins());
			addJSONDesc(new keys());
			
			addJSONDesc(new txpowinfo());
			
			addJSONDesc(new createtoken());
			addJSONDesc(new newaddress());
			addJSONDesc(new newscript());
			addJSONDesc(new runscript());
			addJSONDesc(new exportkey());
			addJSONDesc(new importkey());
			
			addJSONDesc(new txnlist());
			addJSONDesc(new txncreate());
			addJSONDesc(new txndelete());
			addJSONDesc(new txninput());
			addJSONDesc(new txnoutput());
			addJSONDesc(new txnsign());
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
		
		//Auto fill
		if(params.equals("")) {
			getResponseStream().getDataJSON().put(zFunc.getName(), zFunc.getSimple());
		}else {
			getResponseStream().getDataJSON().put(zFunc.getName(), params+ " - " + zFunc.getSimple());
		}
		
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new help();
	}
}
