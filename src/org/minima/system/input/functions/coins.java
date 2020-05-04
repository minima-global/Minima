package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class coins extends CommandFunction {
	
	public coins() {
		super("coins");
		
		setHelp("(relevant) (address:address) (amount:amount) (tokenid:tokenid) (type:spent|unspent|all)", 
				"Search coin database. Defaults to all unspent coins.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		int len = zInput.length;
//		if(len == 1) {
//			getResponseStream().endStatus(false, "MUST specify some criteria for search..");
//			return;
//		}
		
		//The extra data
		boolean relevant      = false;
		
		String address        = "";
		String amount         = "";
		String tokenid        = "";
		String type           = "unspent";
		
		//Cycle through..
		for(int i=1;i<len;i++) {
			String param = zInput[i];
			
			if(param.startsWith("relevant")) {
				relevant=true;
			
			}else if(param.startsWith("address:")) {
				address = param.substring(8);
			
			}else if(param.startsWith("amount:")) {
				amount = param.substring(7);
			
			}else if(param.startsWith("tokenid:")) {
				tokenid = param.substring(8);
			
			}else if(param.startsWith("type:")) {
				type = param.substring(5);
			
			}else {
				getResponseStream().endStatus(false, "UNKNOWN parameter : "+zInput[i]);		
				return;
			}
		}
		
		//Create the message
		Message sender = getResponseMessage(ConsensusPrint.CONSENSUS_COINS);
		sender.addBoolean("relevant", relevant);
		sender.addString("address",   address);
		sender.addString("amount",    amount);
		sender.addString("tokenid",   tokenid);
		sender.addString("type",      type);
		
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new coins();
	}
}