package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class coins extends CommandFunction {

//	public coins() {
//		super("coins");
//		setHelp("(address) (spent|unspent|all)", "Return all your tracked coins ( can specify a specific address ) and spent type. Defaults to unspent.", "");
//	}
//	
//	@Override
//	public void doFunction(String[] zInput) throws Exception {
//		//Get the current balance of the user for all tokens..
//		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_COINS);
//				
//		if(zInput.length>1) {
//			if(zInput.length>2) {
//				//Order matters
//				msg.addString("address", zInput[1]);
//				msg.addString("type", zInput[2]);
//			}else {
//				if(zInput[1].startsWith("0x") || zInput[1].startsWith("Mx")) {
//					//It's an address
//					msg.addString("address", zInput[1]);
//				}else {
//					//It's a spend type
//					msg.addString("type", zInput[1]);
//				}
//			}
//		}
//			
//		//Post It..
//		getMainHandler().getConsensusHandler().PostMessage(msg);
//	}
	
	public coins() {
		super("coins");
		
		setHelp("(relevant) (address:address) (amount:amount) (tokenid:tokenid) (type:spent|unspent|all)", 
				"Search coin database. Defaults to Unspent Minima coins.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		int len = zInput.length;
		if(len == 1) {
			getResponseStream().endStatus(false, "MUST specify some criteria for search..");
			return;
		}
		
		//The extra data
		boolean relevant      = false;
		String address        = "";
		String amount         = "";
		String tokenid        = "";
		String type           = "";
		
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
			}
		}
		
		//Create the message
		Message sender = getResponseMessage(ConsensusPrint.CONSENSUS_TXPOWSEARCH);
		sender.addBoolean("relevant", relevant);
		sender.addString("address", address);
		sender.addString("amount", amount);
		sender.addString("tokenid", tokenid);
		sender.addString("type", type);
		
		
		
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_COINS);
				
//		if(zInput.length>1) {
//			if(zInput.length>2) {
//				//Order matters
//				msg.addString("address", zInput[1]);
//				msg.addString("type", zInput[2]);
//			}else {
//				if(zInput[1].startsWith("0x") || zInput[1].startsWith("Mx")) {
//					//It's an address
//					msg.addString("address", zInput[1]);
//				}else {
//					//It's a spend type
//					msg.addString("type", zInput[1]);
//				}
//			}
//		}
			
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new coins();
	}
}