package org.minima.system.input.functions;

import org.minima.system.brains.SendManager;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class sendpoll extends CommandFunction{

	public sendpoll() {
		super("sendpoll");
		
		setHelp("(list|clear (refid)) [amount] [address] {tokenid}", "Polling version of send", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		String amount  = zInput[1];
		
		//Is it a list or a clear
		if(amount.equals("list")) {
			getMainHandler().getSendManaManager().PostMessage(getResponseMessage(SendManager.SENDMANAGER_LIST));
			return;
		}else if(amount.equals("clear")) {
			String ref = "all";
			if(zInput.length>2) {
				ref = zInput[2];
			}
		
			Message clearer = getResponseMessage(SendManager.SENDMANAGER_CLEAR);
			clearer.addString("reference", ref);
			
			getMainHandler().getSendManaManager().PostMessage(clearer);
			return;
		}

		//It's a normal send..
		String address = zInput[2];
		String tokenid = "0x00";
		
		if(zInput.length>3) { 
			tokenid = zInput[3];
		}
		
		//Create a message
		Message sender = getResponseMessage(SendManager.SENDMANAGER_ADD);
		sender.addString("address", address);
		sender.addString("amount", amount);
		sender.addString("tokenid", tokenid);
		
		//Send it to the miner..
		getMainHandler().getSendManaManager().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new sendpoll();
	}

}
