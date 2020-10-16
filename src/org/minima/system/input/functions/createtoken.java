package org.minima.system.input.functions;

import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class createtoken extends CommandFunction {
	
	public static MiniNumber MAX_COINS = new MiniNumber("1000000000000");
	
	public createtoken() {
		super("createtoken");
		setHelp("[name] [total] (token script)", 
				"Create a token.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Take the Amount..
		String name   = zInput[1];
		String amount = zInput[2];
		String script = "RETURN TRUE";
		
		if(zInput.length>3) {
			script = zInput[3];
		}
		
		//1 Billion MAX for now..
		MiniNumber coins = new MiniNumber(amount);
		if(coins.isMore(MAX_COINS)) {
			getResponseStream().endStatus(false, MAX_COINS+" MAX.. for now..");
			return;
		}
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusHandler.CONSENSUS_CREATETOKEN);
		msg.addString("name", name);
		msg.addString("amount", amount);
		msg.addString("script", script);
		
		//Post it!
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new createtoken();
	}
	
}
