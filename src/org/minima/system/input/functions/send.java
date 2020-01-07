package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class send extends CommandFunction{

	public send() {
		super("send");
		
		setHelp("[amount] [address] {tokenid}", "Send an amount of Minima to a certain address", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The details
		String amount  = zInput[1];
		String address = zInput[2];
		String tokenid = "0x00";
		
		if(zInput.length>3) { 
			tokenid = zInput[3];
		}
		
		//Create a message
		Message sender = getResponseMessage(ConsensusHandler.CONSENSUS_CREATETRANS);
		sender.addString("address", address);
		sender.addString("amount", amount);
		sender.addString("tokenid", tokenid);
		
		//Send it to the miner..
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new send();
	}

}
