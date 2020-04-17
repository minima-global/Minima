package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnauto extends CommandFunction {

	public txnauto() {
		super("txnauto");
		setHelp("[id] [amount] [address] {tokenid}", "Create a complete transaction but don't post it. Like 'send'. Add state variables FIRST and then post.", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the Txn
		int txn = Integer.parseInt(zInput[1]);
		
		//The details
		String amount  = zInput[2];
		String address = zInput[3];
		String tokenid = "0x00";
		
		if(zInput.length>4) { 
			tokenid = zInput[4];
		}
		
		//Send to the consensus Handler
		Message sender = getResponseMessage(ConsensusTxn.CONSENSUS_TXNAUTO);
		sender.addInt("transaction", txn);
		
		sender.addString("amount", amount);
		sender.addString("address", address);
		sender.addString("tokenid", tokenid);
		
		getMainHandler().getConsensusHandler().PostMessage(sender);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnauto();
	}
}
