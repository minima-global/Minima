package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnsign extends CommandFunction {

	public txnsign() {
		super("txnsign");
		setHelp("[id] [public key]", "Sign the Transaction with the chosen Public Key", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		int txn 			= Integer.parseInt(zInput[1]);
		String PublicKey   	= zInput[2];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNSIGN);
		msg.addInt("transaction", txn);
		msg.addString("pubkey", PublicKey);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnsign();
	}
}
