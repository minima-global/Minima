package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnscript extends CommandFunction {

	public txnscript() {
		super("txnscript");
		setHelp("[id] [script] [proof]", "Add a script to the transaction (MAST) with proof", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNSCRIPT);
		msg.addInt("transaction", Integer.parseInt(zInput[1]));
		msg.addString("script", zInput[2]);
		msg.addString("proof", zInput[3]);
				
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnscript();
	}
}
