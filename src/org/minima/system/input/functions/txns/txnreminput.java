package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnreminput extends CommandFunction {

	public txnreminput() {
		super("txnreminput");
		setHelp("[id] [position]", "Remove the transaction input at specified position", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Which transaction
		int txn = Integer.parseInt(zInput[1]);
		int pos = Integer.parseInt(zInput[2]);
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_REMINPUT);
		msg.addInt("transaction", txn);
		msg.addInt("position", pos);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnreminput();
	}
}
