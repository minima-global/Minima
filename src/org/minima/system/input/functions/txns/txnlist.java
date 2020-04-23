package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnlist extends CommandFunction {

	public txnlist() {
		super("txnlist");
		setHelp("(id)", "List all custom transactions or just the selected id", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		Message list = getResponseMessage(ConsensusTxn.CONSENSUS_TXNLIST);
		if(zInput.length>1) {
			int txn = Integer.parseInt(zInput[1]);
			list.addInt("transaction",txn);
		}
		
		getMainHandler().getConsensusHandler().PostMessage(list);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnlist();
	}
}
