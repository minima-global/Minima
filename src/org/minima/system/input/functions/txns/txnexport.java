package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnexport extends CommandFunction{

	public txnexport() {
		super("txnexport");
		setHelp("[id]", "Export a complete transasction. Share it. Can be signed.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Which transaction
		int txn = Integer.parseInt(zInput[1]);
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNEXPORT);
		msg.addInt("transaction", txn);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnexport();
	}
}
