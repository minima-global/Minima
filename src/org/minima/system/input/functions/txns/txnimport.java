package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnimport extends CommandFunction{

	public txnimport() {
		super("txnimport");
		
		setHelp("[id] [data]", "Import a transasction. Can then sign, edit and post it.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Which transaction
		int id = Integer.parseInt(zInput[1]);
		String data = zInput[2];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNIMPORT);
		msg.addInt("transaction", id);
		msg.addString("data", data);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnimport();
	}
}
