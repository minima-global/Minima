package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnimport extends CommandFunction{

	public txnimport() {
		super("txnimport");
		
		setHelp("[data]", "Import a transasction. Can then sign, edit and post it.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Which transaction
		String data = zInput[1];
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNIMPORT);
		msg.addString("data", data);
	
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnimport();
	}
}
