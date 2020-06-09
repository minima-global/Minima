package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class minidapps extends CommandFunction {

	public minidapps() {
		super("minidapps");
		
		setHelp("(name)", "Return a list of all the MiniDAPPs currently installed - or search for a specific one", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_MINIDAPPS);
		
		//Can specify to check ONLY a single address..
		if(zInput.length>1) {
			msg.addString("name", zInput[1]);
		}
			
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new minidapps();
	}
}
