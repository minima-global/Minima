package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class history extends CommandFunction {

	public history() {
		super("history");
		
		setHelp("(clear)", "Return a list of relevant past transactions or clear them all", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_HISTORY);
		
		if(zInput.length>1) {
			if(zInput[1].equalsIgnoreCase("clear") ) {
				msg.addBoolean("clear", true);
			}
		}
		
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new history();
	}
}
