package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class printtree extends CommandFunction{

	public printtree() {
		super("printtree");
		setHelp("", "Print a tree version of the current chain", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_PRINTCHAIN_TREE);
		
		if(zInput.length>1) {
			if(zInput[1].equals("on")) {
				msg.addBoolean("auto", true);
			}else{
				msg.addBoolean("auto", false);
			}
		}else {
			msg.addBoolean("auto", false);
		}
		
		//Print the Tree..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new printtree();
	}
}
