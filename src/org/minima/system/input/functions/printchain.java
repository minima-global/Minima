package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;

public class printchain extends CommandFunction{

	public printchain() {
		super("printchain");
		setHelp("{auto|off}", "Show a tree and chain representation of the complete chain", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
	
		//Is AUTO on..
		if(zInput.length>1) {
			if(zInput[1].equals("auto")) {
				getMainHandler().getConsensusHandler().mPrintChain = true;
			}else{
				getMainHandler().getConsensusHandler().mPrintChain = false;
				return;
			}
		}
	
		//Print the Tree..
		getMainHandler().getConsensusHandler().PostMessage(ConsensusPrint.CONSENSUS_PRINTCHAIN);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new printchain();
	}
}
