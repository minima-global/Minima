package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class runscript extends CommandFunction{

	public runscript() {
		super("runscript");
		
		setHelp("[script] {sigs}", "Run the specified script with the specified signatures.","");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//get the Contract
		String script = zInput[1];
		String sigs   = "";
		
		if(zInput.length>2) {
			sigs   = zInput[2];
		}
		
		//Create a message
		Message rscript = new Message(ConsensusUser.CONSENSUS_RUNSCRIPT);
		rscript.addString("script", script);
		rscript.addString("sigs", sigs);
		
		getMainHandler().getConsensusHandler().PostMessage(rscript);
	}
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new runscript();
	}
}
