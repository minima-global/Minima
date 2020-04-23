package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class runscript extends CommandFunction{

	public runscript() {
		super("runscript");
		
		setHelp("[script] {sigs:..} {state:..} {prevstate:..} {globals:..} {outputs:..} (scripts:..)", "",
				"Test a script and give all the relevant data as # seperated : value key pairs..");
		
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//get the Contract
		String script = zInput[1];
		
		//How much extra data
		int len = zInput.length;
		
		//The extra data
		String sigs        = "";
		String state       = "";
		String prevstate   = "";
		String globals     = "";
		String outputs     = "";
		String scripts     = "";
		
		//Cycle through..
		for(int i=2;i<len;i++) {
			String param = zInput[i];
			
			if(param.startsWith("sigs:")) {
				sigs = param.substring(5);
			}else if(param.startsWith("state:")) {
				state = param.substring(6);
			}else if(param.startsWith("prevstate:")) {
				prevstate = param.substring(10);
			}else if(param.startsWith("globals:")) {
				globals = param.substring(8);
			}else if(param.startsWith("outputs:")) {
				outputs = param.substring(8);
			}else if(param.startsWith("scripts:")) {
				scripts = param.substring(8);
			} 	
		}
		
		//Create the message
		Message rscript = getResponseMessage(ConsensusUser.CONSENSUS_RUNSCRIPT);
		rscript.addString("script", script);
		rscript.addString("sigs", sigs);
		rscript.addString("state", state);
		rscript.addString("prevstate", prevstate);
		rscript.addString("globals", globals);
		rscript.addString("outputs", outputs);
		rscript.addString("scripts", scripts);
		
		//Post it..
		getMainHandler().getConsensusHandler().PostMessage(rscript);	
	}
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new runscript();
	}
}
