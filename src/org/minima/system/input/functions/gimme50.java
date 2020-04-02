package org.minima.system.input.functions;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class gimme50 extends CommandFunction {

//	public static final MiniData32 GIMME50_INPUTADDRESS = new MiniData32("FEED50FEED50FEED50FEED50");
	
	/**
	 * TEST Net Input coin that is always accepted
	 */
	public static MiniData COINID_INPUT = new MiniData("0xFEED50FEED50FEED50FEED50");
	
	public gimme50() {
		super("gimme50");
		setHelp("", "Give yourself 50 TestNet Mini!", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//get the Stream
		Message msg = getResponseMessage(ConsensusHandler.CONSENSUS_GIMME50);
			
		//Must be run from the threadsafe Consensus thread - to access the database
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new gimme50();
	}
}
