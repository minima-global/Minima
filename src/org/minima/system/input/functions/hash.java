package org.minima.system.input.functions;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class hash extends CommandFunction {

	public hash() {
		super("hash");
		setHelp("[bitlength] [data]", "Hash some hex data or a string", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//get the Stream
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_HASH);
	
		int bitlength = Integer.parseInt(zInput[1]); 
		String data = zInput[2];
		
		msg.addInteger("bitlength", bitlength);
		msg.addString("data", data);
		
		//Must be run from the threadsafe Consensus thread - to access the database
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new hash();
	}
}
