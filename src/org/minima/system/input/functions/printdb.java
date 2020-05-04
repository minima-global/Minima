package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class printdb extends CommandFunction{

	public printdb() {
		super("printdb");
		setHelp("(coins) (txpow) (mmr) (tree)", "Print out sections of the database. Useful DEBUG function. TREE ONLY works on command line.. :(", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		boolean coins  = false;
		boolean txpow  = false;
		boolean mmr    = false;
		boolean tree   = false;
		
		int len = zInput.length;
		
		if(len==1) {
			getResponseStream().endStatus(true, "Must choose at least one table");		
			return;
			
		}else {
			for(int i=1;i<len;i++) {
				if(zInput[i].equals("coins")) {
					coins = true;
				}else if(zInput[i].equals("txpow")) {
					txpow = true;
				}else if(zInput[i].equals("mmr")) {
					mmr = true;
				}else if(zInput[i].equals("tree")) {
					tree = true;
				}
			}
		}
		
		//Print the DB
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_PRINTCHAIN);
		msg.addBoolean("coins", coins);
		msg.addBoolean("txpow", txpow);
		msg.addBoolean("mmr", mmr);
		msg.addBoolean("tree", tree);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new printdb();
	}
}
