package org.minima.system.input.functions.txns;

import org.minima.objects.base.MiniHash;
import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txninput extends CommandFunction {

	public txninput() {
		super("txninput");
		setHelp("[txn_num] [coinID]", "Add a specific Coin as an input to the specified transaction", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the Txn
		int txn = Integer.parseInt(zInput[1]);
		
		//Get the coinid of the input
		String coinid = zInput[2];
		MiniHash cid = new MiniHash(coinid);
				
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNINPUT);
		msg.addInt("transaction", txn);
		msg.addObject("coinid", cid);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txninput();
	}
}
