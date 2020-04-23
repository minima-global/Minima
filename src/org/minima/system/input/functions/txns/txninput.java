package org.minima.system.input.functions.txns;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txninput extends CommandFunction {

	public txninput() {
		super("txninput");
		setHelp("[id] [coinid] (position)", "Add a specific Coin as an input to the specified transaction. Can specify position - usually to add at 0.", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the Txn
		int txn = Integer.parseInt(zInput[1]);
		
		//Get the coinid of the input
		String coinid = zInput[2];
		MiniData cid = new MiniData(coinid);
		
		//Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNINPUT);
		msg.addInt("transaction", txn);
		msg.addObject("coinid", cid);
		
		if(zInput.length>3) {
			int position = Integer.parseInt(zInput[3]); 
			msg.addInt("position", position);
		}
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txninput();
	}
}
