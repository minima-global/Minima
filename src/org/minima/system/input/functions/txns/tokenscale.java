package org.minima.system.input.functions.txns;

import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class tokenscale extends CommandFunction {

	public tokenscale() {
		super("tokenscale");
		setHelp("[tokenID] (token:amount|minima:amount)", "Return the scaled amount of a token amount", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		//What token
		String tokenid = zInput[1];
				
		//How much
		String tokamount = zInput[2];
		String amount 	 = null;
		
		//Is it the token amount or Minima amount
		boolean isMinima = false;
		if(tokamount.startsWith("token:")) {
			isMinima = false;
			amount = tokamount.substring(6); 
		}else if(tokamount.startsWith("minima:")) {
			isMinima = true;
			amount = tokamount.substring(7);
		}else {
			getResponseStream().endStatus(false, "Invalid amount argument "+tokamount);
			return;
		}
		
		// Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNSCALE);
		msg.addBoolean("isminima", isMinima);
		msg.addString("amount", amount);
		msg.addString("tokenid", tokenid);
		
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new tokenscale();
	}
}
