package org.minima.system.input.functions.txns;

import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.ConsensusTxn;
import org.minima.system.input.CommandFunction;
import org.minima.utils.messages.Message;

public class txnoutput extends CommandFunction {

	public txnoutput() {
		super("txnoutput");
		setHelp("[id] [amount] [address] {tokenID}", "Add an output to the specified transaction", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {

		int txn = Integer.parseInt(zInput[1]);
		String value = zInput[2];

		//Check value..
		MiniNumber number = new MiniNumber(value);
		if(number.isLess(MiniNumber.ZERO)) {
			getResponseStream().endStatus(false, "Cannot have NEGATIVE outputs..");
			return;
		}
		
		//Check the Address for Minima Address
		String address = zInput[3];
		if(address.startsWith("0x")) {
			//It's a regular HASH address
			address = new MiniData(address).to0xString();
		}else if(address.startsWith("Mx")) {
			//It's a Minima Address!
			address = Address.convertMinimAddress(address).to0xString();
		}
		
		Address addr = new Address(new MiniData(address));

		// is there a TokenID
		String tokenid = "0x00";
		if (zInput.length > 4) {
			tokenid = zInput[4];
		}

		// Send to the consensus Handler
		Message msg = getResponseMessage(ConsensusTxn.CONSENSUS_TXNOUTPUT);
		msg.addInt("transaction", txn);
		msg.addString("value", value);
		msg.addObject("address", addr);
		msg.addString("tokenid", tokenid);

		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new txnoutput();
	}
}
