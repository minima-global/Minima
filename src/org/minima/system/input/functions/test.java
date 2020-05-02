package org.minima.system.input.functions;

import org.minima.system.brains.TxPoWMiner;
import org.minima.system.input.CommandFunction;

public class test extends CommandFunction{

	public test() {
		super("test");
		setHelp("", "Test the hashrate of the machine you are using..", "Test the hashrate of the machine you are using..");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		getMainHandler().getMiner().PostMessage(TxPoWMiner.TXMINER_TESTHASHING);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new test();
	}
}
