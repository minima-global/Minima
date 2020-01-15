package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;

public class quit extends CommandFunction{

	public quit() {
		super("quit");
		setHelp("", "Shutdown the system", "");
	}

	@Override
	public void doFunction(String[] zInput) throws Exception {
		getMainHandler().SystemShutDown();
		
		getResponseStream().endStatus(true, "Minima System shutting down..");
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new quit();
	}
}
