package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;

public class trace extends CommandFunction{

	public trace() {
		super("trace");
		
		setHelp("[on|off]", "Turn ON|OFF full debug info as messages are processed", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//On or Off
		boolean on = zInput[1].equalsIgnoreCase("on");
		
		//Set the trace on for all the handlers
		getMainHandler().setTrace(on);
		
		//It's worked
		getResponseStream().endStatus(true, "Trace : "+on);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new trace();
	}
}
