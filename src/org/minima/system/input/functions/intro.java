package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.utils.MinimaLogger;

public class intro extends CommandFunction{

	public intro() {
		super("intro");
	}
	
	@Override
	public void doFunction(String[] zInput)  {
		MinimaLogger.log("Welcome to Minima. For assistance type help. Then press enter.");
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new intro();
	}
}
