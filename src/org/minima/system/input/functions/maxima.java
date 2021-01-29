package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.maxima.Maxima;
import org.minima.utils.messages.Message;

public class maxima extends CommandFunction{

	public maxima() {
		super("maxima");
		
		setHelp("[info|new|add [pubkey@host]|list|send [pubkey] [message]]", "Maxima functions..", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message max = getResponseMessage(Maxima.MAXIMA_FUNCTION);
				
		//The details
		String func = zInput[1];
		max.addString("function", func);
		
		//Is this more than info or new
		if(func.equals("send")) {
			max.addString("to", zInput[2]);
			max.addString("message", zInput[3]);
		
		}else if(func.equals("receive")) {
			max.addString("message", zInput[2]);
		
		}else if(func.equals("add")) {
			max.addString("maximauser", zInput[2]);
			
		}
		
		//Send it to the miner..
		getMainHandler().getNetworkHandler().getMaxima().PostMessage(max);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new maxima();
	}
}
