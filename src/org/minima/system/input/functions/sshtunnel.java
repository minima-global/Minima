package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.maxima.Maxima;
import org.minima.system.network.sshtunnel.SSHTunnel;
import org.minima.utils.messages.Message;

public class sshtunnel extends CommandFunction{

	public sshtunnel() {
		super("sshtunnel");
		
		setHelp("[start|stop|info|params]", "Create an ssh tunnel to an ssh server for an external Maxima IP", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message ssh = null;
		
		//The details
		String func = zInput[1];
		
		//Is this more than info or new
		if(func.equals("start")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_START);
			
		}else if(func.equals("stop")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_STOP);
			
		}else if(func.equals("info")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_INFO);
			
		}else if(func.equals("params")) {
			
		}
		
		//Send it to the miner..
		getMainHandler().getNetworkHandler().getSSHTunnel().PostMessage(ssh);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new sshtunnel();
	}
}
