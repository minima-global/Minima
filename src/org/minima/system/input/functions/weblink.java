package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.messages.Message;

public class weblink extends CommandFunction{

	public weblink() {
		super("weblink");	
		setHelp("[UUID]", "Connect your phone to a webpage given the UUID", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The port..
		String uuid 	= zInput[1];
		
		//Connect to a specific host:port
		Message connect  = getResponseMessage(NetworkHandler.NETWORK_WEBPROXY);
		connect.addString("uuid", uuid);	
		
		//post it..
		getMainHandler().getNetworkHandler().PostMessage(connect);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new weblink();
	}
}
