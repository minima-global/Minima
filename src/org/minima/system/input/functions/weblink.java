package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.messages.Message;

public class weblink extends CommandFunction{

	public weblink() {
		super("weblink");
		setHelp("[host] [port]", "Connect to a Minima Web Proxy node", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The port..
		String host 	= zInput[1];
		int port 		= Integer.parseInt(zInput[2]);
		String webhost 	= zInput[3];
		
		//Connect to a specific host:port
		Message connect  = getResponseMessage(NetworkHandler.NETWORK_WEBPROXY);
		connect.addInt("port", port).addString("host", host).addString("webhostid", webhost);
		
		//post it..
		getMainHandler().getNetworkHandler().PostMessage(connect);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new weblink();
	}
}
