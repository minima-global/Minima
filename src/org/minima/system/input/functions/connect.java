package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.messages.Message;

public class connect extends CommandFunction{

	public connect() {
		super("connect");
		setHelp("[host] [port]", "Connect to a Minima node", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The port..
		String host = zInput[1];
		int port = Integer.parseInt(zInput[2]);
		
		//Connect to a specific host:port
		Message connect  = getResponseMessage(NetworkHandler.NETWORK_CONNECT);
		connect.addInt("port", port).addString("host", host);
		getMainHandler().getNetworkHandler().PostMessage(connect);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new connect();
	}
}
