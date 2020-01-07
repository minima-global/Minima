package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.messages.Message;

public class disconnect extends CommandFunction{

	public disconnect() {
		super("disconnect");
		setHelp("[UID]", "Disconnect from a network user", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The UID
		String uid = zInput[1];
		
		//Connect to a specific host:port
		Message connect  = new Message(NetworkHandler.NETWORK_DISCONNECT).addString("uid", uid);
		getMainHandler().getNetworkHandler().PostMessage(connect);	
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new disconnect();
	}

}
