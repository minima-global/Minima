package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

public class reconnect extends CommandFunction{

	public reconnect() {
		super("reconnect");
		setHelp("[on|off|reset]", "Set global reconnect ability on or off. Or reset all connections (disconnect-reconnect)", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//The port..
		String onoff = zInput[1];
		
		if(onoff.equalsIgnoreCase("on")) {
			getMainHandler().getNetworkHandler().setGlobalReconnect(true);	
			
			MinimaLogger.log("Reconnect ENABLED");
		}else if(onoff.equalsIgnoreCase("off")) {
			getMainHandler().getNetworkHandler().setGlobalReconnect(false);
			
			MinimaLogger.log("Reconnect DISABLED");
		
		}else if(onoff.equalsIgnoreCase("reset")) {
			Message reconnect = getResponseMessage(NetworkHandler.NETWORK_RECONNECT);
			getMainHandler().getNetworkHandler().PostMessage(reconnect);
		}
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new reconnect();
	}
}
