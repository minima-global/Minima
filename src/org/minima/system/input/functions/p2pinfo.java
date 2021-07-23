package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.p2p.P2PManager;
import org.minima.utils.messages.Message;

public class p2pinfo extends CommandFunction{

	public p2pinfo() {
		super("p2pinfo");
		setHelp("", "Print P2P Info", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Print the DB
		Message msg = getResponseMessage(P2PManager.P2P_PEERINFO);
		getMainHandler().getNetworkHandler().getP2PManager().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new p2pinfo();
	}
}
