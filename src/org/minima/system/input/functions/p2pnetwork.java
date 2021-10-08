package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.utils.messages.Message;

public class p2pnetwork extends CommandFunction {

    public p2pnetwork() {
        super("p2pnetwork");

        setHelp("", "Get a Map of the peer to peer nodes on network", "");
    }

    @Override
    public void doFunction(String[] zInput) throws Exception {
        //Get the current balance of the user for all tokens..
        Message msg = getResponseMessage(P2PMessageProcessor.P2P_PRINT_NETWORK_MAP);

        //Post It..
        getMainHandler().getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
    }

    @Override
    public CommandFunction getNewFunction() {
        // TODO Auto-generated method stub
        return new p2pnetwork();
    }
}
