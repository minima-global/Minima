package org.minima.system.input.functions;

import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.utils.messages.Message;

public class MapNetwork extends CommandFunction {

    public MapNetwork() {
        super("MapNetwork");

        setHelp("", "Get a Map of up to 46k peer to peer nodes on network", "");
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
        return new MapNetwork();
    }
}
