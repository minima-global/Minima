package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.utils.messages.Message;

public class eventlog extends CommandFunction {

    public eventlog() {
        super("eventlog");
    }

    @Override
    public void doFunction(String[] zInput) throws Exception {

        String category = zInput[1];
        String traceId = zInput[2];
        Message msg = getResponseMessage(P2PMessageProcessor.P2P_EVENT_LOG);
        if (!"*".equals(traceId)) {
            msg.addString("traceId", traceId);
        }
        if (!"*".equals(category)) {
            msg.addString("category", category);
        }

        getMainHandler().getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
    }

    @Override
    public CommandFunction getNewFunction() {
        return new eventlog();
    }
}
