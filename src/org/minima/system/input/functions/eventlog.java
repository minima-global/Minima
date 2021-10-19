package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.p2p.P2PMessageProcessor;
import org.minima.utils.messages.Message;

public class eventlog extends CommandFunction {

    public eventlog() {
        super("eventlog.json");
    }

    @Override
    public void doFunction(String[] zInput) throws Exception {

        String traceId1 = zInput[1];
        Message msg = getResponseMessage(P2PMessageProcessor.P2P_EVENT_LOG);
        if (!"*".equals(traceId1)) {
            msg.addString("traceId1", traceId1);
        }

        getMainHandler().getNetworkHandler().getP2PMessageProcessor().PostMessage(msg);
    }

    @Override
    public CommandFunction getNewFunction() {
        return new eventlog();
    }
}
