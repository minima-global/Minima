package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.ConnectionDetails;
import org.minima.system.network.p2p.ConnectionReason;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.Traceable;
import org.minima.system.network.p2p.event.EventPublisher;
import org.minima.utils.Streamable;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

@Getter
@Setter
public class P2PMsgGreeting implements Streamable, Traceable {

    private MiniData traceId = MiniData.getRandomData(8);
    int numClientSlotsAvailable;
    int minimaPort;
    boolean isClient;
    ConnectionReason reason = ConnectionReason.NONE;
    MiniData auth_key = new MiniData();


    public P2PMsgGreeting() {
    }

    public P2PMsgGreeting(P2PState state, MinimaClient client){
        numClientSlotsAvailable = state.getNumLinks() * 2 - state.getClientLinks().size();
        minimaPort = state.getAddress().getPort();
        isClient = state.isClient();

        if (!client.isIncoming()){
            // Outgoing connection only
            ConnectionDetails details = state.getConnectionDetailsMap().remove(client.getMinimaAddress());
            if (details != null){
                reason = details.getReason();
                if(state.isClient() && details.getReason() != ConnectionReason.RENDEZVOUS){
                    reason = ConnectionReason.CLIENT;
                }
                if (details.getAuth_key() != null) {
                    auth_key = details.getAuth_key();
                }
            } else {
                reason = ConnectionReason.CLIENT;
            }
        }
    }


    public static P2PMsgGreeting ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgGreeting data = new P2PMsgGreeting();
        data.readDataStream(zIn);
        return data;
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        traceId.writeDataStream(zOut);
        zOut.writeInt(numClientSlotsAvailable);
        zOut.writeInt(minimaPort);
        zOut.writeBoolean(isClient);
        new MiniString(reason.toString()).writeDataStream(zOut);
        auth_key.writeDataStream(zOut);
        EventPublisher.publishWrittenStream(this);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        setTraceId(MiniData.ReadFromStream(zIn));
        this.setNumClientSlotsAvailable(zIn.readInt());
        this.setMinimaPort(zIn.readInt());
        this.setClient(zIn.readBoolean());
        this.setReason(ConnectionReason.valueOf(MiniString.ReadFromStream(zIn).toString()));
        this.setAuth_key(MiniData.ReadFromStream(zIn));
        EventPublisher.publishReadStream(this);
    }

    @Override
    public String getTraceId() {
        return traceId.to0xString();
    }
}
