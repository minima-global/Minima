package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.Setter;
import org.minima.objects.base.MiniData;
import org.minima.system.network.p2p.Traceable;
import org.minima.system.network.p2p.event.EventPublisher;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;

import static org.minima.system.network.p2p.util.JSONObjectUtils.from;

@Getter
@Setter
public class P2PMsgDoSwap implements Streamable, Traceable {

    private MiniData secret = MiniData.getRandomData(8);
    private InetSocketAddress swapTarget;

    public P2PMsgDoSwap() {
        if (EventPublisher.threadTraceId.get() == null) {
            EventPublisher.threadTraceId.set(getTraceId());
        }
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        secret.writeDataStream(zOut);
        InetSocketAddressIO.writeAddress(swapTarget, zOut);
        EventPublisher.publishWrittenStream(this);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        setSecret(MiniData.ReadFromStream(zIn));
        setSwapTarget(InetSocketAddressIO.readAddress(zIn));
        EventPublisher.publishReadStream(this);
    }

    public static P2PMsgDoSwap ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgDoSwap data = new P2PMsgDoSwap();
        data.readDataStream(zIn);
        return data;

    }

    @Override
    public String getTraceId() {
        return secret.to0xString();
    }

    @Override
    public JSONObject getContent() {

        Map<String, Object> map = new HashMap<>();
        map.put("swapTarget", from(swapTarget));
        return new JSONObject(map);
    }
}
