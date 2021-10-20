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
public class P2PMsgSwapLink implements Streamable, Traceable {

    private MiniData secret = MiniData.getRandomData(8);
    private InetSocketAddress swapTarget;
    private boolean isSwapClientReq = false;
    private boolean isConditionalSwapReq = false;

    public P2PMsgSwapLink(){
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

    public static P2PMsgSwapLink ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgSwapLink data = new P2PMsgSwapLink();
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
        map.put("targetAddress", from(swapTarget));
        map.put("isSwapClientReq", isSwapClientReq);
        map.put("isConditionalSwapReq", isConditionalSwapReq);
        return new JSONObject(map);
    }
}
