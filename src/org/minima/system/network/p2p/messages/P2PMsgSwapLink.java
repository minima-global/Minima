package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.NoArgsConstructor;
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

import static lombok.AccessLevel.PRIVATE;
import static org.minima.system.network.p2p.util.JSONObjectUtils.from;

// TODO split this class into two. A VO and then a DTO without the booleans being used for conditional logic
@NoArgsConstructor(access = PRIVATE)
@Getter
@Setter(PRIVATE)
public class P2PMsgSwapLink implements Streamable, Traceable {

    private MiniData traceId;
    private MiniData secret = MiniData.getRandomData(8);
    private InetSocketAddress swapTarget;
    private boolean isSwapClientReq = false;
    private boolean isConditionalSwapReq = false;

    public P2PMsgSwapLink(MiniData secret,
                          InetSocketAddress swapTarget,
                          boolean isSwapClientReq,
                          boolean isConditionalSwapReq,
                          Traceable traceable) {
        this.secret = secret;
        this.swapTarget = swapTarget;
        this.isSwapClientReq = isSwapClientReq;
        this.isConditionalSwapReq = isConditionalSwapReq;
        traceId = new MiniData(traceable.getTraceId());
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        traceId.writeDataStream(zOut);
        secret.writeDataStream(zOut);
        InetSocketAddressIO.writeAddress(swapTarget, zOut);
        EventPublisher.publishWrittenStream(this);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        setTraceId(MiniData.ReadFromStream(zIn));
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
        return traceId.toString();
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
