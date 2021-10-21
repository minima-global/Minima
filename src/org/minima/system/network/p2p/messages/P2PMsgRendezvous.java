package org.minima.system.network.p2p.messages;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.minima.objects.base.MiniData;
import org.minima.system.network.p2p.Traceable;
import org.minima.system.network.p2p.event.EventPublisher;
import org.minima.system.network.p2p.util.JSONObjectUtils;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import static java.util.stream.Collectors.toList;
import static lombok.AccessLevel.PRIVATE;
import static org.minima.system.network.p2p.util.JSONObjectUtils.from;

@NoArgsConstructor(access = PRIVATE)
@Getter
@Setter(PRIVATE)
public class P2PMsgRendezvous implements Streamable, Traceable {

    private MiniData traceId;
    private MiniData secret = MiniData.getRandomData(8);
    private ArrayList<InetSocketAddress> addresses;
    InetSocketAddress targetAddress;


    public P2PMsgRendezvous(ArrayList<InetSocketAddress> addresses, InetSocketAddress targetAddress, Traceable traceable) {
        this.addresses = addresses;
        this.targetAddress = targetAddress;
        traceId = new MiniData(traceable.getTraceId());
    }

    @Override
    public void writeDataStream(DataOutputStream zOut) throws IOException {
        traceId.writeDataStream(zOut);
        secret.writeDataStream(zOut);
        InetSocketAddressIO.writeAddress(targetAddress, zOut);
        InetSocketAddressIO.writeAddressList(this.addresses, zOut);
        EventPublisher.publishWrittenStream(this);
    }

    @Override
    public void readDataStream(DataInputStream zIn) throws IOException {
        setTraceId(MiniData.ReadFromStream(zIn));
        setSecret(MiniData.ReadFromStream(zIn));
        this.setTargetAddress(InetSocketAddressIO.readAddress(zIn));
        this.setAddresses(InetSocketAddressIO.readAddressList(zIn));
        EventPublisher.publishReadStream(this);
    }

    public static P2PMsgRendezvous ReadFromStream(DataInputStream zIn) throws IOException {
        P2PMsgRendezvous rendezvous = new P2PMsgRendezvous();
        rendezvous.readDataStream(zIn);
        return rendezvous;
    }

    @Override
    public String getTraceId() {
        return traceId.toString();
    }

    @Override
    public JSONObject getContent() {

        Map<String, Object> map = new HashMap<>();
        map.put("targetAddress", from(targetAddress));
        map.put("addresses", new JSONArray(addresses.stream()
                .map(JSONObjectUtils::from)
                .collect(toList())));
        return new JSONObject(map);
    }
}
