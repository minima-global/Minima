package org.minima.system.network.p2p.functions;


import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.minima.system.network.base.MinimaClient;
import org.minima.system.network.p2p.P2PState;
import org.minima.system.network.p2p.messages.*;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.MinimaLogger;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j(topic = "P2P")
public class StartupFuncs {

    private static final String IPV4_PATTERN =
            "^(([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\\.(?!$)|$)){4}$";
    private static final Pattern pattern = Pattern.compile(IPV4_PATTERN);

    public static boolean isValidIPv4(final String email) {
        Matcher matcher = pattern.matcher(email);
        return matcher.matches();
    }

    public static ArrayList<InetSocketAddress> LoadExtraPublicMinimaServers(String[] defaultNodeList) {
        // Moved from Start
        String[] returnList = {};
        /**
         * Load the EXTRA public Minima servers for 0.98 - 0.99 has different P2P
         */
        try {
            //Get a list of the EXTRA Minima clients..
            String extraclients = RPCClient.sendGET("http://34.118.59.216/pubextra.txt");

            //Now chop them up..
            ArrayList<String> pubextra = new ArrayList<>();
            StringTokenizer strtok = new StringTokenizer(extraclients, ",");
            while (strtok.hasMoreElements()) {
                String ip = strtok.nextToken().trim();
                if (isValidIPv4(ip)) {
                    pubextra.add(ip);
                } else {
                    MinimaLogger.log("Invalid Extra Minima Peer : " + ip);
                }
            }

            //Now add all these nodes..
            String[] newnodes = new String[defaultNodeList.length + pubextra.size()];
            for (int i = 0; i < defaultNodeList.length; i++) {
                newnodes[i] = defaultNodeList[i];
            }

            for (int i = 0; i < pubextra.size(); i++) {
                newnodes[defaultNodeList.length + i] = pubextra.get(i);
            }

            //Re-assign!
            returnList = newnodes;

        } catch (Exception e1) {
            MinimaLogger.log(e1);

            //RESET
            returnList = defaultNodeList;

            MinimaLogger.log("Minima Bootstrap nodes reset : " + Arrays.toString(returnList));
        }
        return ConvertToInetSocketAddresses(returnList);
    }

    public static ArrayList<InetSocketAddress> ConvertToInetSocketAddresses(String[] hosts) {
        ArrayList<InetSocketAddress> addresses = new ArrayList<>();
        for (String host : hosts) {
            for (int i = 0; i < 3; i++) { // 9001, 10001, 11001
                try {
                    addresses.add(new InetSocketAddress(InetAddress.getByName(host), 9001 + (1000 * i)));
                } catch (UnknownHostException e) {
                    MinimaLogger.log("Unknown host: " + host);
                }
            }
        }
        return addresses;
    }

    public static void SaveNodeList(P2PState state) {
        final ObjectMapper mapper = new ObjectMapper();
        try {
            if (!state.getP2pDataFile().exists()) {
                state.getP2pDataFile().createNewFile();
            }
            FileOutputStream fos = new FileOutputStream(state.getP2pDataFile());
            final ByteArrayOutputStream out = new ByteArrayOutputStream();

            mapper.writeValue(out, GenRendezvousNodeList(state, 100));
            out.writeTo(fos);
            fos.close();
        } catch (IOException ioe) {
            log.error("Failed to write data to file: ", ioe);
        }
    }

    public static ArrayList<InetSocketAddress> LoadNodeList(P2PState state, String[] defaultNodeList, boolean noExtraHosts) {
        // Try and load node list from the saved data
        ArrayList<InetSocketAddress> loadedNodeList = new ArrayList<>();
        if (state.getP2pDataFile().exists()) {
            try {
                FileInputStream inputStream = new FileInputStream(state.getP2pDataFile());
                final ObjectMapper mapper = new ObjectMapper();
                loadedNodeList = mapper.readValue(inputStream, new TypeReference<ArrayList<InetSocketAddress>>() {
                });
                state.addRecentJoiners(loadedNodeList);
            } catch (IOException ioe) {
                log.error("Error whilst reading in p2pDataFile: ", ioe);
            }
        } else {
            if (noExtraHosts) {
                loadedNodeList = ConvertToInetSocketAddresses(defaultNodeList);
            } else {
                loadedNodeList = LoadExtraPublicMinimaServers(defaultNodeList);
            }
        }
        return loadedNodeList;
    }


    /**
     * PRECONDITION: Arrays A and B are provided
     *
     * @param state
     * @return
     */
    public static ArrayList<InetSocketAddress> GenRendezvousNodeList(P2PState state, int numAddrToReturn) {
        assert state.getOutLinksCopy() != null : "OutLinks Array is null";
        assert state.getRecentJoinersCopy() != null : "InLinks Array is null";
        return Stream.of(state.getRecentJoinersCopy(), state.getOutLinksCopy())
                .flatMap(Collection::stream).distinct().limit(numAddrToReturn).collect(Collectors.toCollection(ArrayList::new));
    }

    public static void processOnRendezvousMsg(P2PState state, P2PMsgRendezvous rendezvous, MinimaClient client) {

        rendezvous.getAddresses().forEach(state::addRandomNodeSet);
        state.setAddress(rendezvous.getTargetAddress());
        state.rendezvousComplete();
    }

}
