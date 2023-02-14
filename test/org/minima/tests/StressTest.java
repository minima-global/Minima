package org.minima.tests;

import com.sun.net.httpserver.HttpServer;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class StressTest {

    public static final String HOOK = "/hook/";
    public static final int HOOK_PORT = 7999;
    static final int CLIENT_NODE_COUNT = 20;
    static final int SPLIT_COUNT = 20;
    static final int M_1 = 1_000_000;
    static final long B_1 = 1_000_000_000;
    static ConcurrentHashMap<String, MinimaProcessNode> nodes = new ConcurrentHashMap<>();

    static void log(String log) {
        MinimaLogger.log(Thread.currentThread() + " " + log);
    }

    static void sendCoin(MinimaProcessNode senderNode, Collection<String> addresses, int splitCount, int amount) throws IOException, ParseException {
        if (addresses.isEmpty()) return;
        String command = "send multi:[" +
                addresses.stream().map(address -> "\"" + address + ":" + amount + "\"").collect(Collectors.joining(", ")) +
                "]" +
                " debug:true" +
                " split:" +
                splitCount;

        senderNode.doRPC(command);
    }

    static void waitForBalance(long expectAtLeast, Collection<MinimaProcessNode> nodes) throws InterruptedException {
        while (true) {
            int correct = 0;
            for (var node : nodes) {
                long balance = node.getBalance();
                if (balance >= expectAtLeast) {
                    correct++;
                }
            }
            Thread.sleep(Duration.ofSeconds(2).toMillis());
            if (correct == nodes.size()) {
                break;
            }
        }
    }

    static void printBalances() {
        new Thread(() -> {
            while (true) {
                try {
                    Thread.sleep(Duration.ofSeconds(5).toMillis());
                    final String balance = nodes
                            .values()
                            .stream()
                            .map(e -> e.getAddress() + " bal:"+e.getBalance() + " coins:" + e.getCoins() + " block:" + e.getBlock() + " mem:" + e.getMempool())
                            .collect(Collectors.joining(";"));
                    log("status: " + balance);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
    }

    static void splitCoins(MinimaProcessNode node) throws IOException, ParseException, InterruptedException {
        log("split node's coins, 1M X 20: " + node.getAddress());
        sendCoin(node, List.of(node.getAddress()), SPLIT_COUNT, M_1);
        // it takes a while for send request to register
        Thread.sleep(Duration.ofSeconds(1).toMillis());
        waitForBalance(M_1, List.of(node));
    }

    public static void main(String[] args) throws Exception {
        startHookServer();

//        final MinimaProcessNode genesisNode = new MinimaProcessNode(45627);
        final MinimaProcessNode genesisNode = new MinimaProcessNode(MinimaProcessNode.Mode.GENESIS);
//        genesisNode.enableLogs();

        log("wait for genesis coins to be mined");
        waitForBalance(B_1, List.of(genesisNode));
        splitCoins(genesisNode);

        startClientNodes();
        log("started, genesis: [" + genesisNode.getAddress() + "]: " + nodes.keySet().size() + " clients: " + nodes.keySet());

        nodes.put(genesisNode.getAddress(), genesisNode);
        printBalances(); // periodically, forever
        Runtime.getRuntime().addShutdownHook(new Thread(StressTest::stopClientNodes));

        log("send 1_M to each node from genesis");
        sendCoin(genesisNode, nodes.keySet(), 1, M_1);
        Thread.sleep(Duration.ofSeconds(2).toMillis());
        waitForBalance(M_1, nodes.values());
        log("done sending 1_M to each node from genesis");
        splitAllNodesCoins();

        // send from genesis
        new Thread(() -> {
            for (int i1 = 1; i1 <= M_1; i1++) {
                try {
                    log("send from genesis to each node; round: " + i1);
                    sendCoin(genesisNode, nodes.keySet(), 1, 1);
                    Thread.sleep(Duration.ofSeconds(2).toMillis());
                    waitForBalance(1, nodes.values());
                    log("done sending to each node from genesis; round: " + i1);
                } catch (IOException | ParseException | InterruptedException e1) {
                    throw new RuntimeException(e1);
                }
            }
        }).start();
        // also send from random node
        new Thread(() -> {
            final Random random = new Random();
            for (int i = 1; i <= M_1; i++) {
                try {
                    final MinimaProcessNode sender = nodes.values().stream().skip(random.nextInt(nodes.size())).findFirst().get();
                    log("send from: " + sender.getAddress() + " to each node; round: " + i);
                    sendCoin(sender, nodes.keySet(), 1, 1);
                    Thread.sleep(Duration.ofSeconds(2).toMillis());
                    waitForBalance(1, nodes.values());
                    log("done send from: " + sender.getAddress() + " to each node; round: " + i);
                } catch (IOException | ParseException | InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();
        // also send from every node
        new Thread(() -> {
            for (int i = 1; i <= M_1; i++) {
                nodes.values().forEach(node -> {
                    try {
                        sendCoin(node, nodes.keySet(), 1, 1);
                        Thread.sleep(Duration.ofMillis(5).toMillis());
                    } catch (IOException | ParseException | InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                });
            }
        }).start();

        // wait for exit
        Scanner s = new Scanner(System.in);
        do {
            var text = s.next();
            if (text.startsWith(":q")) {
                System.exit(-1);
            }
        } while (true);
    }

    private static void splitAllNodesCoins() {
        final ExecutorService es = Executors.newCachedThreadPool();
        for (MinimaProcessNode node : nodes.values()) {
            es.execute(() -> {
                try {
                    splitCoins(node);
                } catch (IOException | ParseException | InterruptedException e) {
                    throw new RuntimeException(e);
                }
            });
        }
        es.shutdown();

        try {
            es.awaitTermination(1, TimeUnit.MINUTES);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    private static void stopClientNodes() {
        final ExecutorService es = Executors.newCachedThreadPool();
        for (MinimaProcessNode node : nodes.values()) {
            es.execute(node::stopNode);
        }
        es.shutdown();

        try {
            es.awaitTermination(1, TimeUnit.MINUTES);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    private static void startClientNodes() throws InterruptedException {
        final ExecutorService es = Executors.newCachedThreadPool();

        for (int i = 0; i < CLIENT_NODE_COUNT; i++) {
            es.execute(() -> {
                try {
                    final MinimaProcessNode node = new MinimaProcessNode(MinimaProcessNode.Mode.CLIENT);
                    nodes.put(node.getAddress(), node);
                    log("Node: " + nodes.size() + " address: " + node.getAddress());
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            });
        }
        es.shutdown();
        es.awaitTermination(2, TimeUnit.MINUTES);
    }

    private static HttpServer startHookServer() throws IOException {
        final HttpServer server = HttpServer.create(new InetSocketAddress(HOOK_PORT), 0);
        server.createContext(HOOK, httpExchange -> {
            InputStream requestBody = httpExchange.getRequestBody();
            String path = httpExchange.getRequestURI().getPath();
            try (
                    InputStreamReader isr = new InputStreamReader(requestBody, "utf-8");
                    BufferedReader br = new BufferedReader(isr)
            ) {
                final JSONObject body = (JSONObject) new JSONParser().parse(br.lines().collect(Collectors.joining()));
                nodes
                        .get(path.substring(HOOK.length()))
                        .handleHookEvent(body.getString("event", "missing"), body);
            } catch (ParseException e) {
                throw new RuntimeException(e);
            }
            httpExchange.sendResponseHeaders(200, 0);
            httpExchange.getResponseBody().close();
        });
        server.setExecutor(null); // creates a default executor
        server.start();
        log("stared webhook on :" + HOOK_PORT);
        return server;
    }

}
