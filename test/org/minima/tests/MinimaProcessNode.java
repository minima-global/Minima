package org.minima.tests;

import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.RPCClient;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.json.parser.ParseException;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.URLEncoder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class MinimaProcessNode {

    private final ProcessHandle process;
    private final int port;
    private final Mode mode;
    private String address;
    private long block;
    private long confirmed;
    private long coins;
    private long memPool;
    private long length;

    public MinimaProcessNode(long pid) {
        mode = Mode.EXTERNAL;
        port = 9001;
        process = ProcessHandle.of(pid).get();
        waitForStart();
        address = getAddress();
        log("found: " + process + " on port: " + port);
    }

    public MinimaProcessNode(Mode mode) throws IOException {
        this.mode = mode;

        final String classPath = ManagementFactory.getRuntimeMXBean().getClassPath();
        final List<String> args = new ArrayList<>(List.of(
//                "/usr/bin/open", "-a", "Terminal",
                "java", "-cp", classPath, "org.minima.Minima",
                "-test",
                "-nop2p",
                "-clean",
                "-showparams",
                "-allowallip", "true",
                "-rpcenable")
        );

        switch (mode) {
            case CLIENT:
                port = ThreadLocalRandom.current().nextInt(30000, 32768);
                final Path tempDirectory = Files.createTempDirectory(Path.of(System.getProperty("java.io.tmpdir")), "minima");
                args.addAll(List.of(
                        "-data", tempDirectory.toString(),
                        "-connect", "127.0.0.1:9001",
                        "-port", String.valueOf(port))
                );
                break;
            case GENESIS:
                port = 9001;
                final Path genesisDirectory = Files.createTempDirectory(Path.of(System.getProperty("java.io.tmpdir")), "genesis");
                log("genesis dir: " + genesisDirectory);
                args.addAll(List.of(
                        "-data", genesisDirectory.toString(),
                        "-genesis",
                        "-port", String.valueOf(port))
                );
                break;
            default:
                throw new IllegalArgumentException("cant start an EXTERNAL node");
        }

//        var command = args.subList(0, 3);
//        command.add(String.join(" ", args.subList(3, args.size())));

        final ProcessBuilder pb = new ProcessBuilder();
//        if (mode == Mode.GENESIS) {
        if (false) {
            pb.inheritIO();
        } else {
            pb.redirectOutput(ProcessBuilder.Redirect.DISCARD);
            pb.redirectError(ProcessBuilder.Redirect.DISCARD);
        }
        final Process startedProcess = pb
                .command(args)
                .start();
        log("starting: " + mode + " " + startedProcess + " on port: " + port);

        new Thread(() -> {
            try {
                int exitCode = startedProcess.waitFor();
                log("process: " + startedProcess + " on port: " + port + " exit: " + exitCode);
            } catch (InterruptedException e) {
                log("process: " + startedProcess + " on port: " + port + " Interrupted ");
                stopNode();
                log("process: " + startedProcess + " on port: " + port + " Stopped ");
            }
        }).start();

        process = startedProcess.toHandle();

        new Thread(() -> {
            while(true) {
                try {
                    Thread.sleep(Duration.ofSeconds(2).toMillis());
                    readBalance();
                    readStatus();
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            }
        }).start();

        waitForStart();
        // this order is important, cannot register webhook before address, and cannot get address before start
        getAddress();
//        registerWebHook(address);

        log("started: " + mode + " " + process + " on port: " + port + "@" + address);
    }

    void log(String log) {
        MinimaLogger.log(process + "@" + port + " " + log);
    }

    JSONObject doRPC(String command) throws IOException, ParseException {
        final String get = "http://127.0.0.1:" + (port + 4) + "/" + URLEncoder.encode(command, MiniString.MINIMA_CHARSET);
//        log("RPC [" + get + "]");
        final JSONObject json = (JSONObject) new JSONParser().parse(RPCClient.sendGET(get));
//        log("RPC [" + get + "] response: " + MiniFormat.JSONPretty(json));
        return json;
    }

    String rpcSimple(String command, Object responseField) {
        try {
            JSONObject response = doRPC(command);
            return ((JSONObject) response.get("response")).get(responseField).toString();
        } catch (Exception ex) {
            throw new RuntimeException("error running command: " + ex);
        }
    }

    String getAddress() {
        if (address != null) {
            return address;
        }
        address = rpcSimple("getaddress", "address");
        return address;
    }

    void readBalance() {
        try {
            JSONObject jsonObject = doRPC("balance");
            JSONArray response = (JSONArray) jsonObject.get("response");
            JSONObject innerJsonObj = (JSONObject) response.get(0);
            confirmed = Long.parseLong(innerJsonObj.get("confirmed").toString());
            coins = Long.parseLong(innerJsonObj.get("coins").toString());
        } catch (Exception ex) {
            log("error getting balance: " + ex);
        }
    }

    void readStatus() {
        try {
            JSONObject jsonObject = doRPC("status");
            JSONObject response = (JSONObject) jsonObject.get("response");
            length = Long.parseLong(response.get("length").toString());
            JSONObject chain = (JSONObject) response.get("chain");
            block = Long.parseLong(chain.get("block").toString());
            JSONObject txpow = (JSONObject) response.get("txpow");
            memPool = Long.parseLong(txpow.get("mempool").toString());
        } catch (Exception ex) {
            log("error getting status: " + ex);
        }
    }

    void enableLogs() {
//        rpcSimple("logs mining:true scripts:true", "mining");
//        rpcSimple("debugflag activate:true", "debug");
        rpcSimple("trace enable:true filter:MAIN", "enabled");
    }

    void registerWebHook(String path) {
        rpcSimple("webhooks action:add hook:http://127.0.0.1:7999/hook/" + path, "webhooks");
    }

    public void stopNode() {
        if (mode == Mode.EXTERNAL) return;
        if (process.isAlive()) {
            process.destroy();
            try {
                Thread.sleep(Duration.ofSeconds(1).toMillis());
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
            if (process.isAlive()) {
                process.destroyForcibly();
            }
        }
    }

    void waitForStart() {
        int attempts = 0;
        do {
            attempts++;
            try {
                Thread.sleep(Duration.ofSeconds(5).toMillis());
                if (length == 0) {
                    log("waiting for IBD");
                    continue;
                }
                break;
            } catch (Exception ex) {
                log("start error: " + ex);
            }
        } while (attempts < 20);
        if (attempts == 20) {
            throw new RuntimeException("Could not start: " + process + " on port: " + port);
        }
    }

    void handleHookEvent(String event, JSONObject body) {
//        log("hook: @" + getAddress() + ":" + event);
        switch (event) {
            case "NEWBLOCK":
                JSONObject data = (JSONObject) body.get("data");
                JSONObject txpow = (JSONObject) data.get("txpow");
                JSONObject header = (JSONObject) txpow.get("header");
                setBlock(Long.parseLong(header.getString("block")));
//                log("hook: @" + getAddress() + ":" + event + " body:" + body);
                break;
            case "NEWBALANCE":
//                log("hook: @" + getAddress() + ":" + event + " body:" + body);
                break;
            default:
                break;
        }

    }

    public long getCoins() {
        return coins;
    }

    public long getBlock() {
        return block;
    }

    public void setBlock(long block) {
        this.block = block;
    }

    public long getMempool() {
        return this.memPool;
    }

    long getBalance() {
        return confirmed;
    }

    public enum Mode {EXTERNAL, GENESIS, CLIENT}
}
