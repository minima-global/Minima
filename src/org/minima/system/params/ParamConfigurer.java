package org.minima.system.params;

import org.minima.system.network.p2p.P2PFunctions;
import org.minima.system.network.p2p.params.P2PParams;

import static java.nio.file.Files.lines;
import static java.util.Arrays.stream;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toMap;
import static org.minima.system.params.ParamConfigurer.ParamKeys.toParamKey;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiConsumer;

public class ParamConfigurer {

    private final Map<ParamKeys, String> paramKeysToArg = new HashMap<>();
    private boolean daemon = false;
    private boolean rpcenable = false;
    private boolean mShutdownhook = true;
    
    public ParamConfigurer usingConfFile(String[] programArgs) {
        List<String> zArgsList = Arrays.asList(programArgs);
        int confArgKeyIndex = zArgsList.indexOf("-" + ParamKeys.conf);

        if (confArgKeyIndex > -1) {
            final String confFileArgValue = zArgsList.get(confArgKeyIndex + 1);
            File confFile = new File(confFileArgValue);

            try {
                paramKeysToArg.putAll(lines(confFile.toPath())
                        .map(line -> {
                            final String[] split = line.split("=");
                            return new Pair(split[0], split.length > 1 ? split[1] : "");
                        })
                        .map(pair -> toParamKey((String) pair.left)
                                .map(paramKey -> new Pair(paramKey, pair.right)))
                        .filter(Optional::isPresent)
                        .map(Optional::get)
                        .collect(toMap(
                                pair -> (ParamKeys) pair.left,
                                pair -> (String) pair.right)));
            } catch (IOException exception) {
                System.out.println("Unable to read conf file.");
                System.exit(1);
            }
        }
        return this;
    }

    public ParamConfigurer usingEnvVariables(Map<String, String> envVariableMap) {
        paramKeysToArg.putAll(envVariableMap.entrySet().stream()
                .filter(e -> e.getKey().toLowerCase().startsWith("minima_"))
                .collect(toMap(
                        entry -> entry.getKey().toLowerCase().replaceFirst("minima_", ""),
                        entry -> ofNullable(entry.getValue())
                                .map(String::toLowerCase)
                                .orElse("")))
                .entrySet().stream()
                .filter(e -> toParamKey(e.getKey()).isPresent())
                .collect(toMap(entry -> toParamKey(entry.getKey()).get(), Map.Entry::getValue)));

        return this;
    }

    public ParamConfigurer usingProgramArgs(String[] programArgs) {

        int arglen = programArgs.length;
        int index = 0;
            while (index < arglen) {
                String arg = programArgs[index];
                final int imuCounter = index;
                progArgsToParamKey(arg)
                        .ifPresent(paramKey -> paramKeysToArg.put(paramKey,
                                lookAheadToNonParamKeyArg(programArgs, imuCounter).orElse("true")));
                index++;
            }
        return this;
    }

    public ParamConfigurer configure() {
        paramKeysToArg.forEach((key, value) -> key.consumer.accept(value, this));
        return this;
    }

    private static Optional<String> lookAheadToNonParamKeyArg(String[] programArgs, int currentIndex) {
        if (currentIndex + 1 <= programArgs.length - 1 ) {
            if (!progArgsToParamKey(programArgs[currentIndex + 1]).isPresent()) {
                return ofNullable(programArgs[currentIndex + 1]);
            }
        }
        return empty();
    }

    private static Optional<ParamKeys> progArgsToParamKey(String str) {
        if (str.startsWith("-")) {
            return of(ParamKeys.toParamKey(str.replaceFirst("-", ""))
                    .orElseThrow(() -> new UnknownArgumentException(str)));
        }
        return empty();
    }

    public boolean isDaemon() {
        return daemon;
    }

    public boolean isRpcenable() {
        return rpcenable;
    }
    
    public boolean isShutDownHook() {
        return mShutdownhook;
    }

    enum ParamKeys {
    	data("data", "Specify the data folder (defaults to .minima/ under user home", (args, configurer) -> {
        	GeneralParams.DATA_FOLDER = new File(args).getAbsolutePath();
        }),
    	host("host", "Specify the host IP", (arg, configurer) -> {
            GeneralParams.MINIMA_HOST = arg;
            GeneralParams.IS_HOST_SET = true;
        }),
        port("port", "Specify the Minima port", (arg, configurer) -> {
            GeneralParams.MINIMA_PORT = Integer.parseInt(arg);
        }),
        rpc("rpc", "Specify the RPC port", (arg, configurer) -> {
            GeneralParams.RPC_PORT = Integer.parseInt(arg);
        }),
        rpcenable("rpcenable", "Enable rpc", (args, configurer) -> {
            if ("true".equals(args)) {
                configurer.rpcenable = true;
            }
        }),
        conf("conf", "Specify a configuration file (absolute)", (args, configurer) -> {
            // do nothing
        }),
        daemon("daemon", "Run in daemon mode with no stdin input ( services )", (args, configurer) -> {
            if ("true".equals(args)) {
                configurer.daemon = true;
            }
        }),
//        private1("private", "Use a private network", (args, configurer) -> {
//            if ("true".equals(args)) {
//                GeneralParams.PRIVATE_NETWORK = true;
//            }
//        }),
        isclient("isclient", "Tells the P2P System that this node can't accept incoming connections", (args, configurer) -> {
            if ("true".equals(args)) {
                GeneralParams.IS_ACCEPTING_IN_LINKS = false;
            }
        }),
        mobile("mobile", "Sets this device to a mobile device - used for metrics only", (args, configurer) -> {
            if ("true".equals(args)) {
                GeneralParams.IS_MOBILE = true;
            }
        }),
        nop2p("nop2p", "Disable the automatic P2P system", (args, configurer) -> {
            GeneralParams.P2P_ENABLED = false;
        }),
        noshutdownhook("noshutdownhook", "Do not use the shutdown hook (Android)", (args, configurer) -> {
        	configurer.mShutdownhook = false;
        }),
        noconnect("noconnect", "Stops the P2P system from connecting to other nodes until it's been connected too", (args, configurer) -> {
            if ("true".equals(args)) {
                GeneralParams.NOCONNECT = true;
            }
        }),
        p2pnode("p2pnode", "Specify the initial P2P host:port list to connect to", (args, configurer) -> {
            GeneralParams.P2P_ROOTNODE = args;
        }),
        p2ploglevelinfo("p2p-log-level-info", "Set the P2P log level to info", (args, configurer) -> {
            P2PParams.LOG_LEVEL = P2PFunctions.Level.INFO;
        }),
        p2plogleveldebug("p2p-log-level-debug", "Set the P2P log level to info", (args, configurer) -> {
            P2PParams.LOG_LEVEL = P2PFunctions.Level.DEBUG;
        }),
//        automine("automine", "Simulate user traffic to construct the blockchain", (args, configurer) -> {
//            if ("true".equals(args)) {
//                GeneralParams.AUTOMINE = true;
//            }
//        }),
//        noautomine("noautomine", "Do not simulate user traffic to construct the blockchain", (args, configurer) -> {
//            GeneralParams.AUTOMINE = false;
//        }),
        connect("connect", "Disable the p2p and manually connect to this list of host:port", (args, configurer) -> {
            GeneralParams.P2P_ENABLED = false;
            GeneralParams.CONNECT_LIST = args;
        }),
        clean("clean", "Wipe data folder at startup", (args, configurer) -> {
            if ("true".equals(args)) {
                GeneralParams.CLEAN = true;
            }
        }),
        genesis("genesis", "Create a genesis block, -clean and -automine", (args, configurer) -> {
            if ("true".equals(args)) {
                GeneralParams.CLEAN = true;
//                GeneralParams.PRIVATE_NETWORK = true;
                GeneralParams.GENESIS = true;
//                GeneralParams.AUTOMINE = true;
            }
        }),
        test("test", "Use test params on a private network", (args, configurer) -> {
            if ("true".equals(args)) {
                GeneralParams.TEST_PARAMS 		= true;
//                GeneralParams.PRIVATE_NETWORK 	= true;
//                GeneralParams.P2P_ENABLED 		= false;
                TestParams.setTestParams();
            }
        }),
        help("help", "Print this help", (args, configurer) -> {
            System.out.println("Minima Help");
            stream(values())
                    .forEach(pk -> System.out.format("%-20s%-15s%n", new Object[] {"-" + pk.key,pk.helpMsg}));
            System.exit(1);
        });

        private final String key;
        private String helpMsg;
        private final BiConsumer<String, ParamConfigurer> consumer;

        ParamKeys(String key, String helpMsg, BiConsumer<String, ParamConfigurer> consumer) {
            this.key = key;
            this.helpMsg = helpMsg;
            this.consumer = consumer;
        }

        static Optional<ParamKeys> toParamKey(String str) {
           return stream(ParamKeys.values())
                   .filter(pk -> pk.key.equals(str))
                   .findFirst();
        }
    }

    public static class UnknownArgumentException extends RuntimeException {
        public UnknownArgumentException(String arg) {
            super("Unknown argument : " + arg);
        }
    }

    static class Pair<L, R> {
        L left;
        R right;

        public Pair(L left, R right) {
            this.left = left;
            this.right = right;
        }
    }
}