package org.minima.mcli;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.google.common.collect.ImmutableMap;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;

import java.io.StringWriter;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.text.ParseException;
import java.util.*;

import static com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_COMMENTS;
import static com.fasterxml.jackson.core.JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES;
import static com.fasterxml.jackson.databind.SerializationFeature.INDENT_OUTPUT;
import static java.lang.System.out;
import static java.net.URI.create;
import static java.net.http.HttpClient.newHttpClient;
import static java.util.Optional.empty;
import static java.util.Optional.of;
import static java.util.stream.IntStream.range;
import static org.yaml.snakeyaml.DumperOptions.FlowStyle.BLOCK;
import static org.yaml.snakeyaml.DumperOptions.NonPrintableStyle.ESCAPE;
import static org.yaml.snakeyaml.DumperOptions.ScalarStyle.PLAIN;
import static org.yaml.snakeyaml.util.UriEncoder.encode;

public class App {

    public final static String SYNTAX_HIGH_ESC_START = "\u001B[";
    public static final String SYNTAX_HIGH_CLEAR = "0";
    public static final String SYNTAX_HIGH_BLUE = "34";
    public final static String SYNTAX_HIGH_ESC_END = "m";

    private static final String PRINTTREE_COMMAND = "printtree";

    private static final String RESPONSE_STATUS_KEY = "status";
    private static final String RESPONSE_RESPONSE_KEY = "response";
    private static final String RESPONSE_MESSAGE_KEY = "message";
    private static final String RESPONSE_ERROR_KEY = "error";
    private static final String RESPONSE_STATUS_VALUE = "true";

    private static final Map<String, String> OPTIONS_MAP = ImmutableMap.of(
            "--rpcport", "Connect to a Minima running on given port. Defaults to 9002",
            "--rpchost", "Connect to a Minima running on given host. Defaults to 127.0.0.1",
            "--verbose", "Verbose logging of activity",
            "--help", "Print this help message.");

    public static void main(String[] args) {
        out.println();

        if (args.length == 0) {
            printHelp();
        }

        String rpcHost = "127.0.0.1";
        int rpcPort = 9002;
        int arglen = args.length;
        int index = 0;
        boolean verbose = false;

        while (index < arglen) {
            if ("--rpcport".equals(args[index])) {
                Optional<String> nextArg = lookAheadToNextArg(args, index);
                rpcPort = Integer.parseInt(nextArg.orElseThrow(RuntimeException::new));
                index++;
                index++;
            } else if ("--rpchost".equals(args[index])) {
                Optional<String> nextArg = lookAheadToNextArg(args, index);
                rpcHost = nextArg.orElseThrow(RuntimeException::new);
                index++;
                index++;
            } else if ("--verbose".equals(args[index])) {
                verbose = true;
                index++;
            } else if ("--help".equals(args[index])) {
                printHelp();
            } else {
                command(args, rpcHost, rpcPort, index, verbose);
            }

        }
        printHelp();
    }

    private static void command(String[] args, String rpcHost, int rpcPort, int commandIndex, boolean verbose) {

        final StringBuilder commandBuilder = new StringBuilder("http://" + rpcHost + ":" + rpcPort + "/");

        range(commandIndex, args.length)
                .forEach(argIndex -> {
                    if (argIndex != commandIndex) {
                        commandBuilder.append(" ");
                    }
                    commandBuilder.append(args[argIndex]);
                });

        HttpRequest request = HttpRequest.newBuilder()
                .uri(create(encode(commandBuilder.toString())))
                .build();

        if (verbose) {
            out.println("Making request...");
            out.println(request.toString());
            out.println();
        }

        try {
            HttpResponse<String> response = newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());

            String body = response.body();

            ObjectMapper om = new ObjectMapper(new JsonFactory()
                    .configure(ALLOW_UNQUOTED_FIELD_NAMES, true)
                    .configure(ALLOW_COMMENTS, true))
                    .configure(INDENT_OUTPUT, true);

            JsonNode jsonNode = om.readTree(body);

            JsonNode responseNode = null;
            boolean commandFailed = false;
            boolean commandSuccess = false;

            final Iterator<Map.Entry<String, JsonNode>> fieldsIterator = jsonNode.fields();

            while (fieldsIterator.hasNext()) {

                final Map.Entry<String, JsonNode> next = fieldsIterator.next();

                if (next.getKey().equals(RESPONSE_STATUS_KEY)) {
                    if (Optional.of(next.getValue()).map(Objects::toString).stream().anyMatch(f -> f.equals(RESPONSE_STATUS_VALUE))) {
                        commandSuccess = true;
                        continue;
                    }
                }

                if (next.getKey().equals(RESPONSE_RESPONSE_KEY) || next.getKey().equals(RESPONSE_MESSAGE_KEY)) {
                    responseNode = next.getValue();
                    break;
                }

                if (next.getKey().equals(RESPONSE_ERROR_KEY)) {
                    final ObjectNode objectNode = JsonNodeFactory.withExactBigDecimals(true).objectNode();
                    responseNode = objectNode.set(next.getKey(), next.getValue());
                    commandFailed = true;
                    break;
                }
            }

            if (responseNode == null && commandSuccess) {
                responseNode = new TextNode("done");
            }

            if (commandFailed || jsonNode.isEmpty()) {
                if (jsonNode.isEmpty()) {
                    exitOnError();
                }
            }

            DumperOptions dumperOptions = new DumperOptions();
            dumperOptions.setAllowUnicode(true);
            dumperOptions.setPrettyFlow(true);
            dumperOptions.setSplitLines(false);
            dumperOptions.setIndentWithIndicator(false);
            dumperOptions.setIndicatorIndent(1);
            dumperOptions.setDefaultScalarStyle(PLAIN);
            dumperOptions.setDefaultFlowStyle(BLOCK);
            dumperOptions.setNonPrintableStyle(ESCAPE);
            Yaml yaml = new Yaml(dumperOptions);

            final Object load = yaml.load(responseNode.toPrettyString());


            if (!PRINTTREE_COMMAND.equals(args[commandIndex])) {
                recursiveHighlighter(load);
            }

            StringWriter w = new StringWriter();
            yaml.dump(load, w);
            out.println(w.toString()
                    .replaceAll("\\{", "")
                    .replaceAll("}", "")
                    .replaceAll("\"", "")
                    .replaceAll("\\\\e\\[34m", SYNTAX_HIGH_ESC_START + SYNTAX_HIGH_BLUE + SYNTAX_HIGH_ESC_END)
                    .replaceAll("\\\\e\\[0m", SYNTAX_HIGH_ESC_START + SYNTAX_HIGH_CLEAR + SYNTAX_HIGH_ESC_END));

            exit();

        } catch (Exception ex) {
            handleException(verbose, ex);
        }
    }

    private static void handleException(boolean verbose, Exception ex) {
        if (verbose) {
            ex.printStackTrace();
        } else if (Objects.equals("Connection refused", ex.getMessage())) {
            out.println("Could not connect to Minima. Make sure it has rpc enabled. Use --verbose to output a stacktrace.");
        } else {
            out.println("Error occurred. Use --verbose to output a stacktrace.");
        }
        exitOnError();
    }

    private static void printHelp() {
        out.println("-------------");
        out.println("Minima Client");
        out.println("-------------");
        out.println();
        out.println("Usage: mcli [options] <command> [params]  Send command to Minima");
        out.println("   or: mcli [options]");
        out.println("Options:");
        out.println();
        OPTIONS_MAP.forEach((key, value) -> out.format("%-15s%-15s%n", new Object[]{key, value}));
        exitOnError();
    }

    private static void exit() {
        out.println();
        System.exit(0);
    }

    private static void exitOnError() {
        out.println();
        System.exit(1);
    }

    private static Optional<String> lookAheadToNextArg(String[] programArgs, int currentIndex) {
        if (currentIndex + 1 <= programArgs.length - 1) {
            return of(programArgs[currentIndex + 1]);
        }
        return empty();
    }

    private static void recursiveHighlighter(Object obj) throws ParseException {
        if (obj instanceof ArrayList) {
            ((ArrayList) obj).forEach(loadedItem -> {
                try {
                    recursiveHighlighter(loadedItem);
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            });
        } else if (obj instanceof Map) {
            for (Map.Entry<String, Object> entry : ((Map<String, Object>) obj).entrySet()) {
                Object value = entry.getValue();
                if (value instanceof Map) {
                    Map<String, Object> subMap = (Map<String, Object>) value;
                    recursiveHighlighter(subMap);
                } else {
                    if (entry.getValue() == null) {
                        entry.setValue("");
                    } else {
                        entry.setValue(new StringBuilder()
                                .append(SYNTAX_HIGH_ESC_START)
                                .append(SYNTAX_HIGH_BLUE)
                                .append(SYNTAX_HIGH_ESC_END)
                                .append(value)
                                .append(SYNTAX_HIGH_ESC_START)
                                .append(SYNTAX_HIGH_CLEAR)
                                .append(SYNTAX_HIGH_ESC_END).toString());
                    }
                }

            }
        }
    }
}
