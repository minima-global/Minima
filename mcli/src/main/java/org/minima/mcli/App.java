package org.minima.mcli;

import com.fasterxml.jackson.core.*;
import com.fasterxml.jackson.core.json.JsonWriteFeature;
import com.fasterxml.jackson.core.util.DefaultIndenter;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import com.fasterxml.jackson.dataformat.yaml.YAMLMapper;
import com.github.skjolber.jackson.jsh.*;
import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.nodes.Tag;

import java.io.IOException;
import java.io.StringWriter;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.stream.IntStream;

import static java.util.stream.IntStream.range;

public class App {

    private static final  String defaultColour = AnsiSyntaxHighlight.BLUE;
    private static final  String errorColour = AnsiSyntaxHighlight.RED;

    public static void main(String[] args) {


        final StringBuilder commandBuilder = new StringBuilder("http://127.0.0.1:9002/");

        range(0, args.length)
                .forEach(argIndex -> {
                    if (argIndex == 0) {
                    } else {
                        commandBuilder.append("%20");
                    }
                    commandBuilder.append(args[argIndex]);
                });
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(commandBuilder.toString()))
                .build();
        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                .thenApply(HttpResponse::body)
                .thenAccept(c -> {
                    try {
                        StringWriter writer = new StringWriter();

                        ObjectMapper om = new ObjectMapper(new JsonFactory()
                                .configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
                                .configure(JsonParser.Feature.ALLOW_COMMENTS, true))
                                .configure(SerializationFeature.INDENT_OUTPUT, true);
                        final JsonNode jsonNode = om.readTree(c);
                        JsonNode responseNode = null;
                        boolean commandFailed = false;

                        final Iterator<Map.Entry<String, JsonNode>> fieldsIterator = jsonNode.fields();
                        while (fieldsIterator.hasNext()) {
                            final Map.Entry<String, JsonNode> next = fieldsIterator.next();
                            if (next.getKey().equals("response")) {
                                responseNode = next.getValue();
                                break;
                            }

                            if (next.getKey().equals("error")) {
                                final ObjectNode objectNode = JsonNodeFactory.withExactBigDecimals(true).objectNode();
                                responseNode = objectNode.set(next.getKey(), next.getValue());
                                commandFailed = true;
                                break;
                            }
                        }
                        final JsonFactoryBuilder jsonFactoryBuilder = new JsonFactoryBuilder()
                                .configure(JsonWriteFeature.QUOTE_FIELD_NAMES, false)
                                .configure(JsonWriteFeature.WRITE_NUMBERS_AS_STRINGS, false);
                        JsonGenerator delegate = jsonFactoryBuilder.build().createGenerator(writer);

                        String syntaxColour = commandFailed ? errorColour : defaultColour;
                        DefaultSyntaxHighlighter.Builder highlighterBuilder =

                                DefaultSyntaxHighlighter
                                        .newBuilder()
                                        .withBoolean(syntaxColour)
                                        .withNull(syntaxColour)
                                        .withNumber(syntaxColour)
                                        .withString(syntaxColour);

                        if (commandFailed) {
                            highlighterBuilder.withField(errorColour);
                        }

                        SyntaxHighlightingJsonGenerator jsonGenerator = new SyntaxHighlightingJsonGenerator(delegate, highlighterBuilder.build(), true);
                        om.writeValue(jsonGenerator, responseNode);
                        System.out.println(writer.toString()
                                .replaceAll("\\{", "")
                                .replaceAll("}", "")
                                .replaceAll("\"", "")
                                .replaceAll(",", ""));

                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                })
                .join();
    }
}
