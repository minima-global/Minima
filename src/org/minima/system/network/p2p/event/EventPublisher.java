package org.minima.system.network.p2p.event;

import org.minima.system.network.p2p.Traceable;
import org.minima.utils.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.Optional.ofNullable;

public class EventPublisher {

    private static final List<Event> eventList = Collections.synchronizedList(new ArrayList<>());
    private static final ThreadLocal<String> TRACE_ID = new ThreadLocal<>();


    public static ArrayList<Event> getEvents() {
        ArrayList<Event> retEvents = new ArrayList<>();
        synchronized (eventList) {
            retEvents.addAll(eventList);
        }
        return retEvents;
    }

    public static void publishReadStream(Traceable traceable) {
        publish("reading stream", traceable);
    }

    public static void publishWrittenStream(Traceable traceable) {
        publish("written stream", traceable);
    }

    public static void publish(String eventType, JSONObject details, Traceable traceable){
        internalPublish(eventType, details, traceable);
    }

    public static void publish(String eventType, Traceable traceable) {
        internalPublish(eventType, traceable.getContent(), traceable);
    }

    private static void internalPublish(String eventType, JSONObject details, Traceable traceable) {
        if (eventType.contains("CONSENSUS") || eventType.contains("SENDTXPOW")) { // TODO Remove and add filter to endpoint
            return;
        }
        String derivedEventType = eventType + " " + traceable.getName();

        Event event = new Event(traceable.getTraceId(), null, derivedEventType, details);
        eventList.add(event);
    }
}
