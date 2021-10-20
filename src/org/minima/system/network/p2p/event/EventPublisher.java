package org.minima.system.network.p2p.event;

import org.minima.system.network.p2p.Traceable;
import org.minima.utils.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static java.util.Optional.ofNullable;

public class EventPublisher {

    public static final ThreadLocal<String> threadTraceId = new ThreadLocal<>();

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

    public static void publish(String eventType, Traceable traceable) {
        String derivedEventType = eventType + " " + traceable.getName();

        Event event = new Event(threadTraceId.get(), traceable.getTraceId(), derivedEventType, traceable.getContent());
        eventList.add(event);
    }

    public static void publish(String eventType, JSONObject details){
        internalPublish(eventType, details);
    }

    public static void publish(String eventType) {
        internalPublish(eventType, new JSONObject());
    }

    private static void internalPublish(String eventType, JSONObject details) {
        if (eventType.contains("CONSENSUS") || eventType.contains("SENDTXPOW")) { // TODO Remove and add filter to endpoint
            return;
        }
        String traceId2 = ofNullable(TRACE_ID.get())
                .map(Object::toString)
                .orElse(null);
        Event event = new Event(threadTraceId.get(), null, eventType, details);
        eventList.add(event);
    }
}
