package org.minima.system.network.p2p.event;

import org.minima.system.network.p2p.Traceable;
import org.minima.utils.json.JSONObject;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class EventPublisher {

    private static final List<Event> eventList = Collections.synchronizedList(new ArrayList<>());

    public static ArrayList<Event> getEvents() {
        ArrayList<Event> retEvents = new ArrayList<>();
        synchronized (eventList) {
            retEvents.addAll(eventList);
        }
        return retEvents;
    }

    public static void publishReadStream(Traceable traceable) {
        publish("read stream", traceable);
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
        String derivedEventType = eventType + " " + traceable.getName();
        String category = derivedEventType.toLowerCase().contains("p2p") ? "p2p" : null;

        Event event = new Event(traceable.getTraceId(), category, derivedEventType, details);
        eventList.add(event);
    }
}
