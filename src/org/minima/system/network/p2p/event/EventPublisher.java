package org.minima.system.network.p2p.event;

import org.minima.system.network.p2p.Traceable;

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

    public static void publish(String eventType, Traceable traceable) {
        String derivedEventType = eventType + " " + traceable.getClass().getSimpleName();

        TRACE_ID.set(traceable.getTraceId());
        Event event = new Event(String.valueOf(Thread.currentThread().getId()), traceable.getTraceId(), derivedEventType);
        eventList.add(event);
    }

    public static void publish(String eventType) {
        if (eventType.contains("CONSENSUS") || eventType.contains("SENDTXPOW")) { // TODO Remove and add filter to endpoint
            return;
        }
        Event event = new Event(String.valueOf(Thread.currentThread().getId()), ofNullable(TRACE_ID.get())
                .map(Object::toString).orElse(null),
                eventType);
        eventList.add(event);
    }
}
