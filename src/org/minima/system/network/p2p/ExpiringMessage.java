package org.minima.system.network.p2p;

import lombok.Getter;
import org.minima.utils.messages.Message;

@Getter
public class ExpiringMessage {
    Message msg;
    long timestamp;

    public ExpiringMessage(Message msg){
        this.msg = msg;
        this.timestamp = System.currentTimeMillis();
    }

}
