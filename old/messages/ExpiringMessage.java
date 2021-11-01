package org.minima.system.network.p2p.old.messages;

import lombok.Getter;
import lombok.Setter;
import org.minima.utils.messages.Message;

@Getter
@Setter
public class ExpiringMessage {
    Message msg;
    long timestamp;

    public ExpiringMessage(Message msg){
        this.msg = msg;
        this.timestamp = System.currentTimeMillis();
    }

}
