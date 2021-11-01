package org.minima.system.network.p2p.old;

import lombok.Getter;
import org.minima.objects.base.MiniData;

@Getter
public class ConnectionDetails {

    ConnectionReason reason;
    MiniData auth_key;

    public ConnectionDetails() {}

    public ConnectionDetails(ConnectionReason reason){
        this.reason = reason;
        this.auth_key = null;
    }

    public ConnectionDetails(ConnectionReason reason, MiniData auth_key){
        this.reason = reason;
        this.auth_key = auth_key;
    }

}
