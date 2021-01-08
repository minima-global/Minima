package org.minima.system.network.maxima;

import org.minima.system.Main;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Maxima extends MessageProcessor {

	public static final String MAXIMA_INIT    = "MAXIMA_INIT";
	
	public static final String MAXIMA_RECMSG   = "MAXIMA_RECMSG";
	public static final String MAXIMA_SENDMSG  = "MAXIMA_SENDMSG";
	
	MaximaServer mServer;
	
	public Maxima() {
		super("MAXIMA_PROCESSOR");
	
		PostMessage(MAXIMA_INIT);
	}
	
	public void stop() {
		if(mServer != null) {
			mServer.stop();
		}
		
		stopMessageProcessor();
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(MAXIMA_INIT)) {
			int port = Main.getMainHandler().getNetworkHandler().getMaximaPort();
			
			mServer = new MaximaServer(port);
			Thread max = new Thread(mServer, "Maxima Server");
			max.setDaemon(true);
			max.start();
			
		}else if(zMessage.getMessageType().equals(MAXIMA_RECMSG)) {
			
		
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMSG)) {
			
		}
		
	}

}
