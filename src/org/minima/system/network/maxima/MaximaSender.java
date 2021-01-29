package org.minima.system.network.maxima;

import java.io.IOException;

import org.minima.system.input.InputHandler;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.messages.Message;

public class MaximaSender implements Runnable {

	Message mMessage;
	
	String mTo;
	String mPost;
	
	public MaximaSender(Message zMessage,String zTo,  String zPost) {
		mMessage = zMessage;
		mTo 	 = zTo;
		mPost    = zPost;
	}
	
	@Override
	public void run() {
		//Send it..
		String resp = "";
		try {
			resp = RPCClient.sendPOST(mTo, mPost);
		}catch(IOException ioexc){
			InputHandler.endResponse(mMessage, false, "Failed to connect to "+mTo);
			return;
		}
		
		//Reply..
		InputHandler.getResponseJSON(mMessage).put("reply", resp);
		InputHandler.endResponse(mMessage, true, "Message sent");
	}	
}
