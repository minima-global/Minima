package org.minima.system.network;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.minima.utils.messages.Message;

public class WebProxyReader implements Runnable {

	WebProxyManager mManager;
	
	public WebProxyReader(WebProxyManager zManager) {
		mManager = zManager;
	}
	
	@Override
	public void run() {
		try {
			//Get a reader
			BufferedReader bis = new BufferedReader(new InputStreamReader(mManager.getSocket().getInputStream()));
			
			while(true) {
				//read in a request.. must end in newline .. \n
				String request = bis.readLine();
				
				//Socket sht down.. ?
				if(request == null) {
					System.out.println("Proxy Web Link null input.. shutdown");
					break;
				}
				
				//Break it down into the randomid and the actual request..
				int index       = request.indexOf(":");
				String rid      = request.substring(0,index);
				String function = request.substring(index+1);
				
				//Make a message
				Message reqmsg = new Message(WebProxyManager.WEBPROXY_REQUEST);
				reqmsg.addString("function", function);
				reqmsg.addString("randid", rid);
				
				//And post it..
				mManager.PostMessage(reqmsg);
			}
			
		}catch(Exception exc) {
//			exc.printStackTrace();
		}
		
		//Tell it to shutdown...
		mManager.PostMessage(WebProxyManager.WEBPROXY_SHUTDOWN);
	}

}
