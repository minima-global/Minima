package org.minima.system.network.rpc;

import java.util.Base64;

import org.minima.objects.base.MiniString;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;

public class Authorizer {

	public static boolean checkAuchCredentials(String zAuthHeader) {
		
		//Are we even checking 
		if(!GeneralParams.RPC_AUTHENTICATE) {
			return true;
		}
		
		//Are we BASIC checking
		if(GeneralParams.RPC_AUTHSTYLE.equals("basic")) {
			
			try {
				//Is it basic Auth
				int pos = zAuthHeader.indexOf("Basic ");
				if(pos!=-1) {
					String userpass = zAuthHeader.substring(pos+6);
					
					byte[] dec 		= Base64.getDecoder().decode(userpass);
					String decstr 	= new String(dec, MiniString.MINIMA_CHARSET).trim();
					
					//Get the 2 bits..
					int col 		= decstr.indexOf(":");
					String user 	= decstr.substring(0,col);
					String password = decstr.substring(col+1, decstr.length());
					
					//Now check
					if(password.equals(GeneralParams.RPC_PASSWORD)) {
						return true;
					}
				}
				
			}catch(Exception exc) {
				MinimaLogger.log(exc);
				return false;
			}
		}
				
		return false;
	}
}
