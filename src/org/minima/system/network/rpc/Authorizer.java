package org.minima.system.network.rpc;

import java.util.Base64;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.base.MiniString;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class Authorizer {

	public static JSONObject checkAuchCredentials(String zAuthHeader) {
		
		JSONObject falseret = new JSONObject();
		falseret.put("valid",false);
		
		JSONObject ret = new JSONObject();
		ret.put("valid",false);
				
		UserDB userdb = MinimaDB.getDB().getUserDB();
		int rpcusers  = userdb.getRPCUsers().size();		
		
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
					
					ret.put("username",user);
					
					//Now check
					if(user.equals("minima")) {
						if(!GeneralParams.RPC_AUTHENTICATE || password.equals(GeneralParams.RPC_PASSWORD)) {
							
							ret.put("valid",true);
							ret.put("mode","write");
							
							return ret;
						}
					
					}else {
						
						JSONArray users = userdb.getRPCUsers();
						for(Object userobj : users) {
							JSONObject rpcuser = (JSONObject)userobj;
							
							//Is it the one to be removed..
							if( rpcuser.getString("username").equals(user) && 
								rpcuser.getString("password").equals(password)) {
								
								ret.put("valid",true);
								ret.put("mode",rpcuser.getString("mode"));
							}
						}
						
						return ret;
					}
				}
				
			}catch(Exception exc) {
				MinimaLogger.log(exc);
				return falseret;
			}
		}
				
		return falseret;
	}
}
