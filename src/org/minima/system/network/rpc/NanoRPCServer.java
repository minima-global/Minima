package org.minima.system.network.rpc;

import java.net.URLDecoder;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.network.commands.CMD;
import org.minima.system.network.commands.FILE;
import org.minima.system.network.commands.NET;
import org.minima.system.network.commands.SQL;
import org.minima.utils.MinimaLogger;
import org.minima.utils.nanohttpd.protocols.http.IHTTPSession;
import org.minima.utils.nanohttpd.protocols.http.NanoHTTPD;
import org.minima.utils.nanohttpd.protocols.http.request.Method;
import org.minima.utils.nanohttpd.protocols.http.response.Response;
import org.minima.utils.nanohttpd.protocols.http.response.Status;

public class NanoRPCServer extends NanoHTTPD{

	int mPort;
	
	public NanoRPCServer(int zPort) {
		super(zPort);
		mPort = zPort;
		
		//SSL ?
		if(Main.getMainHandler().getNetworkHandler().isSSLEnabled()) {
			makeSecure(Main.getMainHandler().getNetworkHandler().getSSLServerFactory(), null);
		}
				
		//Log it..
		MinimaLogger.log("RPC Server started on port "+zPort);
	}

	public int getPort() {
		return mPort;
	}
	
	@Override
    public Response serve(IHTTPSession session) {
        try {
        	//GET or POST
        	Method method = session.getMethod();
			
        	//What are they looking for..
        	String fileRequested = session.getUri();
        	
        	//Remove slashes..
        	if(fileRequested.endsWith("/")) {
        		fileRequested = fileRequested.substring(0,fileRequested.length()-1);
			}
        	if(fileRequested.startsWith("/")) {
				fileRequested = fileRequested.substring(1);
			}
        	
        	MinimaLogger.log("NANORPC REQUEST "+fileRequested);
        	
			//Any parameters
        	Map<String, List<String>> params = new HashMap<>();
        	
        	//Any Files Uploaded..
        	Map<String, String> files = new HashMap<String, String>();
        	
        	String command = null,reqtype = null,finalresult = null;
        	
        	//GET or POST
			if(Method.GET.equals(method)) {
				//Any parameters
	        	params = session.getParameters();
			
	        	MinimaLogger.log("NANORPC GET "+params.toString());
				
	        	//decode URL message
				String function = URLDecoder.decode(fileRequested,"UTF-8").trim();
				
				if(function.startsWith("sql/")) {
					//Get the SQL function
					reqtype="sql";
					command = function.substring(4).trim();
					
				}else if(function.startsWith("net/")) {
					reqtype="net";
					command = function.substring(4).trim();
					
				}else if(function.startsWith("file/")) {
					reqtype="file";
					command = function.substring(5).trim();
					
				}else {
					reqtype="cmd";
					command = new String(function);
				}
				
			}else if(Method.POST.equals(method)) {
				//get the files.. if any MUST DO THIS FIRST - for NANOHTTPD
		        session.parseBody(files);
            
	            //NOW - get any parameters
	        	params = session.getParameters();
	        	
	        	MinimaLogger.log("NANORPC POST "+params.toString());
	        	
	        	
			}
			
			//Get the MinDAPP ID
			String MiniDAPPID = "0x00";
			int slash = fileRequested.indexOf("/");
			if(slash!=-1) {
				MiniDAPPID = fileRequested.substring(slash+1);
				reqtype    = reqtype.substring(0,slash);
			}
			
			//Is this a SQL function
			if(reqtype.equals("sql")) {
				//Create a SQL object
				SQL sql = new SQL(command, MiniDAPPID);
				
				//Run it..
				sql.run();
				
				//Get the Response..
            	finalresult = sql.getFinalResult();
				
			}else if(reqtype.equals("cmd")) {
				CMD cmd = new CMD(command);
            	
            	//Run it..
            	cmd.run();
 
            	//Get the Response..
            	finalresult = cmd.getFinalResult();
			
			}else if(reqtype.equals("file")) {
				//File access..
				FILE file = new FILE(command, MiniDAPPID);
				
				//Run it..
				file.run();
				
				//Get the Response..
            	finalresult = file.getFinalResult();
			
			}else if(reqtype.equals("net")) {
				//Network Comms
				NET netcomm = new NET(command, MiniDAPPID);
				
				//Run it..
				netcomm.run();
				
				//Get the Response..
            	finalresult = netcomm.getFinalResult();
			
			}else {
				finalresult = "{\"status\":false, \"message\":\"Incorrect command TYPE..\"}";
			}
			
			//Make sure in the correct CHARSET
			return getOKResponse(new MiniString(finalresult).toString(), "text/plain");
        	
        }catch(Exception exc) {
        	MinimaLogger.log(exc);
        }
        
        return getInternalErrorResponse("INTERNAL ERROR");
	}
	
	protected Response getOKResponse(String zText, String zContentType) {
		Response resp = Response.newFixedLengthResponse(Status.OK, zContentType, zText);
		resp.addHeader("Server", "HTTP RPC Server from Minima v0.95.14");
		resp.addHeader("Date", new Date().toString());
		resp.addHeader("Access-Control-Allow-Origin", "*");
		return resp;
    }
	
    protected Response getInternalErrorResponse(String s) {
    	Response resp = Response.newFixedLengthResponse(Status.INTERNAL_ERROR, NanoHTTPD.MIME_PLAINTEXT, "INTERNAL ERROR: " + s);
		resp.addHeader("Server", "HTTP RPC Server from Minima v0.95.14");
		resp.addHeader("Date", new Date().toString());
		resp.addHeader("Access-Control-Allow-Origin", "*");
		return resp;
//    	return Response.newFixedLengthResponse(Status.INTERNAL_ERROR, NanoHTTPD.MIME_PLAINTEXT, "INTERNAL ERROR: " + s);
    }
}
