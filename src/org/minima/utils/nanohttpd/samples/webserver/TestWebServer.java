package org.minima.utils.nanohttpd.samples.webserver;

import java.io.File;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.nanohttpd.protocols.http.IHTTPSession;
import org.minima.utils.nanohttpd.protocols.http.NanoHTTPD;
import org.minima.utils.nanohttpd.protocols.http.request.Method;
import org.minima.utils.nanohttpd.protocols.http.response.Response;
import org.minima.utils.nanohttpd.protocols.http.response.Status;
import org.minima.utils.nanohttpd.util.ServerRunner;

public class TestWebServer extends NanoHTTPD{

	String mWebRoot = "/home/spartacusrex/.minima/testweb/getpost/";
	
	public TestWebServer() {
		super(8080);
	}
	
	@Override
    public Response serve(IHTTPSession session) {
        try {
        	//GET or POST
        	Method method = session.getMethod();
			
        	//What are they looking for..
        	String fileRequested = session.getUri();
        	
        	//Quick clean
			if(fileRequested.endsWith("/")) {
				fileRequested = fileRequested.concat("index.html");
			}
			if(fileRequested.startsWith("/")) {
				fileRequested = fileRequested.substring(1);
			}
        	
			//get from the testweb folder..
			String fullfile = mWebRoot+fileRequested;
			
			//Log it..
			MinimaLogger.log("File : "+fullfile);
			
			//GET or POST
			if(Method.GET.equals(method)) {
				MinimaLogger.log("GET ");
		    
				//Any parameters
	        	Map<String, List<String>> params = session.getParameters();
	        	MinimaLogger.log("PARAMS : "+params);
	        
			}else if(Method.POST.equals(method)) {
				MinimaLogger.log("POST ");

				//get the file..
		        Map<String, String> files = new HashMap<String, String>();
	            session.parseBody(files);
	        
	            MinimaLogger.log("Files : "+files);
		    
	            //Any parameters
	        	Map<String, List<String>> params = session.getParameters();
	        	MinimaLogger.log("PARAMS : "+params);
	        
			}
			
			byte[] file     = MiniFile.readCompleteFile(new File(fullfile));
			
			if(file.length>0) {
				return getOKResponse(file, MiniFile.getContentType(fullfile));
			}
			      	
			return getNotFoundResponse();
     
        } catch (Exception ioe) {
        	MinimaLogger.log("DAPPSERVER Error : "+ioe);
        	
        	return getInternalErrorResponse("INTERNAL ERROR");
        }
    }
	
	protected Response getOKResponse(byte[] zHTML, String zContentType) {
		Response resp = Response.newFixedLengthResponse(Status.OK, zContentType, zHTML);
		resp.addHeader("Server", "HTTP RPC Server from Minima v0.95.14");
		resp.addHeader("Date", new Date().toString());
		
		//Cache images..
		if(zContentType.startsWith("image/")) {
			resp.addHeader("Cache-Control", "max-age=86400");
		}
		
//		resp.addHeader("Access-Control-Allow-Origin", "*");
		
        return resp;
    }
	
	protected Response getForbiddenResponse(String s) {
        return Response.newFixedLengthResponse(Status.FORBIDDEN, NanoHTTPD.MIME_PLAINTEXT, "FORBIDDEN: " + s);
    }

    protected Response getInternalErrorResponse(String s) {
        return Response.newFixedLengthResponse(Status.INTERNAL_ERROR, NanoHTTPD.MIME_PLAINTEXT, "INTERNAL ERROR: " + s);
    }

    protected Response getNotFoundResponse() {
        return Response.newFixedLengthResponse(Status.NOT_FOUND, NanoHTTPD.MIME_PLAINTEXT, "Error 404, file not found.");
    }
    
    public static void main(String[] zArgs) {
    	ServerRunner.executeInstance(new TestWebServer());	
    }
    
}
