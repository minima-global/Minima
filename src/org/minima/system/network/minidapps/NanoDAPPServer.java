package org.minima.system.network.minidapps;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.minima.system.network.minidapps.hexdata.faviconico;
import org.minima.system.network.minidapps.hexdata.helphtml;
import org.minima.system.network.minidapps.hexdata.iconpng;
import org.minima.system.network.minidapps.hexdata.indexhtml;
import org.minima.system.network.minidapps.hexdata.installdapphtml;
import org.minima.system.network.minidapps.hexdata.minidappscss;
import org.minima.system.network.minidapps.hexdata.minimajs;
import org.minima.system.network.minidapps.hexdata.tilegreyjpeg;
import org.nanohttpd.protocols.http.IHTTPSession;
import org.nanohttpd.protocols.http.NanoHTTPD;
import org.nanohttpd.protocols.http.request.Method;
import org.nanohttpd.protocols.http.response.Response;
import org.nanohttpd.protocols.http.response.Status;

public class NanoDAPPServer extends NanoHTTPD{

	public NanoDAPPServer(int zPort) {
		super(zPort);

	}

	@Override
    public Response serve(IHTTPSession session) {
        try {
        	//GET or POST
        	Method method                   = session.getMethod();
			
        	//What are they looking for..
        	String fileRequested = session.getUri();
			//System.out.println("GET "+fileRequested);
			
        	//Quick clean
			if(fileRequested.endsWith("/")) {
				fileRequested = fileRequested.concat("index.html");
			}
			if(fileRequested.startsWith("/")) {
				fileRequested = fileRequested.substring(1);
			}
        	
			//GET or POST
			if(Method.GET.equals(method)) {
				//Any parameters
	        	Map<String, List<String>> params = session.getParameters();
	        	
	        	//Is there an uninstall..
	        	String uninst = "";
	        	List<String> uninstall = params.get("uninstall");
	        	if(uninstall != null) {
	        		uninst = uninstall.get(0);
	        	}
	        	
		        //Are we uninstalling a MiniDAPP
				if(fileRequested.equals("index,html") && !uninst.equals("")) {
					//UNINSTALL the DAPP
					//..
					
					return getOKResponse(indexhtml.returnData());
				}
			
				//Otherwise lets see..
				if(fileRequested.endsWith("/minima.js") || fileRequested.equals("minima.js")) {
					return getOKResponse(minimajs.returnData());
				
				}else if(fileRequested.startsWith("minidapps/")) {
					//Send the MiniDAPP!
					//..
					
				}else {
					if(fileRequested.equals("index.html")) {
						String page    = new String(indexhtml.returnData(),StandardCharsets.UTF_8);
//						String newpage = page.replace("######", createMiniDAPPList());
						return getOKResponse(page.getBytes());
						
					}else if(fileRequested.equals("css/minidapps.css")) {
						return getOKResponse(minidappscss.returnData());
						
					}else if(fileRequested.equals("favicon.ico")) {
						return getOKResponse(faviconico.returnData());
						
					}else if(fileRequested.equals("help.html")) {
						return getOKResponse(helphtml.returnData());
					
					}else if(fileRequested.equals("icon.png")) {
						return getOKResponse(iconpng.returnData());
						
					}else if(fileRequested.equals("installdapp.html")) {
						return getOKResponse(installdapphtml.returnData());
						
					}else if(fileRequested.equals("tile-grey.jpeg")) {
						return getOKResponse(tilegreyjpeg.returnData());
						
					}else {
						//Not found..
						return getNotFoundResponse();	
					}	
				}
			}
        	
			return getNotFoundResponse();
        	
        	
        	//Any parameters
//        	Map<String, List<String>> parms = session.getParameters();
//        	
//            System.out.println("URI : "+uri);
//            System.out.println("METHOD : "+method.toString());
//            System.out.println("PARAMS "+parms);
//            
//            if(Method.GET.equals(method)) {
//        		return getOKResponse(getpage);
//            }
//            
//            if(Method.POST.equals(method)) {
//            	//Do some parsing!
//              Map<String, String> files = new HashMap<String, String>();
//              session.parseBody(files);
//            
//              System.out.println("FILES : "+files);
//		      
//              String fileloc = files.get("myfile");
//              File ff = new File(fileloc);
//             
//              System.out.println("UPLOAD : "+ff.getAbsolutePath()+" "+ff.exists()+" "+ff.length());
//		      
//	          return getOKResponse(postpage);
//            }
//        	
//            return getOKResponse("NOT GET OR POST");

        } catch (Exception ioe) {
            return getInternalErrorResponse("INTERNAL ERROR");
        }
    }
	
	protected Response getOKResponse(byte[] zHTML) {
//		Response resp = Response.newFixedLengthResponse(Status.OK, NanoHTTPD.MIME_HTML, zHTML);
		Response resp = Response.newFixedLengthResponse(Status.OK, "text/html", zHTML);
		resp.addHeader("Server", "HTTP RPC Server from Minima v0.88");
		resp.addHeader("Date", new Date().toString());
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
	
	
}
