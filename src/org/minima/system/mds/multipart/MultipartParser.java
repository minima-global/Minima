package org.minima.system.mds.multipart;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.StringTokenizer;

public class MultipartParser {

	public static MultipartData getTextData(String zName, String zData) {
		MultipartData mpd 	= new MultipartData();
		mpd.mType 			= MultipartData.TYPE_TEXT;
		mpd.mName			= zName;
		mpd.mTextData		= zData;
		return mpd;
	}
	
	public static MultipartData getFileData(String zName, String zFilename, String zContentType, byte[] zData) {
		MultipartData mpd 	= new MultipartData();
		mpd.mType 			= MultipartData.TYPE_FILE;
		mpd.mName			= zName;
		mpd.mFileName		= zFilename;
		mpd.mContentType	= zContentType;
		mpd.mFileData		= zData;
		return mpd;
	}
	
	public static Hashtable<String, MultipartData> parseMultipartData(byte[] zAllData) throws IOException {
		
		//The returned set of data
		Hashtable<String, MultipartData> data = new Hashtable<>();
		
		//Now parse the data
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zAllData);
		DataInputStream dis 		= new DataInputStream(bais);
		
		//Get the boundary
		String line = dis.readLine();
		byte[] barr = line.getBytes();
		
		bais.close();
		dis.close();
		
		ArrayList<ByteArray> allsections = new ArrayList<>();
		int currentpos=0;
		while(true) {
			
			//Get the next boundary..
			int index = indexOf(currentpos+1, zAllData, barr);
			if(index != -1) {
				
				//Found a boundary - get all the data
				int len = index - currentpos;
				byte[] section = new byte[len];
				System.arraycopy(zAllData, currentpos, section, 0, len);
				
				//Add to our list
				allsections.add(new ByteArray(section));
			
				currentpos = index;
			}else{
				break;
			}
		}
		
		//Now go through the sections
		for(ByteArray section : allsections) {
			
			//MinimaLogger.log("SECTION:\n"+new String(section.getBytes()));
			
			bais = new ByteArrayInputStream(section.getBytes());
			dis  = new DataInputStream(bais);
			
			//Get the boundary
			line = dis.readLine();
			
			//Get the next header
			boolean isfile 		= false;
			String name 		= "noname";
			String value		= "";
			
			String filename 	= "noname";
			String contenttype	= "text/plain";
			byte[] formdata		= null;
			
			line = dis.readLine();
			while(!line.equals("")) {
				
				StringTokenizer strtok = new StringTokenizer(line,";");
				while(strtok.hasMoreTokens()) {
					String tok = strtok.nextToken().trim();
					
					if(tok.startsWith("Content-Disposition:")) {
						
					}else if(tok.startsWith("Content-Type:")) {
						contenttype = tok.substring(13).trim();
						
					}else if(tok.startsWith("name=")) {
					
						String val = tok.substring(5);
						if(val.startsWith("\"")) {
							val = val.substring(1);
						}
						if(val.endsWith("\"")) {
							val = val.substring(0,val.length()-1);
						}
				
						name = val.trim();
						
					}else if(tok.startsWith("filename=")) {
						isfile = true;
						
						String val = tok.substring(9);
						if(val.startsWith("\"")) {
							val = val.substring(1);
						}
						if(val.endsWith("\"")) {
							val = val.substring(0,val.length()-1);
						}
				
						filename = val.trim();
					}
				}
			
				//Next line
				line = dis.readLine();
			}
			
			//The last bit..
			MultipartData mpd = null;
			if(!isfile) {
				
				//Get the text data
				value = dis.readLine().trim();
				
				mpd = getTextData(name, value);
			}else {
				
				//How much is left
				int available = dis.available();
				byte[] availdata = new byte[available-2];
				dis.read(availdata, 0, available-2);
				
				//Create the Multipart
				mpd = getFileData(name, filename, contenttype, availdata);
				
//				//Get the file data
//				byte[] allfiledata = dis.readAllBytes();
//				
//				//Remove the last 2 bytes \r\n
//				byte[] filedata = new byte[allfiledata.length-2];
//				
//				//Copy the data
//				System.arraycopy(allfiledata, 0, filedata, 0, filedata.length);
				
				//And create the MultiPart
//				mpd = getFileData(name, filename, contenttype, filedata);
			}
			
			//Add to our list..
			data.put(name, mpd);
			
			bais.close();
			dis.close();
		}
		
//		Set<String> keys = data.keySet();
//		for(String key : keys) {
//			MultipartData mpd = data.get(key);
//			MinimaLogger.log(mpd.toJSON().toString());
//		}
		
		return data;
	}
	
	public static int indexOf(int zStartPos, byte[] zMainArray, byte[] zSearchArray) {
	    for(int i = zStartPos; i < zMainArray.length - zSearchArray.length+1; ++i) {
	        boolean found = true;
	        for(int j = 0; j < zSearchArray.length; ++j) {
	           if (zMainArray[i+j] != zSearchArray[j]) {
	               found = false;
	               break;
	           }
	        }
	        if (found) return i;
	     }
	   return -1;  
	}
}
