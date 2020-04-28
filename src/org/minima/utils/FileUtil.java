package org.minima.utils;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;

import org.minima.objects.base.MiniData;

public class FileUtil {

	public final static int MAX_STRING = 1000;
	
	public static void ConvertFileToHex(String zFilePath) {
		File ff = new File(zFilePath);
    	
    	long size  = ff.length();
    	byte[] ret = new byte[(int) size];
    	
    	try {
			FileInputStream fis     = new FileInputStream(zFilePath);
			BufferedInputStream bis = new BufferedInputStream(fis);
			
			bis.read(ret);
	        
	        bis.close();
	        fis.close();
	        
		} catch (IOException e) {
			e.printStackTrace();
		} 
		
	    //Now chop it up..
    	System.out.println("//FILE SIZE "+size);
		int num = (int) (size / MAX_STRING) + 1;
		System.out.println("//HEX NUM "+num);
		
		System.out.println("public final static int HEXNUM = "+num+";");
		System.out.println("public final static byte[][] HEXDATA = new byte["+num+"][];");
		System.out.println("public static byte[] FINAL_ARRAY = null;");
		
		System.out.println("static {");
		
		for(int i=0;i<num;i++) {
			//loop through it..
			int start = i*MAX_STRING;
			int end   = ((i+1)*MAX_STRING);
			
			if(end > size) {
				end = (int) size;
			}
			
			System.out.println("//"+start+" - "+end);
			byte[] copy = Arrays.copyOfRange(ret, start, end);
			String output = "HEXDATA["+i+"] = new MiniData(\""+BaseConverter.encode16(copy)+"\").getData();";
        	System.out.println(output);
		}
		
		System.out.println("}");
		
		System.out.println("\n"
			  + "		public static byte[] returnData() throws IOException {\n" + 
				"			if(FINAL_ARRAY == null) {\n" + 
				"				ByteArrayOutputStream baos = new ByteArrayOutputStream();\n" + 
				"				for(int i=0;i<HEXNUM;i++) {\n" + 
				"					baos.write(HEXDATA[i]);\n" + 
				"				}\n" + 
				"				baos.flush();\n" + 
				"				FINAL_ARRAY = baos.toByteArray();	\n" + 
				"			}\n" + 
				"		    return FINAL_ARRAY;\n" + 
				"		}");
		
	}
	
	public static void main(String[] zArgs) {
		ConvertFileToHex(zArgs[0]);
		
	}
}
