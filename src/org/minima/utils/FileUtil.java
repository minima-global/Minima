package org.minima.utils;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

public class FileUtil {

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
		
    	//And Now convert to HEX!
    	System.out.println(BaseConverter.encode16(ret));
	}
	
	public static void main(String[] zArgs) {
		ConvertFileToHex(zArgs[0]);
	}
}
