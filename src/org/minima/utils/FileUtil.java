package org.minima.utils;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;

import org.minima.objects.base.MiniData;

public class FileUtil {

	public final static int MAX_STRING = 10000;
	
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
		if(size>MAX_STRING) {
    		int num = (int) (size / MAX_STRING) + 1;
    		System.out.println("//HEX NUM "+num);
    		for(int i=0;i<num;i++) {
    			//loop through it..
    			int start = i*MAX_STRING;
    			int end   = ((i+1)*MAX_STRING)-1;
    			
    			if(end > size) {
    				end = (int) size;
    			}
    			
    			System.out.println("//"+start+" - "+end);
    			byte[] copy = Arrays.copyOfRange(ret, start, end);
    			String output = "public final static byte[] HEXDATA"+i+" = new MiniData(\""+BaseConverter.encode16(copy)+"\").getData();";
            	System.out.println(output);
    		}
    		
    	}else {
    		String output = "public final static byte[] HEXDATA = new MiniData(\""+BaseConverter.encode16(ret)+"\").getData();";
        	System.out.println(output);	
    	}
    	
	}
	
	public static void main(String[] zArgs) {
		ConvertFileToHex(zArgs[0]);
	}
}
