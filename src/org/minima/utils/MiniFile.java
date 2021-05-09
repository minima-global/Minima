package org.minima.utils;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

public class MiniFile {

	
	public static void writeDataToFile(File zFile, byte[] zData) throws IOException {
		writeDataToFile(zFile, zData, false);
	}
	
	public static void writeDataToFile(File zFile, byte[] zData, boolean zAppend) throws IOException {
		//Check Parent
		File parent = zFile.getAbsoluteFile().getParentFile();
		if(!parent.exists()) {
			parent.mkdirs();
		}
		
		//Delete the old..
		if(zFile.exists()) {
			if(!zAppend) {
				zFile.delete();
				zFile.createNewFile();
			}
		}else {
			zFile.createNewFile();
		}
		
		//Write it out..
		FileOutputStream fos = new FileOutputStream(zFile, zAppend);
		DataOutputStream fdos = new DataOutputStream(fos);
		
		//And write it..
		fdos.write(zData);
		
		//flush
		fdos.flush();
		fos.flush();
		
		fdos.close();
		fos.close();
	}
	
	public static void writeObjectToFile(File zFile, Streamable zObject) throws IOException {
		writeObjectToFile(zFile, zObject, false);
	}
	
	public static void writeObjectToFile(File zFile, Streamable zObject, boolean zAppend) throws IOException {
		//First write the object to a memory structure..
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		
		zObject.writeDataStream(dos);
		
		dos.flush();
		baos.flush();
		
		//get all the data
		byte[] data = baos.toByteArray();
	
		dos.close();
		baos.close();
		
		//Check Parent
		writeDataToFile(zFile, data, zAppend);
	}
	
	public static byte[] readCompleteFile(File zFile) throws IOException {
    	long size  = zFile.length();
    	byte[] ret = new byte[(int) size];
    	
		FileInputStream fis     = new FileInputStream(zFile);
		BufferedInputStream bis = new BufferedInputStream(fis);
		
		bis.read(ret);
        
        bis.close();
        fis.close();
    
        return ret;
	}
	
	public static void copyFile(File zOrig, File zCopy) throws IOException {
		//Check file exists
		if(!zOrig.exists()){
			MinimaLogger.log("Trying to copy file that does not exist "+zOrig.getAbsolutePath());
			return;
		}
		
		//read in the original..
		byte[] orig = readCompleteFile(zOrig);
		
		//And now write..
		writeDataToFile(zCopy, orig);
	}
	
	public static void deleteFileOrFolder(String mParentCheck, File zFile) {
		//Check for real
		if(zFile == null || !zFile.exists()) {
			return;
		}
		
		//Scan if Directory
		if(zFile.isDirectory()) {
			//List the files..
			File[] files = zFile.listFiles();
			if(files != null) {
				for(File ff : files) {
					deleteFileOrFolder(mParentCheck, ff);
				}
			}	
		}
		
		//And finally delete the actual file.. (double check is a minima file.. )
		if(mParentCheck.equals("")) {
			zFile.delete();
		}else if(zFile.getAbsolutePath().startsWith(mParentCheck)) {
			zFile.delete();
		}else {
			MinimaLogger.log("Attempt to delete File NOT child of parent check "+zFile.getAbsolutePath()+" / "+mParentCheck);
		}
	}
	
	public static String getContentType(String zFile) {
		
		String ending;
		int dot = zFile.lastIndexOf(".");
		if(dot != -1) {
			ending = zFile.substring(dot+1);
		}else {
			return "text/plain";
		}
		
		if(ending.equals("html")) {
			return "text/html";
		}else if(ending.equals("htm")) {
			return "text/html";
		}else if(ending.equals("css")) {
			return "text/css";
		}else if(ending.equals("js")) {
			return "text/javascript";
		}else if(ending.equals("txt")) {
			return "text/plain";
		}else if(ending.equals("xml")) {
			return "text/xml";
		
		}else if(ending.equals("jpg")) {
			return "image/jpeg";
		}else if(ending.equals("jpeg")) {
			return "image/jpeg";
		}else if(ending.equals("png")) {
			return "image/png";
		}else if(ending.equals("gif")) {
			return "image/gif";
		}else if(ending.equals("svg")) {
			return "image/svg+xml";
		}else if(ending.equals("ico")) {
			return "image/ico";
		
		}else if(ending.equals("ttf")) {
			return "font/ttf";
		
		}else if(ending.equals("zip")) {
			return "application/zip";
		}else if(ending.equals("pdf")) {
			return "application/pdf";
			
		}else if(ending.equals("mp3")) {
			return "audio/mp3";
		}else if(ending.equals("wav")) {
			return "audio/wav";
		}
		
		return "text/plain";
	}
}
