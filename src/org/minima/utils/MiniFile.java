package org.minima.utils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.minima.objects.base.MiniData;
import org.minima.system.params.GeneralParams;
import org.minima.utils.encrypt.PasswordCrypto;

public class MiniFile {
	
	
	public static File createBaseFile(String zFilename) {
		
		//Does the name have any slashes in it..
		if(zFilename.contains(File.separator) || zFilename.contains("\\") || zFilename.contains("/")) {
			
			//It's trying to be an absolute path
			return new File(zFilename);
		}
		
		//HAve we specified a base folder
		if(GeneralParams.BASE_FILE_FOLDER.equals("")) {
			
			//Use the default location
			return  new File(zFilename);
		}
		
		return new File(GeneralParams.BASE_FILE_FOLDER,zFilename);
	}
	
	
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
	
	public static void loadObject(File zFile, Streamable zObject) {
		//Does the File exist
		if(!zFile.exists()) {
			MinimaLogger.log("Load Object file does not exist : "+zFile.getAbsolutePath());
			return;
		}
		
		try {
			//Read the whole file.. fast
			byte[] data = MiniFile.readCompleteFile(zFile);
			
			//Convert to a Streamable object
			ByteArrayInputStream bais = new ByteArrayInputStream(data);
			DataInputStream dis = new DataInputStream(bais);
			zObject.readDataStream(dis);
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
	}
	
	public static void loadObjectEncrypted(String zPassword, File zFile, Streamable zObject) {
		//Does the File exist
		if(!zFile.exists()) {
			MinimaLogger.log("Load Object file does not exist : "+zFile.getAbsolutePath());
			return;
		}
		
		try {
			//Read the whole file.. fast
			byte[] data = MiniFile.readCompleteFile(zFile);
			
			//Now decrypt
			MiniData decrypted = PasswordCrypto.decryptPassword(zPassword, new MiniData(data));
			
			//Convert to a Streamable object
			ByteArrayInputStream bais = new ByteArrayInputStream(decrypted.getBytes());
			DataInputStream dis = new DataInputStream(bais);
			zObject.readDataStream(dis);
			dis.close();
			bais.close();
			
		} catch (Exception e) {
			MinimaLogger.log(e);
		}
	}
	
	public static void saveObject(File zFile, Streamable zObject) {
		try {
			//Write into byte array
			MiniData casc = MiniData.getMiniDataVersion(zObject);
			
			//save to disk
			MiniFile.writeDataToFile(zFile, casc.getBytes());
			
		}catch(IOException exc) {
			MinimaLogger.log(exc);
		}
	}
	
	public static void saveObjectDirect(File zFile, Streamable zObject) {
		//Check Parent
		File parent = zFile.getAbsoluteFile().getParentFile();
		if(!parent.exists()) {
			parent.mkdirs();
		}
		
		try {
		
			//Delete the old..
			if(zFile.exists()) {
				zFile.delete();
				zFile.createNewFile();
			}
			
			//Write it out..
			FileOutputStream fos 		= new FileOutputStream(zFile, false);
			
			//256K buffer
			BufferedOutputStream bos 	= new BufferedOutputStream(fos, 65536);
			
			DataOutputStream fdos 		= new DataOutputStream(bos);
			
			//And write it..
			zObject.writeDataStream(fdos);
			
			//flush
			fdos.flush();
			bos.flush();
			fos.flush();
			
			fdos.close();
			bos.close();
			fos.close();
			
		}catch(IOException exc) {
			MinimaLogger.log(exc);
		}
	}
	
	public static void saveObjectEncrypted(String zPassword, File zFile, Streamable zObject) {
		try {
			//Write into byte array
			MiniData casc = MiniData.getMiniDataVersion(zObject);
			
			//Convert to an encrypted object
			MiniData encrypted = PasswordCrypto.encryptPassword(zPassword, casc);
			
			//save to disk
			MiniFile.writeDataToFile(zFile, encrypted.getBytes());
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
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
		boolean del = true;
		if(mParentCheck.equals("")) {
			del = zFile.delete();
		}else if(zFile.getAbsolutePath().startsWith(mParentCheck)) {
			del = zFile.delete();
		}else {
			MinimaLogger.log("Attempt to delete File NOT child of parent check "+zFile.getAbsolutePath()+" / "+mParentCheck);
		}
		
		//Did it work..
		if(!del) {
			MinimaLogger.log("ERROR deleting file "+zFile.getAbsolutePath());
		}
	}
	
	public static long getTotalFileSize(File zFolder) {
		
		long tot = 0;
		
		File[] files = zFolder.listFiles();
		if(files == null) {
			return 0;
		}
		
		for(File file : files) {
			if(file.isDirectory()) {
				tot = tot + getTotalFileSize(file);
			}else {
				tot += file.length();
			}
		}
		
		return tot;
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
	
	//Check if childCandidate is child of path
	public static boolean isChild(File zParent, File zChild) {
		Path pparent  	= Paths.get(zParent.getAbsolutePath()).normalize();
		Path pfile 		= Paths.get(zChild.getAbsolutePath()).normalize();
		
		return pfile.toFile().getAbsolutePath().startsWith(pparent.toFile().getAbsolutePath());
	}
	
	public static byte[] readAllBytes(InputStream inputStream) throws IOException {
	    final int bufLen 	= 1024;
	    byte[] buf 			= new byte[bufLen];
	    int readLen;
	    
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        while ((readLen = inputStream.read(buf, 0, bufLen)) != -1) {
        	outputStream.write(buf, 0, readLen);
        }
            
        return outputStream.toByteArray();
	}
	
	public static void decompressGzipFile(File gzipFile, File newFile) {
        try {
            FileInputStream fis 	= new FileInputStream(gzipFile);
            GZIPInputStream gis 	= new GZIPInputStream(fis);
            FileOutputStream fos 	= new FileOutputStream(newFile);
            byte[] buffer = new byte[1024];
            int len;
            while((len = gis.read(buffer)) != -1){
                fos.write(buffer, 0, len);
            }
            //close resources
            fos.close();
            gis.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
    }

	public static void compressGzipFile(File file, File gzipFile) {
        try {
            FileInputStream fis 	= new FileInputStream(file);
            FileOutputStream fos 	= new FileOutputStream(gzipFile);
            GZIPOutputStream gzipOS = new GZIPOutputStream(fos);
            byte[] buffer = new byte[1024];
            int len;
            while((len=fis.read(buffer)) != -1){
                gzipOS.write(buffer, 0, len);
            }
            //close resources
            gzipOS.close();
            fos.close();
            fis.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
    }
}
