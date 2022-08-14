package org.minima.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class ZipExtractor {

	public static void unzip(InputStream archive, File zDestPath) throws IOException {
        //MinimaLogger.log("unzipping package into " + zDestPath.getAbsolutePath());
        byte[] buffer = new byte[1024];
        ZipInputStream zis = new ZipInputStream(archive);
        ZipEntry zipEntry = zis.getNextEntry();
        // TODO validate
        // TODO we are looking for a package structure that maps to what we would expect including in dapp.yml references
        // pass in validation logic as an argument
        
        while (zipEntry != null) {
            File newFile = newFile(zDestPath, zipEntry);
            if (zipEntry.isDirectory()) {
                if (!newFile.isDirectory() && !newFile.mkdirs()) {
                    throw new IOException("Failed to create directory " + newFile);
                }
            } else {
                // fix for Windows-created archives
                File parent = newFile.getParentFile();
                if (!parent.isDirectory() && !parent.mkdirs()) {
                    throw new IOException("Failed to create directory " + parent);
                }
                
                // write file content
                FileOutputStream fos = new FileOutputStream(newFile);
                int len;
                while ((len = zis.read(buffer)) > 0) {
                    fos.write(buffer, 0, len);
                }
                fos.close();
            }
        zipEntry = zis.getNextEntry();
       }
        zis.closeEntry();
        zis.close();
    }
    
    private static File newFile(File destinationDir, ZipEntry zipEntry) throws IOException {
        File destFile = new File(destinationDir, zipEntry.getName());
        if (destFile.exists()) {
        	MinimaLogger.log("processing zip entry " + destinationDir.toString() + " but it already exists");
        }
    
        String destDirPath = destinationDir.getCanonicalPath();
        String destFilePath = destFile.getCanonicalPath();
    
        if (!destFilePath.startsWith(destDirPath + File.separator)) {
            throw new IOException("Entry is outside of the target dir: " + zipEntry.getName());
        }
    
        return destFile;
    }
	
}
