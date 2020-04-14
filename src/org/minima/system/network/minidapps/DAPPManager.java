package org.minima.system.network.minidapps;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.utils.Crypto;
import org.minima.utils.messages.Message;

public class DAPPManager extends SystemHandler {

	public static String DAPP_INSTALL = "DAPP_INSTALL";
	
	
	
	public DAPPManager(Main zMain) {
		super(zMain, "DAPPMAnager");
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		if(zMessage.getMessageType().equals(DAPP_INSTALL)) {
			//Get the Data
			MiniData data = (MiniData) zMessage.getObject("minidapp");
			
			//Hash it..
			MiniData hash = Crypto.getInstance().hashObject(data, 160);
			
			//This is the folder..
			String root = getMainHandler().getBackupManager().getRootFolder();
			
			//Create the new Folder...
			File alldapps = new File(root,"minidapps");
			
			//And the actual folder...
			File dapp  = new File(alldapps,hash.to0xString());
			
			//Now extract the contents to that folder..
			byte[] buffer = new byte[2048];
			ByteArrayInputStream bais = new ByteArrayInputStream(data.getData());
			
			BufferedInputStream bis = new BufferedInputStream(bais);
            ZipInputStream stream   = new ZipInputStream(bis);
            
	        ZipEntry entry;
	        
	        while ((entry = stream.getNextEntry()) != null) {
	        	//Where does this file go
	            File filePath = new File(dapp,entry.getName());
	
	            //Check the parent folder exists
	            File parent = filePath.getParentFile();
	            if(!parent.exists()) {
	            	parent.mkdirs();
	            }
	            
	            FileOutputStream fos     = new FileOutputStream(filePath);
	            BufferedOutputStream bos = new BufferedOutputStream(fos, buffer.length);
	            
                int len;
                while ((len = stream.read(buffer)) > 0) {
                    bos.write(buffer, 0, len);
                }
	        }
	        
	        //It's done!
			
		}
		
	}

}
