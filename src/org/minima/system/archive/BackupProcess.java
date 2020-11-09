package org.minima.system.archive;

import java.io.File;

import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class BackupProcess extends MessageProcessor {

	public static final String BACKUPPROC_BLOCK = "BACKUPPROC_BLOCK";
	public static final String BACKUPPROC_TXPOW = "BACKUPPROC_TXPOW";
	
	public File mBlockFolder;
	
	public BackupProcess() {
		super("BACKUP_PROCESS");
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		// TODO Auto-generated method stub
		
	}

	
	
}
