package org.minima.system.input.functions;

import java.io.File;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.utils.MiniFile;
import org.minima.utils.messages.Message;

public class minidapps extends CommandFunction {

	public minidapps() {
		super("minidapps");
		
		setHelp("(install:file|uninstall:UID|search:name|list)", "Install, uninstall, search or list the MiniDAPPs on your system. Defaults to list.", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get the current balance of the user for all tokens..
		Message msg = getResponseMessage(ConsensusPrint.CONSENSUS_MINIDAPPS);
		
		//Can specify to check ONLY a single address..
		if(zInput.length>1) {
			if(zInput[1].startsWith("install:")) {
				String file = zInput[1].substring(8);
				File ff = new File(file);
				
				if(!ff.exists()) {
					getResponseStream().getDataJSON().put("cwd", new File("").getAbsolutePath());
					getResponseStream().endStatus(false, "File not found : "+ff.getAbsolutePath());
					return;
				}
				
				//Load the file..
				byte[] data = MiniFile.readCompleteFile(ff);
				MiniData minidapp = new MiniData(data);
				
				//And Post it..
				Message installmsg = getResponseMessage(DAPPManager.DAPP_INSTALL);
				installmsg.addObject("minidapp", minidapp);
				getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(installmsg);
		           
				return;
				
			}else if(zInput[1].startsWith("uninstall:")) {
			
			}else if(zInput[1].startsWith("search:")) {
			
			}else if(zInput[1].equals("list")) {
				
			}
		}else {
			msg.addString("action", "list");
		}
			
		//Post It..
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}

	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new minidapps();
	}
}
