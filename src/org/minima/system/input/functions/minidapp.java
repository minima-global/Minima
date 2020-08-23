package org.minima.system.input.functions;

import java.io.File;

import org.minima.objects.base.MiniData;
import org.minima.system.brains.ConsensusPrint;
import org.minima.system.input.CommandFunction;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.utils.MiniFile;
import org.minima.utils.messages.Message;

public class minidapp extends CommandFunction {

	public minidapp() {
		super("minidapp");
		
		setHelp("(install:file|uninstall:UID|search:name|post:UID message|list)", "Install, uninstall, search, post a message directly or list the MiniDAPPs on your system.", "");
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
				installmsg.addObject("filename", ff.getName());
				installmsg.addObject("minidapp", minidapp);
				getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(installmsg);
		           
				return;
				
			}else if(zInput[1].startsWith("uninstall:")) {
				String minidapp = zInput[1].substring(10);
				
				//And Post it..
				Message uninstall = getResponseMessage(DAPPManager.DAPP_UNINSTALL);
				uninstall.addString("minidapp", minidapp);
				getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(uninstall);
		           
				return;
				
				
			}else if(zInput[1].startsWith("search:")) {
				String name = zInput[1].substring(7);
				
				msg.addString("action", "search");
				msg.addString("name", name);
				
				/**
				 * Post A JSON message to the MiniDAPP!
				 */
			}else if(zInput[1].startsWith("post:")) {
				String minidapp = zInput[1].substring(5);
				String message  = zInput[2];
				
				//Now post it..
				Message post = getResponseMessage(DAPPManager.DAPP_DIRECTPOST);
				post.addString("minidapp", minidapp);
				post.addString("message", message);
				getMainHandler().getNetworkHandler().getDAPPManager().PostMessage(post);
		        
				return;
				
			}else if(zInput[1].equals("list")) {
				msg.addString("action", "list");	
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
		return new minidapp();
	}
}
