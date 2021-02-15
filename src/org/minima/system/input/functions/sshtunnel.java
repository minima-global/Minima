package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.sshtunnel.SSHTunnel;
import org.minima.utils.messages.Message;

public class sshtunnel extends CommandFunction{

	public sshtunnel() {
		super("sshtunnel");
		
		setHelp("[start|stop|clear|info|logging [on|off]|params]", 
				"Create an ssh tunnel to an ssh server for an external Maxima IP", 
				"sshtunnel links Minima to an external server running ssh so you get an external IP\n"
				+ "The Parameters you need to set are : \n"
				+ "sshtunnel params username:.. password:.. host:.. remoteport:..\n"
				+ "remoteport and remoteport+1 MUST be open on the server.. they serve as the Minima and Maxima ports.");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message ssh = null;
		
		//The details
		String func = zInput[1];
		
		//Is this more than info or new
		if(func.equals("start")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_START);
			
		}else if(func.equals("stop")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_STOP);
			
		}else if(func.equals("info")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_INFO);
		
		}else if(func.equals("clear")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_CLEAR);
		
		}else if(func.equals("logging")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_LOGGING);
		
			if(zInput[2].equals("on")) {
				ssh.addBoolean("logging", true);
			}else {
				ssh.addBoolean("logging", false);
			}
			
		}else if(func.equals("params")) {
			int len = zInput.length;
			
			//The extra data
			String username     = "";
			String password     = "";
			String host         = "";
			String remotep      = "";
			
			//Cycle through..
			for(int i=1;i<len;i++) {
				String param = zInput[i];
				
				if(param.startsWith("username:")) {
					username = param.substring(9).trim();
				}else if(param.startsWith("password:")) {
					password = param.substring(9).trim();
				}else if(param.startsWith("host:")) {
					host = param.substring(5).trim();
				}else if(param.startsWith("remoteport:")) {
					remotep = param.substring(11).trim();
				}
			}
			
			//Check that all values were specified
			if(username.equals("") || password.equals("") || host.equals("") || remotep.equals("")) {
				getResponseStream().endStatus(false, "Incorrect parameters. MUST specify all.");
				return;
			}
			
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_PARAMS);
			ssh.addString("username", username);
			ssh.addString("password", password);
			ssh.addString("host", host);
			ssh.addString("remoteport", remotep);
		}
		
		//Send it to the miner..
		getMainHandler().getNetworkHandler().getSSHTunnel().PostMessage(ssh);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new sshtunnel();
	}
}
