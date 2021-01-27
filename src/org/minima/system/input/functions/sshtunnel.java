package org.minima.system.input.functions;

import org.minima.system.input.CommandFunction;
import org.minima.system.network.maxima.Maxima;
import org.minima.system.network.sshtunnel.SSHTunnel;
import org.minima.utils.messages.Message;

public class sshtunnel extends CommandFunction{

	public sshtunnel() {
		super("sshtunnel");
		
		setHelp("[start|stop|info|params]", "Create an ssh tunnel to an ssh server for an external Maxima IP", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Create a message
		Message ssh = null;
		
		//The details
		String func = zInput[1];
		
		int len = zInput.length;
		
		//The extra data
		String username     = "";
		String password     = "";
		String host         = "";
		int remotep         = -1;
		
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
				remotep =  Integer.parseInt(param.substring(11).trim());
			}
		}
		
		//Is this more than info or new
		if(func.equals("start")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_START);
			
		}else if(func.equals("stop")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_STOP);
			
		}else if(func.equals("info")) {
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_INFO);
			
		}else if(func.equals("params")) {
			//Check that all values were specified
			if(username.equals("") || password.equals("") || host.equals("") || remotep == -1) {
				getResponseStream().endStatus(false, "Incorrect parameters. MUST specify all.");
				return;
			}
			
			ssh = getResponseMessage(SSHTunnel.SSHTUNNEL_PARAMS);
			ssh.addString("username", username);
			ssh.addString("password", password);
			ssh.addString("host", host);
			ssh.addInteger("remoteport", remotep);
		}
		
		//Send it to the miner..
		getMainHandler().getNetworkHandler().getSSHTunnel().PostMessage(ssh);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		return new sshtunnel();
	}
}
