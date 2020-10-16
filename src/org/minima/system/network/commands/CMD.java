package org.minima.system.network.commands;

import java.util.StringTokenizer;

import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.input.InputMessage;
import org.minima.utils.ResponseStream;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class CMD implements Runnable {

	//The Command to run
	String mCommand;
	
	//The Final Result..
	String mFinalResult = "";
	
	public CMD(String zCommand) {
		mCommand  = zCommand;
	}
	
	public String getFinalResult() {
		return mFinalResult;
	}
	
	@Override
	public void run() {
		//Get the InputHandler
		InputHandler inhandle = Main.getMainHandler().getInputHandler();
		
		//Is this a multi function..
		boolean multi = false;
		if(mCommand.indexOf(";")!=-1) {
			//It's a multi
			multi = true;
		}
		
		if(!multi) {
			//Now make this request
			ResponseStream response = new ResponseStream();
            
			//Make sure valid
			if(!mCommand.equals("")) {
			    //Send it..
				InputMessage inmsg = new InputMessage(mCommand, response);

				//Post it..
				inhandle.PostMessage(inmsg);
				
				//Wait for the function to finish
                response.waitToFinish();
			}
			
			//Get the response..
			mFinalResult = response.getResponse();
			
		}else {
			//A full JSON array of responses
			JSONArray responses = new JSONArray();
			
			//Cycle through each request..	
			StringTokenizer functions = new StringTokenizer(mCommand,";");
			
			boolean allok = true;
			while(allok && functions.hasMoreElements()) {
				String func = functions.nextToken().trim();
			
				//Now make this request
				ResponseStream response = new ResponseStream();
	            
				//Make sure valid
				if(!func.equals("")) {
					//Send it..
					InputMessage inmsg = new InputMessage(func, response);

					//Post it..
					inhandle.PostMessage(inmsg);
					
					//Wait for the function to finish
	                response.waitToFinish();
	                
	                //Get the JSON
	                JSONObject resp = response.getFinalJSON();
	                
	                //IF there is an erorr.. STOP
	                if(resp.get("status") == Boolean.FALSE) {
	                	//ERROR - stop running functions..
	                	allok = false;
	                }
	                
	                //Add it to the array
	                responses.add(resp);
				}
			}
			
			//And now get all the answers in one go..
			mFinalResult = responses.toString();
		}
	}
}
