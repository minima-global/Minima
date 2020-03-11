package org.minima.system.input;

import org.minima.system.Main;
import org.minima.system.SystemHandler;
import org.minima.system.input.functions.intro;
import org.minima.utils.ResponseStream;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class InputHandler extends SystemHandler{

	/**
	 * Message type for input messages
	 */
	public static final String INPUT_COMMAND = "_**input_command**_";
	
	/**
	 * The desired function
	 */
	public static final String INPUT_FUNCTION = "_**input_function**_";
	
	/**
	 * The output response
	 */
	public static final String INPUT_RESPONSE = "_**outstream**_";
	
	
	/**
	 * Main Constructor
	 * 
	 * @param zMain
	 */
	public InputHandler(Main zMain) {
		super(zMain,"INPUT");
		
		//And run the intro..
		new intro().doFunction(new String[0]);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(INPUT_COMMAND)) {
			//Process the input
			String input = zMessage.getString(INPUT_FUNCTION);
			
			//Get the response Stream
			ResponseStream output = (ResponseStream) zMessage.getObject(INPUT_RESPONSE);
			
			//Remember the function
			output.setFunction(input);
			
			//Split the input up into an array string..
			String[] inputs = InputMessage.splitString(input);
			
			//Cycle through the functions
			CommandFunction found = CommandFunction.getFunction(inputs[0]);
		
			//Function performed..
			if(found==null) {
				//No function..
				output.endStatus(false, "Function not found : "+inputs[0]);
				
			}else {
				//Do it!
				try {
					//Set the Response Stream for this function..
					found.setResponseStream(output);
					
					//Set the Main handler..
					found.setMainHandler(getMainHandler());
					
					//Do it..
					found.doFunction(inputs);	
					
				}catch(Exception exc) {
					//Write the HELP for that function..
					output.getDataJSON().put("help", found.getName()+" "+ found.getParams()+ " - " + found.getSimple());
					
					//Write error
					output.endStatus(false,exc.toString());
				}
			}

		}
	}

//	/**
//	 * Return a message with the correct output response set
//	 *  
//	 * @param zOriginal
//	 * @return
//	 */
//	public static Message newResponseMesage(String zMessageType, Message zOriginal) {
//		//Create a new Message
//		Message output = new Message(zMessageType);
//		
//		//Check is valid
//		if(zOriginal.exists(InputHandler.INPUT_RESPONSE)) {
//			output.addObject(InputHandler.INPUT_RESPONSE, zOriginal.getObject(InputHandler.INPUT_RESPONSE));	
//		}
//		
//		return output;
//	}
	
	public static Message addResponseMesage(Message zNewMessage, Message zOriginal) {
		//Check is valid
		if(zOriginal.exists(InputHandler.INPUT_RESPONSE)) {
			zNewMessage.addObject(InputHandler.INPUT_RESPONSE, zOriginal.getObject(InputHandler.INPUT_RESPONSE));	
		}
		
		return zNewMessage;
	}
	
	public static JSONObject getResponseJSON(Message zMessage) {
		if(zMessage.exists(InputHandler.INPUT_RESPONSE)) {
			ResponseStream out = (ResponseStream)zMessage.getObject(InputHandler.INPUT_RESPONSE);
			return out.getDataJSON();
		}else {
			//If None then just return a fake one.. 
			return new JSONObject();	
		}
	}
	
	public static void endResponse(Message zMessage, boolean zValid, String zStatusMessage) {
		if(zMessage.exists(InputHandler.INPUT_RESPONSE)) {
			//Get the output response
			ResponseStream out = (ResponseStream)zMessage.getObject(InputHandler.INPUT_RESPONSE);
			
			//Set the details
			out.endStatus(zValid, zStatusMessage);
		}
	}
}
