package org.minima.system.input;

import java.util.ArrayList;

import org.minima.utils.ResponseStream;
import org.minima.utils.MinimaLogger;
import org.minima.utils.messages.Message;

/**
 * A standard user input message
 * 
 * @author spartacus
 *
 */
public class InputMessage extends Message{
	
	/**
	 * Main Constructor
	 * 	
	 * @param zInput
	 * @param zResponseStream
	 */
	public InputMessage(String zInput, ResponseStream zResponseStream) {
		super(InputHandler.INPUT_COMMAND);
		
		//Add the Input Message
		addObject(InputHandler.INPUT_FUNCTION, zInput.trim());
		
		//Where do we send the response..
		addObject(InputHandler.INPUT_RESPONSE, zResponseStream);
	}
	
	/**
	 * Split the input string keeping quoted sections as single units
	 * 
	 * @param zString
	 * @return
	 */
	public static String[] splitString(String zInput) {
		ArrayList<String> token = new ArrayList<>();
		String ss = zInput.trim();
		
		//Cycle through looking for spaces or quotes..
		String current = new String();
		boolean quoted = false;
		int len = ss.length();
		for(int i=0;i<len;i++) {
			char cc = ss.charAt(i);
			
			if(cc == ' ') {
				//End of the line..
				if(!quoted) {
					//Add current
					if(!current.equals("")) {
						token.add(current);
					}
						
					//New Current
					current = new String();
				}else {
					current += cc;
				}
			}else if(cc == '\"') {
				//it's a quote!
				if(quoted) {
					//It's finished..
					quoted=false;
				}else {
					quoted=true;
				}
			}else {
				current += cc;
			}
		}
		
		//Add the last bit..
		if(!current.equals("")) {
			token.add(current);
		}
		
		return token.toArray(new String[0]);
	}
	
	
	public static void main(String[] zArgs) {
		
		//String tester = "let there be \" the light of ages\"";
		String tester = "  send   0   0xff  ";
		
		String[] tt = splitString(tester);
		for(int i=0;i<tt.length;i++) {
			MinimaLogger.log(tt[i]);
		}
		MinimaLogger.log("FINISHED!!");
	}
}