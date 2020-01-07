package org.minima.miniscript.functions.base;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.tokens.Token;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;

/**
 * Search for a variable assignment in a Script and replace it's value.
 * @author spartacusrex
 *
 */
public class RPLVAR extends MinimaFunction {

	public RPLVAR() {
		super("RPLVAR");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the script..
		ScriptValue script = (ScriptValue) getParameter(0).getValue(zContract);
		String ss = script.toString();
		
		//Get the variable name
		ScriptValue var    = (ScriptValue) getParameter(1).getValue(zContract);
		
		//Get the expression
		ScriptValue exp    = (ScriptValue) getParameter(2).getValue(zContract);
				
		//Now replace.. 
		String search = "LET "+var+" = ";
		int len       = search.length();
		int index     = ss.indexOf(search);
		
		//Not Found..
		if(index == -1) {
			return new ScriptValue(ss);
		}
		
		//Otherwise..
		int start = index+len;
		
		//Now find the end..
		int end = script.toString().length();
		for(String command : Token.TOKENS_COMMAND) {
			int comm = ss.indexOf(command,start);
			if(comm != -1) {
				if(comm < end) {
					end = comm;
				}
			}
		}
		
		//Now replace that section..
		String ret = ss.substring(0,start);
		ret += " "+exp.toString()+" ";
		ret += ss.substring(end);
		
		return new ScriptValue(ret);
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new RPLVAR();
	}
}
