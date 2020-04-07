/**
 * 
 */
package org.minima.kissvm.functions.base;

import java.nio.charset.Charset;
import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;

/**
 * @author Spartacus Rex
 *
 */
public class CONCAT extends MinimaFunction{

	public CONCAT() {
		super("CONCAT");
	}

	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction(org.ramcash.ramscript.Contract)
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		
		//Run through the function parameters and concatenate..
		ArrayList<Expression> params = getAllParameters();
		int paramnum = params.size();
		byte[][] parambytes = new byte[paramnum][];
		
		if(paramnum < 1) {
			throw new ExecutionException("Invalid number of Parameters for CONCAT "+paramnum);
		}
		
		//The first parameter determines if this is a SCRIPT or HEXVALUE
		int type = params.get(0).getValue(zContract).getValueType();
		
		//Check a valid type
		if(type != HEXValue.VALUE_HEX && type != ScriptValue.VALUE_SCRIPT) {
			throw new ExecutionException("Invaid Value Type in CONCAT "+type+") MUST be HEX or SCRIPT "+params.get(0).toString());
		}
			
		//Sum them
		int totlen  = 0;
		int counter = 0;
		for(Expression exp : params) {
			//check the same
			int intype = exp.getValue(zContract).getValueType();
			if(intype != type) {
				throw new ExecutionException("Invaid Value Type in CONCAT "+intype+") MUST all be the same");
			}
			
			//Get the bytes
			parambytes[counter] = exp.getValue(zContract).getRawData();
			totlen += parambytes[counter].length;
			counter++;
		}
		
		//The result is placed in here
		byte[] result     = new byte[totlen];
		String fullstring = "";
		//And sum
		int pos=0;
		for(int i=0;i<counter;i++) {
			if(type == HEXValue.VALUE_HEX) {	
				//Is it RAW data
				System.arraycopy(parambytes[i], 0, result, pos, parambytes[i].length);
				pos += parambytes[i].length;
			}else {
				//Must be a script
				fullstring += new String(parambytes[i], Charset.forName("US-ASCII") )+" ";
			}
		}
		
		if(type == HEXValue.VALUE_HEX) {
			return new HEXValue(result);	
		
		}else if(type == ScriptValue.VALUE_SCRIPT) {
			return new ScriptValue(fullstring);	
		
		}else {
			throw new ExecutionException("Invaid Value Type in CONCAT "+type+") "+params.get(0).toString());
		}
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CONCAT();
	}

}
