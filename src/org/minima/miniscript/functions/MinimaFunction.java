/**
 * 
 */
package org.minima.miniscript.functions;

import java.util.ArrayList;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.exceptions.MinimaParseException;
import org.minima.miniscript.expressions.Expression;
import org.minima.miniscript.functions.base.CONCAT;
import org.minima.miniscript.functions.base.LEN;
import org.minima.miniscript.functions.base.REV;
import org.minima.miniscript.functions.base.RPLVAR;
import org.minima.miniscript.functions.base.SUBSET;
import org.minima.miniscript.functions.cast.ASCII;
import org.minima.miniscript.functions.cast.BOOL;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.functions.cast.NUMBER;
import org.minima.miniscript.functions.cast.SCRIPT;
import org.minima.miniscript.functions.maths.ABS;
import org.minima.miniscript.functions.maths.BITGET;
import org.minima.miniscript.functions.maths.BITSET;
import org.minima.miniscript.functions.maths.CEIL;
import org.minima.miniscript.functions.maths.DEC;
import org.minima.miniscript.functions.maths.FLOOR;
import org.minima.miniscript.functions.maths.INC;
import org.minima.miniscript.functions.maths.MAX;
import org.minima.miniscript.functions.maths.MIN;
import org.minima.miniscript.functions.sha.ADDR;
import org.minima.miniscript.functions.sha.CHAINSHA;
import org.minima.miniscript.functions.sha.SHA2;
import org.minima.miniscript.functions.sha.SHA3;
import org.minima.miniscript.functions.sigs.CHECKSIG;
import org.minima.miniscript.functions.sigs.MULTISIG;
import org.minima.miniscript.functions.sigs.SIGNEDBY;
import org.minima.miniscript.functions.txn.PREVSTATE;
import org.minima.miniscript.functions.txn.STATE;
import org.minima.miniscript.functions.txn.output.GETOUTADDR;
import org.minima.miniscript.functions.txn.output.GETOUTAMT;
import org.minima.miniscript.functions.txn.output.GETOUTTOK;
import org.minima.miniscript.functions.txn.output.VERIFYOUT;
import org.minima.miniscript.values.Value;
import org.minima.system.input.functions.newaddress;

/**
 * @author Spartacus Rex
 *
 */
public abstract class MinimaFunction {
	
	/**
	 * A list of all the available functions
	 */
	public static MinimaFunction[] ALL_FUNCTIONS = 
			{ 
				new CONCAT(), new LEN(), new RPLVAR(),new REV(),new SUBSET(), 
				new BOOL(), new NUMBER(), new HEX(), new SCRIPT(), new ASCII(),
				new ABS(), new CEIL(), new FLOOR(),new MAX(), new MIN(), new DEC(), new INC(),
				new SHA3(), new SHA2(), new CHAINSHA(), new BITSET(), new BITGET(), new ADDR(), 
				new SIGNEDBY(), new MULTISIG(), new CHECKSIG(),
				new GETOUTADDR(), new GETOUTAMT(), new GETOUTTOK(),new VERIFYOUT(),
				new STATE(), new PREVSTATE()
			};
	
	/**
	 * The name used to refer to this function in RamScript. 
	 */
	private String mName;
	
	/**
	 * The Parameters
	 */
	ArrayList<Expression> mParameters;
	
	/**
	 * 
	 */
	public MinimaFunction(String zName) {
		//Function names are always Uppercase
		mName = zName.toUpperCase();
		
		//Blank the parameters
		mParameters = new ArrayList<>();
	}
	
	public void addParameter(Expression zParam) {
		mParameters.add(zParam);
	}
	
	public Expression getParameter(int zParamNum) throws ExecutionException {
		if(zParamNum>=getParameterNum()) {
			throw new ExecutionException("Parameter missing for "+getName()+" num:"+zParamNum);
		}
		return mParameters.get(zParamNum);
	}
	
	public int getParameterNum(){
		return mParameters.size();
	}
	
	public ArrayList<Expression> getAllParameters(){
		return mParameters;
	}
	
	public String getName() {
		return mName;
	}
	
	/**
	 * Run it. And return a Value. 
	 * @return
	 */
	public abstract Value runFunction(Contract zContract) throws ExecutionException;

	/**
	 * Return a new copy of this function
	 * @return
	 */
	public abstract MinimaFunction getNewFunction();

	/**
	 * Get a specific function given it's name
	 * 
	 * @param zFunction
	 * @return the Function
	 * @throws MinimaParseException
	 */
	public static MinimaFunction getFunction(String zFunction) throws MinimaParseException{
		//Cycle through all the functions - find the right one..
		for(MinimaFunction func : MinimaFunction.ALL_FUNCTIONS) {
			//Check it..
			if(func.getName().equalsIgnoreCase(zFunction)) {
				return func.getNewFunction();
			}
		}
		
		throw new MinimaParseException("Invalid Function : "+zFunction);
	}
	
}
