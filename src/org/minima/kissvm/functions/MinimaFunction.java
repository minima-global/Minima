/**
 * 
 */
package org.minima.kissvm.functions;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.base.GET;
import org.minima.kissvm.functions.base.HEXCAT;
import org.minima.kissvm.functions.base.LEN;
import org.minima.kissvm.functions.base.REV;
import org.minima.kissvm.functions.base.RPLVAR;
import org.minima.kissvm.functions.base.STRCAT;
import org.minima.kissvm.functions.base.SUBSET;
import org.minima.kissvm.functions.cast.ASCII;
import org.minima.kissvm.functions.cast.BOOL;
import org.minima.kissvm.functions.cast.HEX;
import org.minima.kissvm.functions.cast.NUMBER;
import org.minima.kissvm.functions.cast.SCRIPT;
import org.minima.kissvm.functions.maths.ABS;
import org.minima.kissvm.functions.maths.BITGET;
import org.minima.kissvm.functions.maths.BITSET;
import org.minima.kissvm.functions.maths.CEIL;
import org.minima.kissvm.functions.maths.DEC;
import org.minima.kissvm.functions.maths.FLOOR;
import org.minima.kissvm.functions.maths.INC;
import org.minima.kissvm.functions.maths.MAX;
import org.minima.kissvm.functions.maths.MIN;
import org.minima.kissvm.functions.maths.POW;
import org.minima.kissvm.functions.maths.SIGDIG;
import org.minima.kissvm.functions.sha.CHAINSHA;
import org.minima.kissvm.functions.sha.SHA2;
import org.minima.kissvm.functions.sha.SHA3;
import org.minima.kissvm.functions.sigs.CHECKSIG;
import org.minima.kissvm.functions.sigs.MULTISIG;
import org.minima.kissvm.functions.sigs.SIGNEDBY;
import org.minima.kissvm.functions.state.DYNSTATE;
import org.minima.kissvm.functions.state.PREVSTATE;
import org.minima.kissvm.functions.state.SAMESTATE;
import org.minima.kissvm.functions.state.STATE;
import org.minima.kissvm.functions.txn.output.GETOUTADDR;
import org.minima.kissvm.functions.txn.output.GETOUTAMT;
import org.minima.kissvm.functions.txn.output.GETOUTTOK;
import org.minima.kissvm.functions.txn.output.VERIFYOUT;
import org.minima.kissvm.values.Value;

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
				new STRCAT(), new HEXCAT(), new LEN(), new RPLVAR(),new REV(),new SUBSET(), new GET(),
				new BOOL(), new NUMBER(), new HEX(), new SCRIPT(), new ASCII(),
				new ABS(), new CEIL(), new FLOOR(),new MAX(), new MIN(), new DEC(), new INC(), 
				new SIGDIG(), new POW(),
				new SHA3(), new SHA2(), new CHAINSHA(), new BITSET(), new BITGET(),
				new SIGNEDBY(), new MULTISIG(), new CHECKSIG(),
				new GETOUTADDR(), new GETOUTAMT(), new GETOUTTOK(),new VERIFYOUT(),
				new STATE(), new PREVSTATE(), new SAMESTATE(), new DYNSTATE()
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
