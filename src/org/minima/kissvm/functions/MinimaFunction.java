/**
 * 
 */
package org.minima.kissvm.functions;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.functions.cast.BOOL;
import org.minima.kissvm.functions.cast.HEX;
import org.minima.kissvm.functions.cast.NUMBER;
import org.minima.kissvm.functions.cast.STRING;
import org.minima.kissvm.functions.general.GET;
import org.minima.kissvm.functions.hex.BITCOUNT;
import org.minima.kissvm.functions.hex.BITGET;
import org.minima.kissvm.functions.hex.BITSET;
import org.minima.kissvm.functions.hex.CONCAT;
import org.minima.kissvm.functions.hex.LEN;
import org.minima.kissvm.functions.hex.REV;
import org.minima.kissvm.functions.hex.SUBSET;
import org.minima.kissvm.functions.number.ABS;
import org.minima.kissvm.functions.number.CEIL;
import org.minima.kissvm.functions.number.DEC;
import org.minima.kissvm.functions.number.FLOOR;
import org.minima.kissvm.functions.number.INC;
import org.minima.kissvm.functions.number.MAX;
import org.minima.kissvm.functions.number.MIN;
import org.minima.kissvm.functions.number.POW;
import org.minima.kissvm.functions.number.SIGDIG;
import org.minima.kissvm.functions.sha.KECCAK;
import org.minima.kissvm.functions.sha.PROOF;
import org.minima.kissvm.functions.sha.SHA2;
import org.minima.kissvm.functions.sigs.CHECKSIG;
import org.minima.kissvm.functions.sigs.MULTISIG;
import org.minima.kissvm.functions.sigs.SIGNEDBY;
import org.minima.kissvm.functions.state.PREVSTATE;
import org.minima.kissvm.functions.state.SAMESTATE;
import org.minima.kissvm.functions.state.STATE;
import org.minima.kissvm.functions.string.REPLACE;
import org.minima.kissvm.functions.string.SUBSTR;
import org.minima.kissvm.functions.string.UTF8;
import org.minima.kissvm.functions.txn.input.GETINADDR;
import org.minima.kissvm.functions.txn.input.GETINAMT;
import org.minima.kissvm.functions.txn.input.GETINID;
import org.minima.kissvm.functions.txn.input.GETINTOK;
import org.minima.kissvm.functions.txn.input.VERIFYIN;
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
				new CONCAT(), new LEN(), new REV(),new SUBSET(), new GET(),
				new BOOL(), new HEX(), new NUMBER(), new STRING(),
				new ABS(), new CEIL(), new FLOOR(),new MAX(), new MIN(), new DEC(), new INC(), 
				new SIGDIG(), new POW(), 
				new REPLACE(), new SUBSTR(), new UTF8(),
				new KECCAK(), new SHA2(), new PROOF(), new BITSET(), new BITGET(), new BITCOUNT(),
				new SIGNEDBY(), new MULTISIG(), new CHECKSIG(),
				new GETINADDR(), new GETINAMT(), new GETINID(), new GETINTOK(),new VERIFYIN(),
				new GETOUTADDR(), new GETOUTAMT(), new GETOUTTOK(),new VERIFYOUT(),
				new STATE(), new PREVSTATE(), new SAMESTATE()
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
	 * Check all parameters are of the Type required
	 * 
	 * @param zType
	 * @param zContract
	 * @param zParams
	 * @throws ExecutionException 
	 */
	protected void checkAllParamsType(int zType,Contract zContract) throws ExecutionException {
		int count=0;
		for(Expression exp : getAllParameters()) {
			Value vv = exp.getValue(zContract);
			if(vv.getValueType() != zType) {
				throw new ExecutionException("Incorrect type in parameters @ "+count
						+". Found "+Value.getValueTypeString(vv.getValueType())
						+" expected "+Value.getValueTypeString(zType));
			}
			count++;
		}
	}
	
	protected void checkIsOfType(Value zValue, int zType) throws ExecutionException {
		if((zValue.getValueType() & zType) == 0) {
			throw new ExecutionException("Parameter is incorrect type in "+getName()+". Found "+Value.getValueTypeString(zValue.getValueType()));
		}
	}
	
	protected void checkExactParamNumber(int zNumberOfParams) throws ExecutionException {
		if(getAllParameters().size() != zNumberOfParams) {
			throw new ExecutionException("Function requires "+zNumberOfParams+" parameters");
		}
	}
	
	protected void checkMinParamNumber(int zMinNumberOfParams) throws ExecutionException {
		if(getAllParameters().size() < zMinNumberOfParams) {
			throw new ExecutionException("Function requires minimum of "+zMinNumberOfParams+" parameters");
		}
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
	 * How many Parameters do you expect
	 */
	public abstract int requiredParams();
	
	/**
	 * Can be overridden in calsses that set a minimum
	 * @return
	 */
	public boolean isRequiredMinimumParameterNumber() {
		return false;
	}
	
	/**
	 * External function to do a quick check
	 */
	public void checkParamNumberCorrect() throws MinimaParseException {
		int paramsize = getAllParameters().size();
		int reqparam  = requiredParams();
		
		if(isRequiredMinimumParameterNumber()) {
			if(paramsize < reqparam) {
				throw new MinimaParseException(getName()+" function requires a  minimum of "+reqparam+" parameters not "+paramsize);
			}
		}else {
			if(paramsize != reqparam) {
				throw new MinimaParseException(getName()+" function requires exactly "+reqparam+" parameters not "+paramsize);
			}
		}
	}
	
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
