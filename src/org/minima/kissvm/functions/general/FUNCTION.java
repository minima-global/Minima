package org.minima.kissvm.functions.general;

import java.util.List;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.StatementParser;
import org.minima.kissvm.tokens.ScriptToken;
import org.minima.kissvm.tokens.ScriptTokenizer;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

public class FUNCTION extends MinimaFunction{

	public static String FUNCTION_RETURN = "returnvalue";
	
	public FUNCTION() {
		super("FUNCTION");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkMinParamNumber(requiredParams());
		
		//get the Script..
		StringValue script = zContract.getStringParam(0, this);
		
		//Replace all the $ variables..
		String finalfunction = script.toString();
		int params = getAllParameters().size();
		for(int i=1;i<params;i++) {
			
			//Get the param..
			Value paramval = getParameter(i).getValue(zContract);
			
			//What type is it..
			if(paramval.getValueType() == Value.VALUE_SCRIPT) {
				finalfunction = finalfunction.replace("$"+i, "["+paramval.toString()+"]");
			}else {
				finalfunction = finalfunction.replace("$"+i, paramval.toString());
			}
		}
		
		//Remove any previous return vars..
		zContract.removeVariable(FUNCTION_RETURN);
		
		try {
			//Tokenize the script
			ScriptTokenizer tokz = new ScriptTokenizer(finalfunction);
			
			//Convert the script to KISSVM!
			List<ScriptToken> tokens = tokz.tokenize();	
		
			//And now convert to a statement block..
			StatementBlock mBlock = StatementParser.parseTokens(tokens);
			
			//Now run it..
			mBlock.run(zContract);
		
		}catch(ExecutionException exc) {
			throw exc;
		
		}catch(Exception exc) {
			throw new ExecutionException(exc.toString());			
		}
		
		//Is there a return variable..
		if(zContract.existsVariable(FUNCTION_RETURN)) {
			//Get the return vale..
			return zContract.getVariable(FUNCTION_RETURN);
		}
		
		return new BooleanValue(true);
	}

	@Override
	public boolean isRequiredMinimumParameterNumber() {
		return true;
	}
	
	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new FUNCTION();
	}
}
