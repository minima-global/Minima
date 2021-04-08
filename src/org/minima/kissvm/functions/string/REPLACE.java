package org.minima.kissvm.functions.string;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;

public class REPLACE extends MinimaFunction {

	public REPLACE() {
		super("REPLACE");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());

		//Get the the first string
		StringValue strmain   	= zContract.getStringParam(0, this);
		StringValue strsearch 	= zContract.getStringParam(1, this);
		StringValue strrepl 	= zContract.getStringParam(2, this);

		String main 	= strmain.toString();
		String search 	= strsearch.toString();
		String repl 	= strrepl.toString();

		//Now replace..
		String newstr = main.replace(search, repl);

		return new StringValue(newstr);	
	}

	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new REPLACE();
	}
}
