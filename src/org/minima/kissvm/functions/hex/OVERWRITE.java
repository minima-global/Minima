package org.minima.kissvm.functions.hex;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;

/**
 * Works on Scripts and HEX
 * @author spartacusrex
 *
 */
public class OVERWRITE extends MinimaFunction {

	public OVERWRITE() {
		super("OVERWRITE");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get a a subset of a hex value..
		MiniData src 	= zContract.getHexParam(0, this).getMiniData();
		int srcpos 		= zContract.getNumberParam(1, this).getNumber().getAsInt();
		
		MiniData destorig 	= zContract.getHexParam(2, this).getMiniData();
		MiniData dest 		= new MiniData(destorig.to0xString());
		int destpos 		= zContract.getNumberParam(3, this).getNumber().getAsInt();
		
		int len   			= zContract.getNumberParam(4, this).getNumber().getAsInt();
		
		//Do some checks..
		if(destpos+len > dest.getLength()) {
			throw new ExecutionException("OVERWRITE destination array too short");
		
		}else if( srcpos+len > src.getLength()) {
			throw new ExecutionException("OVERWRITE src array too short");
		
		}else if( len < 0) {
			throw new ExecutionException("Cannot have negative length "+len);
			
		}
			
		
		//Now overwrite the bytes..
		System.arraycopy(src.getBytes(), srcpos, dest.getBytes(), destpos, len);
		
		return new HexValue(dest);	
	}
	
	@Override
	public int requiredParams() {
		return 5;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new OVERWRITE();
	}
}
