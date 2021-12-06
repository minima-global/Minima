package org.minima.kissvm.functions.sha;

import java.io.IOException;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMRProof;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.HexValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;

public class PROOF extends MinimaFunction {

	public PROOF() {
		super("PROOF");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get the initial data
		MiniData data = zContract.getHexParam(0, this).getMiniData();

		//Create an MMRData object - 0 value..
		MMRData mmrdata = new MMRData(data);
		
		//Get the proof chain 
		HexValue chain = zContract.getHexParam(1, this);
		
		//Create into the MMRProof..
		MMRProof proof = null;
		try {
			proof = MMRProof.convertMiniDataVersion(chain.getMiniData());
		} catch (IOException e) {
			//Invalid Proof..
			throw new ExecutionException("Invalid MMRProof at PROOF "+chain.getMiniData().to0xString());
		}
		
		//And calculate the final chain value..
		MMRData root = proof.calculateProof(mmrdata); 
				
		//Return..
		return new HexValue(root.getData());
	}
	
	@Override
	public int requiredParams() {
		return 2;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new PROOF();
	}
}
