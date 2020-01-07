package org.minima.miniscript.functions.sha;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;

import org.minima.miniscript.Contract;
import org.minima.miniscript.exceptions.ExecutionException;
import org.minima.miniscript.functions.MinimaFunction;
import org.minima.miniscript.functions.cast.HEX;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData32;
import org.minima.utils.Crypto;

public class CHAINSHA extends MinimaFunction {

	public CHAINSHA() {
		super("CHAINSHA");
	}
	
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//Get the input hash...
		HEXValue input = (HEXValue) getParameter(0).getValue(zContract);
		
		//Get the 32 byte hash data chain + 1 byte for left right 
		HEXValue chain = (HEXValue) getParameter(1).getValue(zContract);
		
		//Now cycle through..
		byte[] indata = input.getRawData();
		byte[] chdata = chain.getRawData();
		
		//indata must be 32 bytes long
		if(indata.length != 32) {
			throw new ExecutionException("Input data must be 32 bytes long.");
		}
		//Chdata MUST be a multiple of 32
		if(chdata.length % 33 != 0) {
			throw new ExecutionException("Chain data not a multiple of 33 bytes long.");
		}
		
		int loop = chdata.length / 33;
		
		//The running total
		MiniData32 total = new MiniData32(indata);
		
		ByteArrayInputStream bais = new ByteArrayInputStream(chdata);
		DataInputStream dis = new DataInputStream(bais);
		
		for(int i=0;i<loop;i++) {
			//Is it to the left or the right 
			MiniByte leftrigt = MiniByte.ReadFromStream(dis);
			
			//What data to hash
			MiniData32 data = MiniData32.ReadFromStream(dis);
		
			//Do it!
			if(leftrigt.isTrue()) {
				total = Crypto.getInstance().hashObjects(data, total);	
			}else {
				total = Crypto.getInstance().hashObjects(total, data);
			}
		}
		
		//Clean up
		try {dis.close();} catch (IOException e) {
			throw new ExecutionException("Strange IO Exception at CHAINSHA !? "+e);
		}
		
		//Return..
		return new HEXValue(total.getData());
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CHAINSHA();
	}
}
