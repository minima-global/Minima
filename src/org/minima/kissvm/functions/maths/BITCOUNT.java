package org.minima.kissvm.functions.maths;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniData;

/**
 * Count the number of bits set in a HEX value..
 * @author spartacusrex
 *
 */
public class BITCOUNT extends MinimaFunction {

	public static final int[] BITSPERBYTE = {0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8};
	
	/**
	 * @param zName
	 */
	public BITCOUNT() {
		super("BITCOUNT");
	}
	
	/* (non-Javadoc)
	 * @see org.ramcash.ramscript.functions.Function#runFunction()
	 */
	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		//get the Input Data
		byte[] data = getParameter(0).getValue(zContract).getRawData();
		
		//How many Bits are set..
		int bits = totalBits(data);
		
		//return the New HEXValue
		return new NumberValue(bits);
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new BITCOUNT();
	}
	
	public static int totalBits(byte[] zData) {
		int total = 0;
		for(int i=0;i<zData.length;i++) {
			total+=BITSPERBYTE[zData[i] & 0xFF]; 
		}
		return total;
	}
	
	public static void main(String[] zArgs) {
		
		byte[] values = new byte[256];
		
		System.out.print("{");
				
		for(int i=0;i<256;i++) {
			values[i] = (byte)i;
		
			int tot = 0;
			//Count bits..
			for(int loop=0;loop<8;loop++) {
				int testval = (int) Math.pow(2, loop);
				if( (values[i] & testval) == testval) {
					tot+=1;
				}	
			}
			
			if(i<255) {
				System.out.print(tot+",");	
			}else {
				System.out.print(tot+"}");
			}
		}
		
		System.out.println();
		
		//Calculate..
		MiniData tester = new MiniData("0x0000000001");
		System.out.println(tester.to0xString()+" "+BITCOUNT.totalBits(tester.getData()));
		
		tester = new MiniData("0xFFFF");
		System.out.println(tester.to0xString()+" "+BITCOUNT.totalBits(tester.getData()));
		
		tester = new MiniData("0x0301");
		System.out.println(tester.to0xString()+" "+BITCOUNT.totalBits(tester.getData()));
		
	}
}
