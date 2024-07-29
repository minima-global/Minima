package org.minima.utils;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;

public class FastByteArrayStream extends ByteArrayOutputStream {
	
	/**
	 * 256k max initial size + 256K when needed
	 */
	private static final int SIZE_INCREASE = 256 * 1024;
	
	/**
	 * How much do we actually need
	 */
	private static int getRequiredSize(int zTotal) {
		if(zTotal < SIZE_INCREASE) {
			return zTotal;
		}
		return SIZE_INCREASE;
	}
	
	public FastByteArrayStream(int zTotalSize) {
		super(getRequiredSize(zTotalSize));
	}
	
	public void writeData(byte b[], int off, int len) {
        
		//Check valid
		if(off<0) {
			return;
		}
//		Objects.checkFromIndexSize(off, len, b.length);
        
		//What is the current size
		int csize = buf.length;
		
		//What is the new size
		int required = count + len;
		if(required>csize) {
			
			//How much bigger to make it..
			int increase = SIZE_INCREASE;
			if(len > increase) {
				increase = len;
			}
			
			//Make it bigger
			try {
				buf = Arrays.copyOf(buf, csize+increase);
			} catch (OutOfMemoryError e) {
				
				MinimaLogger.log("[!] SERIOUS OUT OF MEMORY ERROR at FastByteArrayStream "
						+ " currentsize:"+csize
						+ " required:"+required
						+ " increase:"+increase
						+ " count:"+count
						+ " len:"+len
						);
				
				//Hmm.. serious error..
				Runtime.getRuntime().halt(0);
			}
		}
		
		//Now copy data
        System.arraycopy(b, off, buf, count, len);
        
        //Increment counter
        count += len;
    }
}
