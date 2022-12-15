package org.minima.utils;

import java.io.ByteArrayOutputStream;
import java.util.Arrays;
import java.util.Objects;

public class FastByteArrayOutputStream extends ByteArrayOutputStream {
	
	/**
	 * 256k initial size + 256K when needed
	 */
	public static final int SIZE_INCREASE = 256 * 1024;
	
	public FastByteArrayOutputStream() {
		super(SIZE_INCREASE);
	}
	
	public void writeData(byte b[], int off, int len) {
        
		//Check valid
		Objects.checkFromIndexSize(off, len, b.length);
        
		//What is the current size
		int csize = buf.length;
		
		//What is the new size
		int required = count + len;
		if(required>=csize) {
			
			//How much bigger to make it..
			int increase = SIZE_INCREASE;
			if(len > increase) {
				increase = len;
			}
			
			//Make it bigger
			buf = Arrays.copyOf(buf, csize+increase);
		}
		
		//Now copy data
        System.arraycopy(b, off, buf, count, len);
        
        //Increment counter
        count += len;
    }
}
