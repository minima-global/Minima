package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;

public class HORSTUtils {

	public static MiniData shrinkData(int zBytes, MiniData zOrig) {
		
		byte[] orig = zOrig.getBytes();
		byte[] res 	= new byte[zBytes];
		for(int i=0;i<zBytes;i++) {
			res[i] = orig[i];
		}
		
		return new MiniData(res);
	}
	
	public static int getKeyRef(int zPos, MiniData zOrig) {
		byte[] allbytes	= zOrig.getBytes();
		int val 		= allbytes[zPos] & 0xFF;
		return val;
	}
	
}
