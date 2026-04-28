package org.minima.utils.sphincs;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.TreeKey;

public class SPHINCS {

	TreeKey mTreeKey;
	
	public SPHINCS(MiniData zSeed) {
		
		//The XMSS hash tree
		mTreeKey = new TreeKey(zSeed, 16, 3);
	}
	
}
