package org.minima.miniscript.tokens;

import java.util.Hashtable;
import java.util.StringTokenizer;

import org.minima.miniscript.Contract;
import org.minima.objects.base.MiniData32;
import org.minima.utils.Crypto;

public class MASTRecurse {
	
	
	public static MiniData32 MAST(String zRamScript, Hashtable<MiniData32, String> zTable) {
		//Clean it..
		String script = Contract.cleanScript(zRamScript);
				
		System.out.println("MAST : *"+script+"*");
		
		//Find the first instance of MAST
		int mindex = script.indexOf("MAST ");
		
		//If no MAST return
		if(mindex == -1) {
			//Hash It..
			byte[] hdata = Crypto.getInstance().hashData(script.getBytes());
			MiniData32 hsh = new MiniData32(hdata);
			
			//That is the script code
			zTable.put(hsh, script);
			
			return hsh;
		}
		
		//If there is a mast.. recurse ..
		String ender = "MASTEND";
		int mindexend = script.indexOf(ender, mindex);

		//Now get that section..
		String internal = script.substring(mindex, mindexend+ender.length());
		
		//Now recurse that..
		System.out.println("Internal : *"+internal+"*");
		
		
		return null;
	}
	
	
	public String getInternalMAST(String zRamScript) {
		
		StringTokenizer strtok = new StringTokenizer(zRamScript," ");
		while(strtok.hasMoreElements()) {
			String tok = strtok.nextToken();
			
//			if()
			
		}
		
		
		return "";
	}
	
	
	
	public static void main(String[] zArgs) {
		
		Hashtable<MiniData32, String> mast = new Hashtable<MiniData32, String>();
		
		String RamScript = "let t = 1 MASt let y=t+1 MASTor let y=t+2 mastend if y GT 3 then return false endif return true";
		
		MAST(RamScript, mast);
		
	}
}
