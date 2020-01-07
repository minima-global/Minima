package org.minima.utils;

public class MiniFormat {

	public static String PrettyJSON(String zJSON) {
		//The Copy
		String ret = "";
		
		//How long
		int len  = zJSON.length();
		int tabs = 0;
		String tabstring = maketabstring(tabs);
		
		boolean inquotes = false;
		for(int i=0;i<len;i++) {
			char cc = zJSON.charAt(i);
	
			//Are we in quotes..
			if(cc == '"') {
				inquotes = !inquotes;
			}
			
			if(!inquotes) {
				if(cc == '{') {
					tabs++;
					tabstring = maketabstring(tabs);
					
					ret+= "{\n";
					ret += tabstring;
					
				}else if(cc == '}') {
					tabs--;
					tabstring = maketabstring(tabs);
					
					ret += "\n";
					ret += tabstring;
					ret += "}";
				
					
				}else if(cc == ',') {
					ret+= ",\n";
					ret += tabstring;
					
				}else {
					ret+= cc;
				}
			}else {
				ret+= cc;
			}
		}
		
		return ret;
	}
	
	private static String maketabstring(int zNum) {
		String ret = "";
		for(int i=0;i<zNum;i++) {
			ret+="  ";
		}
		return ret;
	}
	
	
	public static void main(String[] zArgs) {
		
		String test = "{\"version\":0.4,\"milliuptime\":2450,\"stringuptime\":\"0 Years 0 Months 0 Weeks 0 Days 0 Hours 0 Minutes 2 Seconds 450 Milliseconds\",\"conf\":\"\\/home\\/spartacusrex\\/minima\\/minima\",\"host\":\"0.0.0.0\",\"port\":9001,\"pulse\":true,\"root\":{\"block\":0,\"isblock\":true,\"txpowid\":\"0x8C73A2FF132C3242E83FA2F6AD389884A3170C0D5AE283BDEC89DD08151DD8E8\",\"parent\":\"0x0000000000000000000000000000000000000000000000000000000000000000\",\"blkdiff\":0,\"txndiff\":0,\"txn\":{\"inputs\":[],\"outputs\":[]},\"witness\":params[] pubk[] scripts[],\"txnlist\":[],\"nonce\":256,\"mmr\":\"0x7094AA8139BFBCC37CA832CADD5BB49ECCB2C54C6C1C6FC8DBAB354BA052BF66\",\"timemilli\":1573815008746},\"tip\":{\"block\":7,\"isblock\":true,\"txpowid\":\"0x188CF963491A7ED735CC60217C29598F86631503F4A7BB228AE9CAA3C85EF2EA\",\"parent\":\"0x8F50F40A85EABC440C10AB0781C39C41B3F44DC1A793C176AC4ECEB80F680AFE\",\"blkdiff\":0,\"txndiff\":0,\"txn\":{\"inputs\":[],\"outputs\":[]},\"witness\":params[] pubk[] scripts[],\"txnlist\":[],\"nonce\":-2804983266292932580,\"mmr\":\"0x7094AA8139BFBCC37CA832CADD5BB49ECCB2C54C6C1C6FC8DBAB354BA052BF66\",\"timemilli\":1573815011101},\"chainspeed\":2.9723991507431,\"lastblock\":7,\"totalpow\":8}";
		
		String pretty = PrettyJSON(test);
		
		System.out.println(pretty);
	}
}
