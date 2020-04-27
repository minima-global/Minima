package org.minima.utils;

public class BaseConverter {
	
	/**
	 * BASE 16 Encode and Decode
	 */
	private static final char[] HEX16ARRAY = "0123456789ABCDEF".toCharArray();
	
	public static String encode16(byte[] bytes) {
	    char[] hexChars = new char[bytes.length * 2];
	    for ( int j = 0; j < bytes.length; j++ ) {
	        int v = bytes[j] & 0xFF;
	        hexChars[j * 2]     = HEX16ARRAY[v >>> 4];
	        hexChars[j * 2 + 1] = HEX16ARRAY[v & 0x0F];
	    }
	    
	    return "0x"+new String(hexChars);
	}
	
	public static byte[] decode16(String zHex) {
		String hex = zHex;
		if(hex.startsWith("0x")) {
			hex = zHex.substring(2);
		}		
		
		//Go Upper case - make sure always the same
		hex = hex.toUpperCase();
		int len = hex.length();
	
		//Must be 2 digits per byte
		if(len % 2 != 0) {
			//Need a leading zero
			hex="0"+hex;
			len = hex.length();
		}
		
		byte[] data = new byte[len / 2];
	    for (int i = 0; i < len; i += 2) {
	        data[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4) + Character.digit(hex.charAt(i+1), 16));
	    }
	    
	    return data;
	}
	
	/**
	 * BASE 32 Encode and Decode
	 */
	private static final String HEX32ARRAY = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
//	private static final String HEX32ARRAY = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
	
	private static int findChar32(char zChar){
		return HEX32ARRAY.indexOf(zChar);
	}
	
	public static String encode32(byte[] zData) throws ArithmeticException {
		if(zData.length % 5 != 0) {
			throw new ArithmeticException("Minima Address Encode32 data MUST be multiples of 5 in length");
		}
		
		//The final length
		int rounds = zData.length / 5;
		int len    = rounds * 8 ;
		
		//The data32 array
		int[] data32 = new int[len];
		
		//The return string and temp variables
		String hex32 = "";
		int counter=0;
		int currByte, digit;
		
		for(int i=0;i<rounds;i++) {
			int start = i*8;
			
			//1
			currByte      = zData[counter++] & 255;
		    data32[start] = currByte >> 3;
		    digit         = (currByte & 7) << 2;
	
		    //2
		    currByte        = zData[counter++] & 255;
		    data32[start+1] = digit | (currByte >> 6);
		    data32[start+2] = (currByte >> 1) & 31;
		    digit           = (currByte & 1) << 4;
		    
		    //3
		    currByte        = zData[counter++] & 255;
		    data32[start+3] = digit | (currByte >> 4);
		    digit           = (currByte & 15) << 1;
		    
		    //4
		    currByte        = zData[counter++] & 255;
		    data32[start+4] = digit | (currByte >> 7);
		    data32[start+5] = (currByte >> 2) & 31;
		    digit           = (currByte & 3) << 3;
		    
		    //5
		    currByte        = zData[counter++] & 255;
		    data32[start+6] = digit | (currByte >> 5);
		    data32[start+7] = currByte & 31;
		}
		
		//Now add to the result string
	    for(int i=0;i<len;i++) {
	    	//Clean up the edges
	    	data32[i] = data32[i] & 255;
			
	    	//Get the Letters
	    	hex32 += HEX32ARRAY.charAt(data32[i]);	
		}
	    
		return hex32;
	}
	
	public static byte[] decode32(String zHex32) throws ArithmeticException {
		String hex = zHex32.toUpperCase();
		int strlen = hex.length();
		
		if(strlen % 8 != 0) {
			throw new ArithmeticException("Minima Address Base 32 String must be multiple of 8 in length");
		}
		
		//Get the Len..
		int rounds  = strlen / 8;
		int bytelen = rounds * 5; 
		
		//Get all the digits..
		int[] digits = new int[strlen];
		for(int i=0;i<strlen;i++) {
			digits[i] = findChar32(hex.charAt(i));
		}
		
		//Now decode..
		byte[] redata = new byte[bytelen];
		for(int i=0;i<rounds;i++) {
			int sdata = i*5;
			int sdigi = i*8;
			
			redata[sdata]   = (byte) ((digits[sdigi] << 3   | digits[sdigi+1] >> 2) & 255);  
			redata[sdata+1] = (byte) ((digits[sdigi+1] << 6 | digits[sdigi+2] << 1 | digits[sdigi+3] >> 4) & 255);  
			redata[sdata+2] = (byte) ((digits[sdigi+3] << 4 | digits[sdigi+4] >> 1) & 255);
			redata[sdata+3] = (byte) ((digits[sdigi+4] << 7 | digits[sdigi+5] << 2 | digits[sdigi+6] >> 3) & 255);
			redata[sdata+4] = (byte) ((digits[sdigi+6] << 5 | digits[sdigi+7]) & 255);
		}
		
		return redata;
	}
	
	public static void main(String[] zArgs) {

//		MiniData hash = MiniData.getRandomData(64);
//		byte[] hdata = hash.getData();
//		byte[] checkhash = Crypto.getInstance().hashData(hash.getData());
//		
//		byte[] addr = new byte[65];
//		for(int i=0;i<32;i++) {
//			addr[i] = hdata[i];
//		}
//		
////		addr[32] = checkhash[0];
////		addr[33] = checkhash[1];
////		addr[34] = checkhash[2];
//		
//		
//		String address = "Mx"+encode32(addr);
//		
//		System.out.println(hash.to0xString());
//		System.out.println(address);
		
		//BASE32
		byte[] data = new byte[10];
		
		data[0] = (byte) 1;
		data[1] = (byte) 13;
		data[2] = (byte) 34;
		data[3] = (byte) 44;
		data[4] = (byte) 33;
		data[5] = (byte) 56;
		data[6] = (byte) 99;
		data[7] = (byte) 76;
		data[8] = (byte) 9;
		data[9] = (byte) 12;

		String tt = encode32(data);
		
		System.out.println("32 "+tt);
		
		byte[] reda = decode32(tt);
		
		for(int i=0;i<10;i++) {
			System.out.println(i+") "+( reda[i] & 255 ) );	
		}
//		
//		byte[] hh = new byte[2];
//		hh[0] = (byte) 15;
//		hh[1] = (byte) 15;
//		
//		System.out.println("HEX : "+encode16(hh));	
	}
}
