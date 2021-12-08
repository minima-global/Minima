package org.minima.utils;

import java.math.BigInteger;

import org.minima.objects.base.MiniData;

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
	    
	    if(hexChars.length == 0) {
	    	return "";
	    }
	    
	    return "0x"+new String(hexChars);
	}
	
	public static byte[] decode16(String zHex) throws NumberFormatException {
		String hex = zHex;
		if(hex.toLowerCase().startsWith("0x")) {
			hex = zHex.substring(2);
		}		
		
		//Go Upper case - make sure always the same
		hex = hex.toUpperCase();
		int len = hex.length();
	
		//return empty array for 0 length hex string
		if(len == 0) {
			return new byte[0];
		}
		
		//Check that every char is a valid base 16 value..
		boolean isHex = hex.matches("[0-9A-F]+");
		if(!isHex) {
			throw new NumberFormatException("Invalid HEX string in decode16 : "+zHex);
		}
		
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
	
	public static String numberToHex(int zNumber) {
		String hex = Integer.toHexString(zNumber);
		if(hex.length() % 2 != 0) {
			hex = "0"+hex;
		}
		return new String("0x"+hex.toUpperCase());
	}
	
	public static int hexToNumber(String zHex) {
		return new BigInteger(zHex.substring(2), 16).intValue();
	}
	
	
	/**
	 * BASE 32 Encode and Decode
	 */
//	private static final String HEX32ARRAY = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
//	private static final String HEX32ARRAY = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
	
//	private static int findChar32(char zChar){
//		return HEX32ARRAY.indexOf(zChar);
//	}
//	
//	public static String encode32(byte[] zData) throws ArithmeticException {
//		if(zData.length % 5 != 0) {
//			throw new ArithmeticException("Minima Address Encode32 data MUST be multiples of 5 in length");
//		}
//		
//		//The final length
//		int rounds = zData.length / 5;
//		int len    = rounds * 8 ;
//		
//		//The data32 array
//		int[] data32 = new int[len];
//		
//		//The return string and temp variables
//		String hex32 = "";
//		int counter=0;
//		int currByte, digit;
//		
//		for(int i=0;i<rounds;i++) {
//			int start = i*8;
//			
//			//1
//			currByte      = zData[counter++] & 255;
//		    data32[start] = currByte >> 3;
//		    digit         = (currByte & 7) << 2;
//	
//		    //2
//		    currByte        = zData[counter++] & 255;
//		    data32[start+1] = digit | (currByte >> 6);
//		    data32[start+2] = (currByte >> 1) & 31;
//		    digit           = (currByte & 1) << 4;
//		    
//		    //3
//		    currByte        = zData[counter++] & 255;
//		    data32[start+3] = digit | (currByte >> 4);
//		    digit           = (currByte & 15) << 1;
//		    
//		    //4
//		    currByte        = zData[counter++] & 255;
//		    data32[start+4] = digit | (currByte >> 7);
//		    data32[start+5] = (currByte >> 2) & 31;
//		    digit           = (currByte & 3) << 3;
//		    
//		    //5
//		    currByte        = zData[counter++] & 255;
//		    data32[start+6] = digit | (currByte >> 5);
//		    data32[start+7] = currByte & 31;
//		}
//		
//		//Now add to the result string
//	    for(int i=0;i<len;i++) {
//	    	//Clean up the edges
//	    	data32[i] = data32[i] & 255;
//			
//	    	//Get the Letters
//	    	hex32 += HEX32ARRAY.charAt(data32[i]);	
//		}
//	    
//		return hex32;
//	}
//	
//	public static byte[] decode32(String zHex32) throws ArithmeticException {
//		String hex = zHex32.toUpperCase();
//		int strlen = hex.length();
//		
//		if(strlen % 8 != 0) {
//			throw new ArithmeticException("Minima Address Base 32 String must be multiple of 8 in length");
//		}
//		
//		//Get the Len..
//		int rounds  = strlen / 8;
//		int bytelen = rounds * 5; 
//		
//		//Get all the digits..
//		int[] digits = new int[strlen];
//		for(int i=0;i<strlen;i++) {
//			digits[i] = findChar32(hex.charAt(i));
//		}
//		
//		//Now decode..
//		byte[] redata = new byte[bytelen];
//		for(int i=0;i<rounds;i++) {
//			int sdata = i*5;
//			int sdigi = i*8;
//			
//			redata[sdata]   = (byte) ((digits[sdigi] << 3   | digits[sdigi+1] >> 2) & 255);  
//			redata[sdata+1] = (byte) ((digits[sdigi+1] << 6 | digits[sdigi+2] << 1 | digits[sdigi+3] >> 4) & 255);  
//			redata[sdata+2] = (byte) ((digits[sdigi+3] << 4 | digits[sdigi+4] >> 1) & 255);
//			redata[sdata+3] = (byte) ((digits[sdigi+4] << 7 | digits[sdigi+5] << 2 | digits[sdigi+6] >> 3) & 255);
//			redata[sdata+4] = (byte) ((digits[sdigi+6] << 5 | digits[sdigi+7]) & 255);
//		}
//		
//		return redata;
//	}
	
	public static String encode32(byte[] zData) throws ArithmeticException {
		
		//First create a BigInter
		BigInteger bigint = new BigInteger(1,zData);
		
		//Now convert to base32
		String bigint32 = bigint.toString(32).toLowerCase();  
		
		//Replace problematic characters..
		bigint32 = bigint32.replaceAll("i", "w");
		bigint32 = bigint32.replaceAll("l", "y");
		bigint32 = bigint32.replaceAll("o", "z");
		
		return "Mx"+bigint32.toUpperCase();
	}
	
	public static byte[] decode32(String zBase32) {
		
		//First remove the Mx..
		String b32 = zBase32.toLowerCase();
		if(b32.startsWith("mx")) {
			b32 = b32.substring(2);
		}
		
		//Replace problematic characters..
		b32 = b32.replaceAll("w", "i");
		b32 = b32.replaceAll("y", "l");
		b32 = b32.replaceAll("z", "o");
				
		//Now create a bigint..
		BigInteger bigint = new BigInteger(b32, 32);
		
		//Now get the HEX version..
		MiniData hexval = new MiniData("0x"+bigint.toString(16));
		
		return hexval.getBytes();
	}
	
	public static void main(String[] zArgs) {
		
//		MiniData hex = MiniData.getRandomData(3);
		MiniData hex = new MiniData("0xffdef");
		String hstr = hex.to0xString(); 
		System.out.println("HEX : "+hstr.length()+" "+hstr);
		
		String base32 = encode32(hex.getBytes());
		System.out.println("B32 : "+base32.length()+" "+base32);
	
		MiniData conv = new MiniData(base32);
		String convstr = conv.to0xString();
		System.out.println("COV : "+convstr.length()+" "+convstr);
		
		System.out.println(conv.isEqual(hex));
		
		
	}
}
