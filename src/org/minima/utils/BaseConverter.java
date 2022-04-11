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
