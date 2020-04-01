package org.minima.utils;

import java.math.BigInteger;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;

public class BaseConverter {
	
	public static String encode16(byte[] zData){
		if(zData.length == 0) {
			return "";
		}
		
		String ret = new BigInteger(1,zData).toString(16).toUpperCase();
		
		//Now format so 0 padding..
		ret = String.format("%"+(zData.length*2)+"s", ret).replace(" ", "0");
		
		return ret;
	}
	
	public static byte[] decode16(String zBaseString) {
		if(zBaseString.equals("")) {
			return new byte[0];
		}
		
		//Get the data
		byte[] data = new BigInteger(zBaseString, 16).toByteArray();	
		
		//Now make sure the padding taken into account
		byte[] ffres = new byte[zBaseString.length()/2];
		
		//And copy over..
		int offset=ffres.length-data.length;
		for(int i=0;i<data.length;i++) {
			ffres[i+offset] = data[i];
		}
		
		return ffres;
	}
	
	public static String encode32(byte[] zData) throws ArithmeticException{
		if(zData.length % 8 != 0) {
			throw new ArithmeticException("Base32 encoder data length MUST be a multiple of 8");
		}
		
		if(zData.length == 0) {
			return "";
		}
		
		//How many 8 Bar units.. 8 Values 5 Characters
		int bar = zData.length / 8;
				
		//There are 8 characters to decode every cycle..
		byte[] values = new byte[8];
		
		int counter=0;
		for(int i=0;i<8;i++) {
			
			byte current =  (byte) (zData[counter] & (byte)255);
			
//			values[counter] = zData[counter] & ;
					
		}
		
		
		
		
		
		String ret = new BigInteger(1,zData).toString(32).toUpperCase();
		
		//Now format so 0 padding..
//		ret = String.format("%"+(bar*5)+"s", ret).replace(" ", "0");
		
		return ret;
	}
	
//	public static byte[] decode32(String zBaseString) {
//		if(zBaseString.equals("")) {
//			return new byte[0];
//		}
//		
//		//Get the data
//		byte[] data = new BigInteger(zBaseString, 32).toByteArray();	
//		
//		//Now make sure the padding taken into account
//		byte[] ffres = new byte[zBaseString.length()/2];
//		
//		//And copy over..
//		int offset=ffres.length-data.length;
//		for(int i=0;i<data.length;i++) {
//			ffres[i+offset] = data[i];
//		}
//		
//		return ffres;
//	}
	
	
//	public static byte[] convertData(String zBaseString, int zBase) {
//		if(zBaseString.length() == 0) {
//			return new byte[0];
//		}
//		
//		return new BigInteger(zBaseString, zBase).toByteArray();
//	}
	
	
//	public static String getDataAsBase32(byte[] zData) {
//		if(zData.length == 0) {
//			return "";
//		}
//		return new BigInteger(1,zData).toString(32).toUpperCase();
//	}
//	
//	public static byte[] convert32Data(String zBaseString) {
//		if(zBaseString.startsWith("0z")) {
//			return convertData(zBaseString.substring(2), 32);
//		}else {
//			return convertData(zBaseString, 32);	
//		}
//	}
//	
	
	public static void main(String[] zArgs) {
		
//		byte[] pp = new byte[2];
//		pp[0] = (byte) -1;
//		pp[1] = (byte) -1;
////		pp[2] = 0;
////		pp[3] = 0;
////		pp[4] = 0;
////		pp[5] = 0;
////		pp[6] = 0;
////		pp[7] = 0;
////		pp[8] = 0;
//		
//		
//		//BASE 16
//		for(int i=0;i<pp.length;i++) {
//			System.out.println("Original "+i+") "+pp[i]);
//		}
//		String hex = encode16(pp);
//		System.out.println("\nHEX   : "+hex+"\n");
//		byte[] data = decode16(hex);
//		for(int i=0;i<data.length;i++) {
//			System.out.println("Decode16 "+i+") "+data[i]);
//		}
//
//		
//		//BASE 32
//		String hex32 = encode32(pp);
//		System.out.println("\nHEX32 : "+hex32+"\n");
////		data = decode32(hex32);
////		for(int i=0;i<data.length;i++) {
////			System.out.println("Decode32 "+i+") "+data[i]);
////		}
//		
		byte[] oo = new byte[1];
		oo[0] = 1;	
//		oo[1] = 1;
//		oo[2] = 1;
//		oo[3] = 1;
//		oo[4] = 1;
//		oo[5] = 1;
//		oo[6] = 1;
//		oo[7] = 1;
		
		System.out.println(new BigInteger(1,oo).toString(32).toUpperCase());
	
	
//	String base32Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
	String base32Chars = "0123456789ABCDEFGHIJKLMNOPQRSTUV";
	
	//BASE32
	byte[] data = new byte[8];
	data[0] = 8;
	data[1] = 8;
	data[2] = 8;
	data[3] = 8;
	data[4] = 8;
	data[5] = 8;
	data[6] = 8;
	data[7] = 8;
	
	String hex32 = "";
	int counter=0;
	
	int cbyte = data[counter++] & 255;
	
	int digit = cbyte >> 3;
	hex32 += base32Chars.charAt(digit);
	
	digit = (cbyte & 7) << 2;
	
	cbyte = data[counter++] & 255;
	
	digit = digit | (cbyte & 192);
	hex32 += base32Chars.charAt(digit);
	
	
	System.out.println(hex32);
	
	
	
	
	
	
	}
}
