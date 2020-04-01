package org.minima.utils.digest;

import java.util.Random;

import org.minima.objects.base.MiniData;

public class tester {

	public static void main(String[] zArgs) {
		
		//Get some bytes..
		String sdata = "Hello from Minima!!";
		byte[] data  = sdata.getBytes();
			
		//Need a Random Seed..
		byte[] seed = new byte[64];
		Random rand = new Random();
		rand.nextBytes(seed);
		
//		for(int i=0;i<seed.length;i++) {
//			seed[i] = (byte) i;
//		}
				
		Digest digest = new KeccakDigest(160);

		WinternitzOTSignature wots = new WinternitzOTSignature(seed, digest, 12);
		
		byte[] pubk = wots.getPublicKey();
		MiniData pubkey  = new MiniData(pubk);
		System.out.println("Public Key : "+pubkey.to0xString());
		
		byte[] signature = wots.getSignature(data);

		MiniData sig  = new MiniData(signature);
		System.out.println("Signature : "+sig.to0xString());
		System.out.println("Signature Length : "+sig.getLength());
		
		//Verify
		WinternitzOTSVerify wver = new WinternitzOTSVerify(digest, 12);
		byte[] key = wver.Verify(data, signature);
		
		MiniData pub = new MiniData(key);
		
		if(Arrays.areEqual(pubk, key)){
			System.out.println("Signature OK! "+pub.getLength());
		}else {
			System.out.println("Signature WRONG! "+pub.getLength());
		}
		
		
	}
	
}
