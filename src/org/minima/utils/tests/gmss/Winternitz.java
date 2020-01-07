/**
 * 
 */
package org.minima.utils.tests.gmss;

/**
 * @author Spartacus Rex
 *
 */
public class Winternitz {

	public static void main(String[] zArgs){
		
		//Get some bytes..
//		String sdata = "Hello from Ramcash!! This is a really long message! And it "
//				+ "needs to be som that I can cjeck it..Hello from Ramcash!! This is ";
		
//		String sdata = "0x58bf01092e67681f4e172ed9a5e99907a95e045a10acebf1f5800e2f3137c84e";
//		
//		byte[] data  = sdata.getBytes();
//
//		//Need a Random Seed..
//		byte[] seed = new byte[64];
//		for(int i=0;i<seed.length;i++) {
//			seed[i] = (byte) i;
//		}
//		
////		Random rand = new Random();
////		rand.nextBytes(seed);
//		
//		//Get  the Digest.`
////		Digest digest = new SHA256Digest();
////		Digest digest = new KeccakDigest(512);
//		Digest digest = new SHA3Digest();
////		Digest digest = new SHA3Digest();
////		Digest digest = new SHA384Digest();
////		Digest digest = new RIPEMD160Digest();
////		Digest digest = new SHA1Digest();
//		
////		Digest digest = new SHA512Digest();
//		
//		int wnum = 12;
//		int len = digest.getDigestSize();
//		SimpleLogger.log("Digest : "+digest.getAlgorithmName()+" len:"+digest.getDigestSize());
//		
//		//Create a WOTS..
//		WinternitzOTSignature wots = new WinternitzOTSignature(seed, digest, wnum);
//		
//		MiniData pubkey = new MiniData(wots.getPublicKey());
//		SimpleLogger.log("Public Key : "+pubkey);
//		
////		MiniData privkey = new MiniData(wots.getPrivateKey());
//		SimpleLogger.log("Private Key : "+wots.getPrivateKey());  
//		
//		//Do it..
//		byte[] pubk 		= wots.getPublicKey();
//		byte[] signature 	= wots.getSignature(data);
////		signature[23] = 0;
////		signature[102] = 0;
//		
//		SimpleLogger.log("Signature :"+new BigInteger(signature).toString(16).toUpperCase());
//		SimpleLogger.log("Length :"+signature.length);
//		
//		long timestart = System.currentTimeMillis();
//		int maxnum = 1;
//		for(int i=0;i<maxnum;i++){
//			
//			//Verify
//			WinternitzOTSVerify wver = new WinternitzOTSVerify(digest, wnum);
//			byte[] key = wver.Verify(data, signature);
//			
//			MiniData pub = new MiniData(key);
//			
//			if(Arrays.areEqual(pubk, key)){
//				SimpleLogger.log("Signature OK! "+pub.getLength());
//			}else {
//				SimpleLogger.log("Signature WRONG! "+pub.getLength());
//			}
//		}
//		long timefinish = System.currentTimeMillis();
//		long diff = timefinish - timestart;
//		
//		SimpleLogger.log("Time x"+maxnum+" : "+diff);
//		SimpleLogger.log("Time x1 : "+(diff/maxnum));
//		
//		if(true){
//			return;
//		}
//		
//		//Ouptut..
//		SimpleLogger.log("Signature data    : "+sdata);
////		SimpleLogger.log("Signature datalen : "+data.length);
////		SimpleLogger.log("Signature len     : "+signature.length);
//		
		
		
	}
}


