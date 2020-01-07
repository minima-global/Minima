/**
 * 
 */
package org.minima.utils.tests.gmss;

import java.security.SecureRandom;

import org.minima.utils.MinimaLogger;

/**
 * @author Spartacus Rex
 *
 */
public class GMSS {

public static void main(String args[]){

		MinimaLogger.log("Starting..");
	
		SecureRandom    keyRandom = new SecureRandom();
		keyRandom.setSeed(10);
		
		int[] defh = {10,10};
        int[] defw = {8,8};
        int[] defk = {2,2};
//        int[] defh = {10};
//        int[] defw = {8};
//        int[] defk = {2}; 
//        int[] defh = {10,10,10};
//        int[] defw = {8,8,8};
//        int[] defk = {2,2,2};
        
        
////		   GMSSParameters params = new GMSSParameters(10);
//		   GMSSParameters params = new GMSSParameters(defh.length, defh, defw, defk);
//		
//			    GMSSDigestProvider digProvider = new GMSSDigestProvider()
//			    {
//			        public Digest get()
//			        {
//			        	return new KeccakDigest(256);
////			            return new SHA256Digest();
////			            return new SHA1Digest();
//			        }
//			    };
//
//			    
//			    GMSSKeyPairGenerator gmssKeyGen = new GMSSKeyPairGenerator(digProvider);
//			    
//			    GMSSKeyGenerationParameters genParam = new GMSSKeyGenerationParameters(keyRandom, params);
//
//			    long timenow = System.currentTimeMillis();
//			    SimpleLogger.log("Key Generation..");
//			    gmssKeyGen.init(genParam);
//			    
//			    AsymmetricCipherKeyPair pair = gmssKeyGen.generateKeyPair();
//				
//			    long diff = System.currentTimeMillis() - timenow;
//			    SimpleLogger.log("Key Generation.. done "+diff+" millisecs");
//			    
//			    ParametersWithRandom param = new ParametersWithRandom(pair.getPrivate(), keyRandom);
//
//			    DigestingMessageSigner gmssSigner = new DigestingMessageSigner(new GMSSSigner(digProvider), new KeccakDigest(256));
//			    gmssSigner.init(true, param);
//
//				String ex="hello you how are you doing!!@";
//				if (args.length>1) ex = args[0];
//				
//				byte[] message = ex.getBytes();
//			    
//			    gmssSigner.update(message, 0, message.length);
//			    byte[] sig = gmssSigner.generateSignature();
//			    
//			    
//			    gmssSigner.init(false, pair.getPublic());
//			    gmssSigner.update(message, 0, message.length);
//			    
//			    SimpleLogger.log("Message:"+ex);
//			    
//				GMSSPrivateKeyParameters priv = (GMSSPrivateKeyParameters) pair.getPrivate();
//				GMSSPublicKeyParameters pub = (GMSSPublicKeyParameters) pair.getPublic();
//
//				byte[] pubKey = pub.getPublicKey();
//				
//				SimpleLogger.log("Public key: 0x"+getHexString(pubKey));
//
//			    String sig_hex=getHexString(sig);
//			    String sig_out = sig_hex.substring(0, Math.min(sig_hex.length(), 40));
//			    SimpleLogger.log("Signature Length            :"+sig.length);
//			    SimpleLogger.log("Signature Length (hex chars):"+sig_hex.length());
//			    SimpleLogger.log("Signature Length (first 40 hex char):"+sig_out);
//			     
//			    
//			    //Now create a verifier..
//			    timenow 					= System.currentTimeMillis();
//			    DigestingMessageSigner verSig 	= new DigestingMessageSigner(new GMSSSigner(digProvider), new KeccakDigest(256));
//			    	
//			    GMSSPublicKeyParameters pubKeyParams = new GMSSPublicKeyParameters(pubKey, params);
//			    verSig.init(false, pubKeyParams);
//			    verSig.update(message, 0, message.length);
//			    
//			    boolean success = verSig.verifySignature(sig);
//			    
//			    diff = System.currentTimeMillis() - timenow;
//			    
//			    if (!success)
//			    {
//			    	SimpleLogger.log("Failure");
//			    }
//			    else {
//			    	SimpleLogger.log("It Works! "+diff+" milli seconds");
//			    
//			    }
//
		    
		
		}
	public static String getHexString(byte[] b) {
		  String result = "";
		  for (int i=0; i < b.length; i++) {
		    result +=
		          Integer.toString( ( b[i] & 0xff ) + 0x100, 16).substring( 1 );
		  }
		  return result;
		}
}


