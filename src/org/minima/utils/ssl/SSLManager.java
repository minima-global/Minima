package org.minima.utils.ssl;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.KeyStore;
import java.security.cert.X509Certificate;

import javax.net.ssl.KeyManagerFactory;

import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;

public class SSLManager {

	public static File getKeystroeFile() {
		//Where are we storing the key file..
		File sslkeyfolder 	= new File(GeneralParams.DATA_FOLDER,"ssl"); 
		sslkeyfolder.mkdirs();
		
		File sslkeyfile 	= new File(sslkeyfolder,"sslkeystore"); 
		
		return sslkeyfile;
	}
	
	public static void makeKeyFile() {
		
		try {
			
			File sslfile = getKeystroeFile();
			
			if(!sslfile.exists()) {
				MinimaLogger.log("Generating SSL Keystore.. "+KeyStore.getDefaultType());
				
				// Create Key
		        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
		        keyPairGenerator.initialize(4096);
		        KeyPair keyPair 			= keyPairGenerator.generateKeyPair();
		        final X509Certificate cert 	= SelfSignedCertGenerator.generate(keyPair, "SHA256withRSA", "localhost", 730);
		        KeyStore createkeystore 	= SelfSignedCertGenerator.createKeystore(cert, keyPair.getPrivate());

		        // Save the File
		        OutputStream fos = new FileOutputStream(sslfile);
		        createkeystore.store(fos, "MINIMAPWD".toCharArray());
		        fos.flush();
		        fos.close();
			}else {
				MinimaLogger.log("Loading SSL Keystore.. ");
			}
			
	        
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
	}
	
	public static KeyStore getSSLKeyStore() {
		try {
			// Load the keystore
	        KeyStore loadedKeyStore = KeyStore.getInstance(KeyStore.getDefaultType());
	        InputStream fis = new FileInputStream(getKeystroeFile());
	        loadedKeyStore.load(fis, "MINIMAPWD".toCharArray());
	        fis.close();
			
			return loadedKeyStore;
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		return null;
	}
	
	public static KeyManagerFactory getSSLKeyFactory(KeyStore zKeyStore) {
		try {
//			KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
			KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
			keyManagerFactory.init(zKeyStore, "MINIMAPWD".toCharArray());

			return keyManagerFactory;
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		return null;
	}
}
