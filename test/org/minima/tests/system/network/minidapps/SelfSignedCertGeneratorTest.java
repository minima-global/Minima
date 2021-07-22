package org.minima.tests.system.network.minidapps;

import org.bouncycastle.cert.CertIOException;
import org.bouncycastle.operator.OperatorCreationException;
import org.junit.Test;
import org.minima.system.network.minidapps.SelfSignedCertGenerator;
import org.minima.system.Main;

import java.io.*;
import java.security.*;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

import static org.junit.Assert.*;


public class SelfSignedCertGeneratorTest {

    @Test
    public void createSelfSignedCertificate() throws CertificateException, CertIOException, OperatorCreationException, NoSuchAlgorithmException {
        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(4096);
        KeyPair keyPair = keyPairGenerator.generateKeyPair();
        final X509Certificate cert = SelfSignedCertGenerator.generate(keyPair, "SHA256withRSA", "localhost", 730);
        assertNull(cert.getKeyUsage());
        assertNull(cert.getExtendedKeyUsage());
        assertEquals("X.509", cert.getType());
        assertEquals("CN=localhost", cert.getSubjectDN().getName());
        assertEquals(cert.getSubjectDN(), cert.getIssuerDN());
        assertEquals("SHA256withRSA", cert.getSigAlgName());
        assertEquals(3, cert.getVersion());
    }

    @Test
    public void CertificateStorage() throws Exception {

        // Create Key
        KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
        keyPairGenerator.initialize(4096);
        KeyPair keyPair = keyPairGenerator.generateKeyPair();
        final X509Certificate cert = SelfSignedCertGenerator.generate(keyPair, "SHA256withRSA", "localhost", 730);
        KeyStore keystore = SelfSignedCertGenerator.createKeystore(cert, keyPair.getPrivate());

        // Save the File
        File keysfile = new File("sslkeystore");
        OutputStream fos = new FileOutputStream(keysfile);
        keystore.store(fos, "MINIMAPWD".toCharArray());
        fos.flush();
        fos.close();

        // Load the keystore
        KeyStore loadedKeyStore = KeyStore.getInstance(KeyStore.getDefaultType());
        InputStream fis = new FileInputStream(keysfile);
        loadedKeyStore.load(fis, "MINIMAPWD".toCharArray());
        fis.close();

        // Check keystore is not null after it should have been loaded
        assertNotNull(loadedKeyStore);

        // Get the key
        KeyStore.PrivateKeyEntry kk = (KeyStore.PrivateKeyEntry) loadedKeyStore.getEntry(SelfSignedCertGenerator.CERTIFICATE_ALIAS, new KeyStore.PasswordProtection("MINIMAPWD".toCharArray()));
        assertNotNull(kk);
        X509Certificate loaded_cert = (X509Certificate) kk.getCertificate();

        // Check its valid
        assertNull(loaded_cert.getKeyUsage());
        assertNull(loaded_cert.getExtendedKeyUsage());
        assertEquals("X.509", loaded_cert.getType());
        assertEquals("CN=localhost", loaded_cert.getSubjectDN().getName());
        assertEquals(loaded_cert.getSubjectDN(), loaded_cert.getIssuerDN());
        assertEquals("SHA256withRSA", loaded_cert.getSigAlgName());
        assertEquals(3, loaded_cert.getVersion());

    }
}