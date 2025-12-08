package org.minima.utils.jni;

public class jnifunctions {

	//Load the native library..
	//MUST start minima.jar with -Djava.library.path=PATH_TO_MINIMA_JNI_FOLDER
	public static void loadNativeLib() {
		System.loadLibrary("native");
	}
	
	public jnifunctions() {}
	
	//JNI Methods..
	public native void sayHello();
    
	public native long sumIntegers(int first, int second);
	
	public native String sayHelloToMe(String name, boolean isFemale);
    
	public native byte[] hashHeader(byte[] headerbytes);
	
	//Returns the NONCE as byte array
	//I have added mytestnonce just so I can return a valid MiniNumber from the JNI for testing.. you don't need to use it!
	public native byte[] hashHeaderWithDiff(byte[] mytestnonce, int maxattempts, byte[] txndifficulty, byte[] headerbytes);
}
