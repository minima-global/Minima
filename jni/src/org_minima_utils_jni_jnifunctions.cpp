#include <jni.h>        // JNI header provided by JDK
#include <stdio.h>      // C Standard IO Header
#include <iostream>
#include "org_minima_utils_jni_jnifunctions.h"   // Generated

JNIEXPORT void JNICALL Java_org_minima_utils_jni_jnifunctions_sayHello
  (JNIEnv* env, jobject thisObject) {
    std::cout << "Hello from C++ !!" << std::endl;
}

JNIEXPORT jlong JNICALL Java_org_minima_utils_jni_jnifunctions_sumIntegers 
  (JNIEnv* env, jobject thisObject, jint first, jint second) {
    std::cout << "C++: The numbers received are : " << first << " and " << second << std::endl;
    return (long)first + (long)second;
}

JNIEXPORT jstring JNICALL Java_org_minima_utils_jni_jnifunctions_sayHelloToMe 
  (JNIEnv* env, jobject thisObject, jstring name, jboolean isFemale) {
    const char* nameCharPointer = env->GetStringUTFChars(name, NULL);
    std::string title;
    if(isFemale) {
        title = "Ms. ";
    }
    else {
        title = "Mr. ";
    }

    std::string fullName = title + nameCharPointer;
    return env->NewStringUTF(fullName.c_str());
}

JNIEXPORT jbyteArray JNICALL Java_org_minima_utils_jni_jnifunctions_hashHeader
	(JNIEnv* env, jobject thisObject, jbyteArray headerbytes){
	
	//DO STUFF!!
	//..
	
	return headerbytes;
}

JNIEXPORT jbyteArray JNICALL Java_org_minima_utils_jni_jnifunctions_hashHeaderWithDiff
  (JNIEnv * env, jobject thisObject, jbyteArray mytestnonce, jint maxattempts, jbyteArray targetdifficulty, jbyteArray headerbytes){
	
   //DO STUFF!!
	//..
	
	return mytestnonce;		
}