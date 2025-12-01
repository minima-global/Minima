#!/bin/sh

#First cd into java src folder
cd ../src/org/minima/utils/jni

#Create the header file
javac -h . jnifunctions.java

#Clean up
rm jnifunctions.class

#Move header file into cc folder
mv org_minima_utils_jni_jnifunctions.h ../../../../../jni/src

#Now go into the cc folder
cd ../../../../../jni/src

#Now compile the cpp file
g++ -c -fPIC -I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux org_minima_utils_jni_jnifunctions.cpp -o org_minima_utils_jni_jnifunctions.o

#Now create the lib
g++ -shared -fPIC -o libnative.so org_minima_utils_jni_jnifunctions.o -lc

#Copy to lib folder
mv libnative.so ../lib

