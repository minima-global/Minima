#!/bin/sh

echo "Build the JARS using gradle"
./gradlew clean build

echo "Now copy to jar folder"
cp -v ./build/libs/minima.jar ./jar/minima-nolibs.jar
cp -v ./build/libs/minima-all.jar ./jar/minima.jar

echo List Files
ls -la ./jar

echo Done

