#FROM openjdk:11
FROM adoptopenjdk/openjdk11:x86_64-alpine-jdk-11.0.9_11-slim
#COPY jar/minima.jar /usr/src/minima/
COPY build/libs/minima.jar /opt/minima/
WORKDIR /opt/minima
#RUN javac Main.java
CMD ["java", "-jar", "minima.jar"]

