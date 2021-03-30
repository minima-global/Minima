#FROM openjdk:11
FROM adoptopenjdk/openjdk11:x86_64-alpine-jdk-11.0.9_11-slim as build-stage
WORKDIR /usr/src/minima/
COPY gradle gradle
COPY gradlew settings.gradle build.gradle ./
WORKDIR /usr/src/minima/
# Call gradlew before copying the source code to only download the gradle distribution once (layer will be cached)
#RUN ./gradlew --no-daemon -v
COPY lib lib
COPY src src
COPY test test
# minimal jar (750 kb) without dapp server -> build/libs/minima.jar
#RUN ./gradlew --no-daemon jar
# fatjar with all deps -> build/libs/minima-all.jar
RUN ./gradlew --no-daemon jar shadowJar
RUN md5sum build/libs/*
RUN ls -l build/libs/*
RUN stat build/libs/minima-all.jar
#RUN tar -cf minimajar.tar build/libs/minima-all.jar

FROM adoptopenjdk/openjdk11:x86_64-alpine-jdk-11.0.9_11-slim as production-stage
RUN apk --no-cache add curl
COPY --from=build-stage /usr/src/minima/build/libs/minima-all.jar /opt/minima/minima.jar
#COPY --from=build-stage /usr/src/minima/minimajar.tar /opt/minima/minimajar.tar
WORKDIR /opt/minima
#RUN tar -xf minimajar.tar
#RUN mv build/libs/minima-all.jar minima.jar
RUN touch -a -m -t 202011010000.00 minima.jar
#RUN rm minimajar.tar
RUN md5sum *.jar
RUN ls -l *.jar
RUN stat minima.jar
# 9001 minima protocol 9002 REST 9003 WebSocket 9004 MiniDapp Server 
EXPOSE 9001 9002 9003 9004 
ENTRYPOINT ["java", "-jar", "minima.jar"]
