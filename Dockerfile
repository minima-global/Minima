#FROM openjdk:11
FROM adoptopenjdk/openjdk11:x86_64-alpine-jdk-11.0.9_11-slim as build-stage
WORKDIR /usr/src/minima/
COPY gradle gradle
COPY gradlew settings.gradle build.gradle .classpath ./
# Call gradlew before copying the source code to only download the gradle distribution once (layer will be cached)
RUN ./gradlew --no-daemon -v
COPY lib lib
COPY src src
COPY test test
RUN ./gradlew --no-daemon jar

FROM adoptopenjdk/openjdk11:x86_64-alpine-jdk-11.0.9_11-slim as production-stage
COPY build/libs/minima.jar /opt/minima/
WORKDIR /opt/minima
CMD ["java", "-jar", "minima.jar"]

