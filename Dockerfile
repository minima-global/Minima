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
# minimal jar (750 kb) without dapp server -> build/libs/minima.jar
#RUN ./gradlew --no-daemon jar
# fatjar with all deps -> build/libs/minima-all.jar
RUN ./gradlew --no-daemon shadowJar

FROM adoptopenjdk/openjdk11:x86_64-alpine-jdk-11.0.9_11-slim as production-stage
COPY --from=build-stage /usr/src/minima/build/libs/minima-all.jar /opt/minima/minima.jar
WORKDIR /opt/minima
CMD ["java", "-jar", "minima.jar"]

