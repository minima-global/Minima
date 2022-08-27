FROM openjdk:11 as builder

# Copy minima sources and build
COPY . /app
WORKDIR /app
RUN ./gradlew shadowJar
RUN mv /app/build/libs/minima*-all.jar /app/build/libs/minima-all.jar

FROM openjdk:11

ENV HOME=/home/minima
ENV LOCAL=/usr/local

WORKDIR $LOCAL

# Create Minima user
RUN groupadd -g 1000 minima
RUN useradd -r -u 1000 -g 1000 -d $HOME minima

# Copy minima jar from builder
COPY --from=builder /app/build/libs/minima-all.jar minima/minima.jar

# Get other permissions right, too
RUN mkdir -p $HOME/data
RUN mkdir -p $HOME/dapps

# Copy over the MiniDAPPs
COPY *.mds.zip $HOME/dapps/

# Copy the Config
COPY minima.config $HOME/minima.config

# Make minima the owner
RUN chown -R minima:minima $HOME

# Minima ports
EXPOSE 9001 9002 9003 9004 9005

# Switch to Minima User
USER minima

# Start her up 
ENTRYPOINT ["java", "-jar", "minima/minima.jar", "-conf", "/home/minima/minima.config"]
