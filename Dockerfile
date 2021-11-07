FROM openjdk:11

ENV HOME=/home/minima
ENV LOCAL=/usr/local

WORKDIR $LOCAL

# Create Minima user
RUN groupadd -g 1000 minima
RUN useradd -r -u 1000 -g 1000 -d $HOME minima

# Copy in startup script, minima and dapps
COPY minima-all.jar minima/minima.jar

# Get other permissions right, too
RUN mkdir -p $HOME/.minima
RUN chown -R minima:minima $HOME

VOLUME /home/minima/.minima

# Minima ports
EXPOSE 9001 9002 9003 9004 9005

USER minima
ENTRYPOINT ["java", "-jar", "minima/minima.jar"]