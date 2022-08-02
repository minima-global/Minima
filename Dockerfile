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
RUN mkdir -p $HOME/data
RUN mkdir -p $HOME/data/.minima
RUN mkdir -p $HOME/dapps

COPY terminal-1.91.mds.zip $HOME/dapps/terminal-1.91.mds.zip
COPY block-0.1.5.mds.zip $HOME/dapps/block-0.1.5.mds.zip
COPY wallet_1.6.2.mds.zip $HOME/dapps/wallet_1.6.2.mds.zip
COPY minima.config $HOME/minima.config

RUN chown -R minima:minima $HOME


# Minima ports
EXPOSE 9001 9002 9003 9004 9005

USER minima
# ENTRYPOINT ["java", "-jar", "minima/minima.jar", "-daemon" ,"-conf", "/home/minima/minima.config"]
ENTRYPOINT ["java", "-jar", "minima/minima.jar", "-conf", "/home/minima/minima.config"]