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

COPY block-0.1.5.mds.zip $HOME/dapps/block-0.1.5.mds.zip
COPY docs_1.1.3.mds.zip $HOME/dapps/docs_1.1.3.mds.zip
COPY ic_1.3.11.mds.zip $HOME/dapps/ic_1.3.11.mds.zip
COPY maxsolo_1.9.mds.zip $HOME/dapps/maxsolo_1.9.mds.zip
COPY news-1.0.mds.zip $HOME/dapps/news-1.0.mds.zip
COPY scriptide-1.7.mds.zip $HOME/dapps/scriptide-1.7.mds.zip
COPY terminal-1.91.mds.zip $HOME/dapps/terminal-1.91.mds.zip
COPY wallet_1.9.1.mds.zip $HOME/dapps/wallet_1.9.1.mds.zip

COPY minima.config $HOME/minima.config

RUN chown -R minima:minima $HOME


# Minima ports
EXPOSE 9001 9002 9003 9004 9005

USER minima
ENTRYPOINT ["java", "-jar", "minima/minima.jar", "-daemon" ,"-conf", "/home/minima/minima.config"]
