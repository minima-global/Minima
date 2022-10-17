#Use a JDK that allows multiple platforms
FROM adoptopenjdk:11-jre-hotspot

#Create a home folder
RUN mkdir -p /home/minima
ENV HOME=/home/minima

#Set the local work directory
ENV LOCAL=/usr/local
WORKDIR $LOCAL

# Copy in startup script, minima and dapps
COPY minima-all.jar minima/minima.jar

# Get other permissions right, too
RUN mkdir -p $HOME/data
RUN mkdir -p $HOME/dapps

# Copy over the MiniDAPPs
COPY *.mds.zip $HOME/dapps/

# Copy the Config
COPY minima.config $HOME/minima.config

#Copy the minima script
COPY minima /usr/bin/minima
RUN chmod +x /usr/bin/minima

#Install curl
#RUN apt-get update
#RUN apt-get -y install curl jq vim
#RUN apt-get clean
#RUN rm -rf /var/lib/apt/lists/*

#Make BASH the defult
RUN ln -sf /bin/bash /bin/sh

# Minima ports
EXPOSE 9001 9002 9003 9004

# Start her up 
ENTRYPOINT ["java", "-jar", "minima/minima.jar", "-conf", "/home/minima/minima.config"]
