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

#Copy the minima script
COPY minima /usr/bin/minima
RUN chmod +x /usr/bin/minima

#Make BASH the defult
RUN ln -sf /bin/bash /bin/sh

# Minima ports
EXPOSE 8888

# Start her up 
ENTRYPOINT ["java", "-cp", "minima/minima.jar", "org.minima.utils.archivenode.ArchiveServer"]
