# Dockerfile to create image with cron services
FROM eclipse-temurin:11

#Install Cron
RUN apt-get update
RUN apt-get -y install cron

# Add the script to the Docker Image
ADD archivedata.sh /root/archivedata.sh

# Give execution rights on the cron scripts
RUN chmod +x /root/archivedata.sh

# Add the cron job
RUN crontab -l | { cat; echo "0 1 * * * /root/archivedata.sh"; } | crontab -
