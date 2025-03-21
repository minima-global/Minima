
Minima MySQL Archive
--------------------

With these files you can easily setup a MySQL backed Minima Archive node.

The only requirement is that you are running Docker.

SETUP
-----

1) Extract these files into a folder and go into that folder on Command line / PowerShell

2) Edit the docker-compose.yml file in _some_ text editor and change any params you need to.. ports, backup folder AND SET YOU MDS PASSWORD

vim docker-compose.yml

3) Pull the latest images.. so you are up to date..

docker-compose pull

4) Build the images for the containers

docker-compose build

5) Start the container cluster with :

docker-compose up -d

6) The Database tables are created automatically after 20 seconds when you start the system up - so wait 20 seconds.

7) You now need to load the latest Minima Archive data to fill the MySQL database..
   Download the latest archive - say from https://spartacusrex.com
   Copy the file (example : minima_archive_2023_10_04.gzip) into the ./minima folder - this is your Minima data folder
   
8) Import the data into the MySQL Database - you can do this in MDS Terminal or from the command line

 - From MDS.. Open the Terminal MiniDAPP

 - From command line, first find the name of the container running Minima :

docker-compose ps

Then connect to the container (your name will be different) :

docker exec -it minimaarchive-minima-1 /bin/bash

Once in the container just start the rpc client with :

minima

9) Now run this - the parameters will be the same except the file - change that to the correct filename  

mysql action:h2import host:mysql:3306 database:archivedb user:archiveuser password:archivepassword file:minima_archive_2023_10_04.gzip

10) This function takes a while to run.. wait for it to finish.. there wil be logs (extracting the h2db takes a while at the start..) 

11) Setup automatic MySQL updates - so your MySQL db will be updated periodically with the latest blocks

mysql action:autobackup enable:true host:mysql:3306 database:archivedb user:archiveuser password:archivepassword 

12) And.. Voila!.. all done. You can now open the MDS and add peers as normal.. Your node will connect to the network.   

..

Backups are made daily at 1am and put into the ./backups folder.

You can wipe and restart the containers - but if you do delete your ./minima folder. 

Your ./backups folder is unaffected.

Enjoy!
