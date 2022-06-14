/**
* MAXSOLO Initialisation
* 
* @spartacusrex
*/


//Create the DB if not exists
window.executeSQL("CREATE TABLE IF NOT EXISTS `messages` ( "
					+"  `id` IDENTITY PRIMARY KEY, "
					+"  `roomname` varchar(160) NOT NULL, "
					+"  `publickey` varchar(512) NOT NULL, "
					+"  `username` varchar(160) NOT NULL, "
					+"  `type` varchar(64) NOT NULL, "
					+"  `message` varchar(512) NOT NULL, "
					+"  `filedata` clob(256K) NOT NULL, "
					+"  `customid` varchar(128) NOT NULL DEFAULT '0x00', "
					+"  `state` varchar(128) NOT NULL DEFAULT '', "
					+"  `read` int NOT NULL DEFAULT 0, "
					+"  `date` bigint NOT NULL "
					+" )");
