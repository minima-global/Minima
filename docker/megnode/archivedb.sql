CREATE TABLE IF NOT EXISTS `cascadedata` (
  `id` int NOT NULL AUTO_INCREMENT,
  `cascadetip` bigint NOT NULL,
  `fulldata` mediumblob NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE IF NOT EXISTS `syncblock` (
  `id` int NOT NULL AUTO_INCREMENT,
  `txpowid` varchar(80) NOT NULL,
  `block` bigint NOT NULL,
  `timemilli` bigint NOT NULL,
  `syncdata` mediumblob NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `txpowid` (`txpowid`),
  UNIQUE KEY `block` (`block`)
) ENGINE=InnoDB AUTO_INCREMENT=458157 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;
