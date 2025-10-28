
/**
 * Create a destroy the DB
 */
function wipeDB(callback){
	//Run this..
	MDS.sql("DROP TABLE `channels`",function(msg){
		MDS.sql("DROP TABLE `logs`",function(msg){
			MDS.log("DB Wiped..");
			
			if(callback){
				callback();
			}
		});
	});
}

function createDB(callback){
	
	//Create the DB if not exists
	var initsql = "CREATE TABLE IF NOT EXISTS `channels` ( "
				+"  `id` bigint auto_increment, "
				
				+"  `hashid` varchar(256) NOT NULL, "
				+"  `state` varchar(256) NOT NULL, "
				
				+"  `usernum` int NOT NULL, "
				+"  `userpublickey` varchar(256), "
								
				+"  `user1name` varchar(1024), "	
				+"  `user1maximaid` varchar(1024), "	
				+"  `user1publickey` varchar(256), "
				+"  `user1address` varchar(256), "
				+"  `user1amount` varchar(256), "
				
				+"  `user2name` varchar(1024), "	
				+"  `user2maximaid` varchar(1024),"	
				+"  `user2publickey` varchar(256),"
				+"  `user2address` varchar(256),"
				+"  `user2amount` varchar(256),"
				
				+"  `tokenname` varchar(256),"
				+"  `tokenid` varchar(256),"
				+"  `tokendata` varchar(256000),"
																
				+"  `totalamount` varchar(256),"
												
				+"  `fundingaddress` varchar(256),"
				+"  `eltooaddress` varchar(256),"
				
				+"  `sequence` bigint,"
				
				+"  `triggertxn` varchar(256000),"
				+"  `settletxn` varchar(256000),"								
				+"  `updatetxn` varchar(256000),"
				
				+"  `fundingspent` int NOT NULL default 0,"
				
				+"  `payoutfound` int NOT NULL default 0,"
				+"  `payoutamount` varchar(256) NOT NULL default '0',"
																				
				+"  `date` bigint NOT NULL "
				+" )";
				
	//Run this..
	MDS.sql(initsql,function(msg){
		
		//Create the DB if not exists
		var messages = "CREATE TABLE IF NOT EXISTS `logs` ( "
					+"  `id` bigint auto_increment, "
					+"  `hashid` varchar(256) NOT NULL, "
					+"  `type` varchar(256) NOT NULL, "
					+"  `message` varchar(4096) NOT NULL, "
					+"  `date` bigint NOT NULL "
					+" )";
					
		//Run this..
		MDS.sql(messages,function(msg){
			
			if(callback){
				callback(msg);
			}
		});
	});
}


/**
 * Add channel details
 */
function sqlInsertNewChannel(details, state, usernum, callback){
	
	//Insert this unread message
	var sql = "INSERT INTO channels(hashid, state, usernum, user1name, user1maximaid, user1publickey, user1address, user1amount, "
									+"user2maximaid, user2amount, tokenname, tokenid, tokendata, totalamount, date) "
			 +"VALUES ('"+details.hashid+"','"+state+"',"+usernum+",'"
				//User details
				+encodeStringForDB(details.user.name)+"','"+details.user.maximaid+"','"+details.user.publickey+"','"+details.user.address+"','"+details.useramount
				
				//Counterpary User
				+"','"+details.tomaximapublickey
				+"','"+details.requestamount
				
				//Token Details
				+"','"+encodeStringForDB(details.tokenname)
				+"','"+details.tokenid
				+"','"+details.tokendata
				
				//Total amount in channel
				+"','"+details.totalamount
				
				//Date
				+"',"+getTimeMilli()+")";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

/**
 * Get ALL details for ALL channels
 */
function sqlSelectAllChannels(callback){
	//Find a record
	var sql = "SELECT * FROM channels";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

/**
 * Get details for open channels
 */
function sqlSelectAllOpenChannels(callback){
	//Find a record
	var sql = "SELECT * FROM channels WHERE state !='STATE_CHANNEL_CLOSED'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

/**
 * Get details for closed channels
 */
function sqlSelectAllClosedChannels(callback){
	//Find a record
	var sql = "SELECT * FROM channels WHERE state ='STATE_CHANNEL_CLOSED'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

function sqlDeleteAllClosedChannels(callback){
	//Find a record
	var sql = "DELETE FROM channels WHERE state ='STATE_CHANNEL_CLOSED'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}


/**
 * Get ELTOO check details for ALL channels
 */
function sqlSelectEltooChannels(callback){
	//Find a record
	var sql = "SELECT hashid, state, eltooaddress, sequence FROM channels";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}
	});
}

/**
 * Get the details of a channel
 */
function sqlSelectChannel(hashid, callback){
	//Find a record
	var sql = "SELECT * FROM channels WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}	
	});
}

/**
 * Check for FUNDING coin
 */
function sqlSelectRelevantFundingCoin(address, callback){
	//Find a record
	var sql = "SELECT * FROM channels WHERE fundingaddress='"+address+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}	
	});
}

/**
 * Check for ELTOO coin
 */
function sqlSelectRelevantEltooCoin(address, callback){
	//Find a record
	var sql = "SELECT * FROM channels WHERE eltooaddress='"+address+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}	
	});
}

/**
 * Check for PAYOUT coins
 */
function sqlSelectPayoutCoin(address, callback){
	//Find a record
	var sql = "SELECT * FROM channels WHERE user1address='"+address+"' OR user2address='"+address+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);
		}	
	});
}

/**
 * Update MY PUBLIC KEY
 */
function updateMyPublicKey(hashid, publickey, callback){
	//Find a record
	var sql = "UPDATE channels SET userpublickey='"+publickey+"' WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Update Channel State
 */
function updateChannelState(hashid, state, callback){
	//Find a record
	var sql = "UPDATE channels SET state='"+state+"' WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Update User2 details
 */
function updateChannelUser2(hashid, user, callback){
	//Find a record
	var sql = "UPDATE channels SET user2name='"+encodeStringForDB(user.name)+"', user2publickey='"+user.publickey+"', user2address='"+user.address+"' WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Update the special address
 */
function updateChannelAddresses(hashid, alldata, callback){
	//Find a record
	var sql = "UPDATE channels SET fundingaddress='"+alldata.addresses.fundingaddress.address
				+"', eltooaddress='"+alldata.addresses.eltooaddress.address+"', sequence=0 WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Update the special address
 */
function updateDefaultChannelTransactions(hashid, alldata, callback){
	//Find a record
	var sql = "UPDATE channels SET triggertxn='"+alldata.transactions.triggertxn
				+"', settletxn='"+alldata.transactions.settletxn+"', sequence=0 WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Update to a NEW Sequence, Settlement and Update txn
 */
function updateNewSeqeunceTxn(hashid, sequence, user1amount, user2amount, settletxn, updatetxn, callback){
	//Find a record
	var sql = "UPDATE channels SET settletxn='"+settletxn
				+"', updatetxn='"+updatetxn+"',"
				+"user1amount='"+user1amount+"', user2amount='"+user2amount+"',"
				+" sequence="+sequence+" WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Set Funding spent
 */
function updateFundingSpent(hashid, callback){
	//Find a record
	var sql = "UPDATE channels SET fundingspent=1, state='STATE_CHANNEL_START_CLOSE' WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Set Payout found
 */
function updatePayoutFound(hashid, amount, callback){
	//Find a record
	var sql = "UPDATE channels SET payoutfound=1, payoutamount='"+amount+"' WHERE hashid='"+hashid+"'";
				
	//Run this..
	MDS.sql(sql,function(msg){
		
		//Now select the NEW details
		sqlSelectChannel(hashid, function(select){
			if(callback){
				callback(select.rows[0]);	
			}	
		});
	});
}

/**
 * Close channels
 */
function updateClosedChannels(callback){

	//The records we are looking for	
	var where = " state!='STATE_CHANNEL_CLOSED' AND ((fundingspent=1 AND payoutfound=1) OR state='STATE_REQUEST_CANCELLED' OR state='STATE_REQUEST_DENIED')"
	
	//Find a record
	var checksql = "SELECT hashid, state, fundingspent, payoutfound FROM channels WHERE "+where;
	
	//Run this..
	var closedfound=false;
	MDS.sql(checksql,function(checkmsg){
		if(checkmsg.count>0){
			closedfound=true;
		}
		
		//Insert a LOG for each channel
		for(var i=0;i<checkmsg.count;i++){
			//LOGS
			insertLog(checkmsg.rows[i].HASHID,"CHANNEL_CLOSE","The channel was successfully closed")	
		}
		
		//Did we find any channels
		if(closedfound){
			//Find a record
			var sql = "UPDATE channels SET state='STATE_CHANNEL_CLOSED' WHERE "+where;
						
			//Run this..
			MDS.sql(sql,function(msg){
				if(callback){
					callback(true);	
				}	
			});		
		}else{
			if(callback){
				callback(false);	
			}
		}
	});
}

/**
 * Add a log
*/
var PRINT_LOGS = true;
function insertLog(hashid, type, message, callback){
	
	//Do we print to stdio
	if(PRINT_LOGS){
		MDS.log(hashid+"> "+type+": "+message);
	}
	
	//Insert this unread message
	var sql = "INSERT INTO logs(hashid, type, message, date) "
			 +"VALUES ('"+hashid+"','"+type+"','"+encodeStringForDB(message)+"',"+getTimeMilli()+")";
	
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

function getLogs(hashid, callback){
	var sql = "SELECT * FROM logs WHERE hashid='"+hashid+"'";
	MDS.sql(sql,function(msg){
		if(callback){
			callback(msg);	
		}
	});
}

