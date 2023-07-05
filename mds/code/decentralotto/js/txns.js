/**
 * TXN create code..
 */

var LOTTERY_ADVERT_SCRIPT  = "LET lotto=[ This is a DecentraLotto Advert ] LET pubkey=PREVSTATE(0) RETURN SIGNEDBY(pubkey)"
var LOTTERY_ADVERT_ADDRESS = "MxG081TA19N7Y8G4BGF30BD34GCS77KCKKD43JNUNSYN84T107V1DW78AQDGRY8"
var LOTTERY_ADVERT_AMOUNT  = "0.00000000000000000001";

var LOTTERY_GAME_SCRIPT 	= "LET round=STATE(0) LET prevround=PREVSTATE(0) ASSERT round EQ INC(prevround) LET playerpubkey=PREVSTATE(1) LET secret=PREVSTATE(2) LET odds=PREVSTATE(3) ASSERT (odds GT 0) AND (odds LT 1) LET lottopubkey=PREVSTATE(4) LET lottoaddress=PREVSTATE(5) IF round EQ 1 THEN IF @COINAGE GT 20 AND SIGNEDBY(playerpubkey) THEN RETURN TRUE ENDIF ASSERT SIGNEDBY(lottopubkey) ASSERT SAMESTATE(1 6) LET lottorand=STATE(7) ASSERT ((HEX(lottorand) EQ lottorand) AND (LEN(lottorand) LTE 32)) LET requiredamount=@AMOUNT/odds RETURN VERIFYOUT(@INPUT @ADDRESS requiredamount @TOKENID TRUE) ELSEIF round EQ 2 THEN IF @COINAGE GT 20 AND SIGNEDBY(lottopubkey) THEN RETURN TRUE ENDIF ASSERT SIGNEDBY(playerpubkey) LET preimage=STATE(8) LET checkhash=SHA3(preimage) ASSERT checkhash EQ secret LET lottorand=PREVSTATE(7) LET decider=SHA3(CONCAT(preimage lottorand)) LET hexsubset=SUBSET(0 8 decider) LET numvalue=NUMBER(hexsubset) LET maxvalue=NUMBER(0xFFFFFFFFFFFFFFFF) LET target=FLOOR(maxvalue*odds) LET iswin=numvalue LTE target IF iswin THEN RETURN TRUE ELSE RETURN VERIFYOUT(@INPUT lottoaddress @AMOUNT @TOKENID TRUE) ENDIF ENDIF";
var LOTTERY_GAME_ADDRESS 	= "MxG08743CMGQZ27HWANK8VJJZF44689YG3V0H0STTPBYA0MEYYTH4MADY0FWBNQ";

function addLotteryAdvertAddress(callback){
	MDS.cmd("newscript trackall:false script:\""+LOTTERY_ADVERT_SCRIPT+"\"", function(resp){
		MDS.cmd("newscript trackall:false script:\""+LOTTERY_GAME_SCRIPT+"\"", function(resp){
			if(callback){
				callback();
			}
		});	
	});
}

function createAdvertTxn(pubkey, lottoaddress,  odds, min, max, fee, uid, callback){
	
	//Construct the state variables
	var state = {};
	state[0] = ""+pubkey;
	state[1] = ""+odds;
	state[2] = ""+min;
	state[3] = ""+max;
	state[4] = ""+fee;
	state[5] = ""+uid;
	state[6] = ""+lottoaddress;
	
	var statestr = JSON.stringify(state);
	
	var sendcommand = "send amount:"+LOTTERY_ADVERT_AMOUNT+" address:"+LOTTERY_ADVERT_ADDRESS+" state:"+statestr;
	
	MDS.cmd(sendcommand,function(resp){
		if(!resp.status){
			//Something went wrong..
		}
		callback(resp.status);	
	});
}

function cancelAdvertTxn(uid, callback){
	
	//First get the coin..
	MDS.cmd("getaddress;coins simplestate:true relevant:true address:"+LOTTERY_ADVERT_ADDRESS,function(resp){
		
		//Get an address..
		var myaddress = resp[0].response.address; 
		
		var len = resp[1].response.length;
		for(var i=0;i<len;i++){
			
			//Get the coin
			var coin = resp[1].response[i];
			
			//Check the state UID
			var gameuid = coin.state[5];
			if(gameuid == uid){
				
				//Get the coinid
				var pubkey  = coin.state[0];
				var coinid 	= coin.coinid;
				var amount 	= coin.amount;
				
				//Now create the spend txn
				var txnname = "cancel_"+coinid;
				var creator  = "";
				creator 	+= "txndelete id:"+txnname;
				creator 	+= ";txncreate id:"+txnname;
				creator 	+= ";txninput id:"+txnname+" coinid:"+coinid;
				creator 	+= ";txnoutput id:"+txnname+" amount:"+amount+" address:"+myaddress;
				creator 	+= ";txnsign id:"+txnname+" publickey:"+pubkey;			
				creator 	+= ";txnpost id:"+txnname+" auto:true";
				creator 	+= ";txndelete id:"+txnname;

				MDS.cmd(creator,function(resp){
					if(callback){
						callback(true);	
					}	
				});
				
				return;					
			} 
		}
		
		//Coin not found!
		callback(false);
	});
}

function checkAdvertTxn(mylotteries, callback){
	
	//How many lotteries
	var lottolen = mylotteries.length;
	
	//First get the coin..
	MDS.cmd("block;coins simplestate:true relevant:true address:"+LOTTERY_ADVERT_ADDRESS,function(resp){
		
		//Get the current block
		var block = resp[0].response.block;
		
		var len = resp[1].response.length;
		for(var i=0;i<len;i++){
			
			//Get the coin
			var coin = resp[1].response[i];
			
			//Check the state UID
			var gameuid = coin.state[5];
			
			//Cycle through My Lotterries..
			var found = false;
			for(var l=0;l<lottolen;l++){
				if(gameuid == mylotteries[l].UID){
					
					//Check the coin age..
					var coinage = Number(block) - Number(coin.created); 
					
					MDS.log("ADVERT FOUND age : "+coinage);
					
					//Found one..
					if(coinage > 100){
						MDS.log("CREATE NEW ADVERT FOUND age : "+coinage);
						found = true;	
					}
					break;
				}
			}	
		
			if(found){
				
				//Get the coinid
				var pubkey  = coin.state[0];
				var coinid 	= coin.coinid;
				var address	= coin.address;
				var amount 	= coin.amount;
				var state 	= coin.state;
				
				//Now create the spend txn
				var txnname = "resubmit_"+coinid;
				var creator = "";
				creator 	+= "txndelete id:"+txnname;
				creator 	+= ";txncreate id:"+txnname;
				creator 	+= ";txninput id:"+txnname+" coinid:"+coinid;
				creator 	+= ";txnoutput id:"+txnname+" amount:"+amount+" address:"+address;
				
				//Add ALL the state vars..
				for(var st=0;st<7;st++){
					creator 	+= ";txnstate id:"+txnname+" port:"+st+" value:"+coin.state[st];	
				}
				
				//And finish up..
				creator 	+= ";txnsign id:"+txnname+" publickey:"+pubkey;			
				creator 	+= ";txnpost id:"+txnname+" auto:true";
				creator 	+= ";txndelete id:"+txnname;
			
				//Do it..
				MDS.log("RESUBMITTING : "+txnname)	
				MDS.cmd(creator, function(resp){
					//MDS.log(JSON.stringify(resp))
				});
			}
		}
	});
}

function findLottoGame(gameuid, callback){
	
	//Get ALL the games
	MDS.cmd("coins simplestate:true address:"+LOTTERY_ADVERT_ADDRESS,function(resp){
		
		var len = resp.response.length;
		for(var i=0;i<len;i++){
			
			var coin = resp.response[i];
			var uid  = coin.state[5];
			if(gameuid == uid){
				//found it..
				callback(coin);
				break;
			}
		}
	});
}

function playLottoGame(mypubkey, secret, odds, lottopubkey, lottoaddress, uid, amount, callback){
	
	//Construct the state variables
	var state = {};
	state[0] = "0";
	state[1] = ""+mypubkey;
	state[2] = ""+secret;
	state[3] = ""+odds;
	state[4] = ""+lottopubkey;
	state[5] = ""+lottoaddress;
	state[6] = ""+uid;
	
	var statestr = JSON.stringify(state);
	
	var sendcommand = "send address:"+LOTTERY_GAME_ADDRESS+" amount:"+amount+" state:"+statestr;
	
	MDS.cmd(sendcommand,function(resp){
		if(!resp.status){
			//Something went wrong..
		}
		callback(resp.status);	
	});
}

function checkAllGames(){
	
	MDS.cmd("block;coins simplestate:true address:"+LOTTERY_GAME_ADDRESS+" relevant:true",function(resp){
			
		var block 	= Number(resp[0].response.block);
		var coins 	= resp[1].response;
		var length 	= coins.length;
		
		for(var i=0;i<length;i++){
			
			//What round is it..
			var coin 	= coins[i];
			var round 	= coin.state[0];
			
			//How old is it..
			var age  = block - Number(coin.created);
			
			//Check Old enough
			if(age>5){
				//This is a game
				if(round == "0"){
					checkRoundOneGame(block,coin);	
				}else if(round == "1"){
					checkRoundTwoGame(block,coin);
				}else{
					MDS.log("[!] INVALID GAME ROUND FOR COIN : "+coin.coinid);
				}	
			}
		}
	});
}

function checkRoundOneGame(blocknum, coin){
	
	//The 2 players
	var playerpubkey 	= coin.state[1];
	var lottopubkey 	= coin.state[4];
	
	//How old is this round
	var age  			= blocknum - Number(coin.created); 
	
	//Check this IS actually a PLAYER game
	var cmdstr 	 = "keys action:list publickey:"+playerpubkey;
	cmdstr 		+= ";keys action:list publickey:"+lottopubkey;
	cmdstr 		+= ";getaddress";
	cmdstr 		+= ";random";
	
	//Run it..
	MDS.cmd(cmdstr,function(resp){
		
		//Which player are we  - can be both..
		var isplayer = resp[0].response.total>0;
		var islotto  = resp[1].response.total>0;
		var address	 = resp[2].response.address;
		var rand     = resp[3].response.random;
		
		//Is it one of ours..
		if(false && isplayer && age>20){
			
			//It's one of ours.. as PLAYER.. collect it..
			collectExpiredGame(1, coin, address, playerpubkey);
				
		}else if(islotto){
			
			//We are the lotto..
			lottoRoundOne(coin,lottopubkey,rand);
		}
	}); 
}

function collectExpiredGame(round, coin, address, pubkey){
	
	var txnname="collect_"+coin.coinid;
	
	var creator  = "";
	creator		+= "txndelete id:"+txnname;
	creator		+= ";txncreate id:"+txnname;
	creator		+= ";txninput id:"+txnname+" coinid:"+coin.coinid;
	creator		+= ";txnoutput id:"+txnname+" amount:"+coin.amount+" address:"+address;
	
	//Set the round state var..
	creator += ";txnstate id:"+txnname+" port:0 value:"+round;
	
	//Sign and post and delete
	creator		+= ";txnsign id:"+txnname+" publickey:"+pubkey;
	creator		+= ";txnpost id:"+txnname+" auto:true";
	creator		+= ";txndelete id:"+txnname;
	
	MDS.log("COLLECT EXPIRED COIN!");
	MDS.cmd(creator,function(resp){
		//MDS.log(JSON.stringify(resp));
	});
}

function lottoRoundOne(coin, random){
	
	//Check details are valid - odds min max..
	var uid = coin.state[6];
	
	//The pubkey and address
	var lottopubkey  = coin.state[4];
	var lottoaddress = coin.state[5];
	
	//Check the address
	MDS.cmd("checkaddress address:"+lottoaddress,function(resp){
		if(resp.status){
			if(resp.response.simple){
				//It's one ot yours
				loadLottery(uid,function(sqlres){
					//MDS.log("LOTTERY:"+JSON.stringify(rows));	
					if(sqlres.count==1){
						
						//Get the data
						var row = sqlres.rows[0]; 
						
						//Found the game.. check the values..
						var coinamount 	= Number(coin.amount);
						var coinodds 	= Number(coin.state[3]);
						
						var min	 = Number(row.MIN);
						var max	 = Number(row.MAX); 
						var odds = Number(row.ODDS);
						
						var valid = true;
						if(coinodds != odds){
							valid = false;
						}else if(coinamount<min || coinamount>max){
							valid = false;
						}
						
						//CHECK ADDRESS AND PUBKEY!!
						//..
						
						//Is it a valid game..
						if(!valid){
							MDS.log("[!] INVALID GAME COIN FOUND "+JSON.stringify(row)+" "+JSON.stringify(coin));
						}else{
							
							//Lets play!
							MDS.log("LETS PLAY! amount:"+coinamount+" odds:"+odds+" lottopubkey:"+pubkey);
			
							var requiredamount 	= coinamount / odds;
							var difference 		= requiredamount - coinamount;  
					
							var txnname="lotto1_"+coin.coinid;
				
							var creator  = "";
							creator		+= "txndelete id:"+txnname;
							creator		+= ";txncreate id:"+txnname;
							
							creator		+= ";txninput id:"+txnname+" coinid:"+coin.coinid;
							creator		+= ";txnoutput id:"+txnname+" amount:"+requiredamount+" address:"+coin.address;
							
							//Set the round state var
							creator += ";txnstate id:"+txnname+" port:0 value:1";
							
							//Add all the current state vars.. except state
							for(var st=1;st<7;st++){
								creator += ";txnstate id:"+txnname+" port:"+st+" value:"+coin.state[st];	
							}
							
							//And now add our OWN random number
							creator += ";txnstate id:"+txnname+" port:7 value:"+random;
							
							//NOW - add the difference of the amount.. ONLY the change sent back
							creator += ";txnaddamount id:"+txnname+" amount:"+difference+" onlychange:true";
							
							//Now sign automagically all the added coin from txnaddamount
							creator		+= ";txnsign id:"+txnname+" publickey:auto";
							
							//Now SIGN with the LOTTO pubkey
							creator		+= ";txnsign id:"+txnname+" publickey:"+lottopubkey;
							
							//Post and delete
							creator		+= ";txnpost id:"+txnname+" auto:true";
							creator		+= ";txndelete id:"+txnname;
							
							MDS.cmd(creator,function(resp){
								MDS.log("BASE CREATED!");
							});
						}
					}else{
						MDS.log("GAME NOT FOUND! uid:"+uid);
					}
				});			
			}
		}
	});	
}

function checkRoundTwoGame(blocknum, coin){
	
	//The 2 players
	var playerpubkey 	= coin.state[1];
	var lottopubkey 	= coin.state[1];
	
	//How old is this round
	var age  			= blocknum - Number(coin.created); 
	
	//Check this IS actually a PLAYER game
	var cmdstr 	 = "keys action:list publickey:"+playerpubkey;
	cmdstr 		+= ";keys action:list publickey:"+lottopubkey;
	cmdstr 		+= ";getaddress";
	
	//Run it..
	MDS.cmd(cmdstr,function(resp){
		
		//Which player are we  - can be both..
		var isplayer = resp[0].response.total>0;
		var islotto  = resp[1].response.total>0;
		var address	 = resp[2].response.address;
		
		//Is it one of ours..
		if(islotto && age>64){
			
			//It's one of ours.. as PLAYER.. collect it..
			collectExpiredGame(2,coin,address,lottopubkey);
				
		}else if(isplayer){
			
			//We are the lotto..
			//playerRoundTwo(coin,playerpubkey);
		}
	});
}

function playerRoundTwo(coin,pubkey){
	
}