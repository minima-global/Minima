/**
* CHATTER backend service
* 
* @spartacusrex
*/

//Load a file..
MDS.load("./js/decimal.min.js");
MDS.load("./js/sql.js");
MDS.load("./js/txns.js");

//Check the age of your lottery advert
function checkAdvertAge(){
	//Load all my games
	loadMyLotteries(true,function(rows){
		//And now check those..
		checkAdvertTxn(rows,function(){});
	});
}
	
//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Create the MAIN DB
		createDB(function(){
			
			//Hard unlock
			hardUnlockTxn();
			
			//Add the lottery address
			addLotteryAdvertAddress();
			
			//Clean up the secrets
			cleanUpSecrets();
			
			MDS.log("Lotto Inited!");
		});
	
	//Check rechatter messages
	}else if(msg.event == "NEWBLOCK"){
		
		//What block
		var block = msg.data.txpow.header.block;
		
		//Check every 3 blocks - to give previous actions time to confirm
		if(block % 3 == 0){
			//Check the games..
			checkAllGames();
		}	
		
		//Check your advert
		if(block % 5 == 0){
			//Check Advert Coin Age
			checkAdvertAge();
		}
				
	}else if(msg.event == "NEWTXPOW"){
		
		//Check if this is a game..
		checkForGameEnd(msg.data.txpow)
		
	}
});
