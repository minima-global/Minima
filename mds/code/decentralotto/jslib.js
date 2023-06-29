
var LOTTO_PUBKEY 	= "0x5FC0D79C24F81585578D0B73A7A6B3A93ED660283A7982BB7BA6C2221985A429";
var PLAYER_PUBKEY 	= "0xAE9C00464089C835ED73A9A7EC4375612E5CF0B4AE883FB6E9F32FF2C1A614DD";

var randomhash 	= "0x3D9DF83D2E053A04EB3B3D62CEEA2D6C0C85D050B1307AE5D339F4D1B9CF3CAA"; 
var hashsecret 	= "0x4A05726F61F3F046FCD3F542D8FCB3570B6DB8AC449C1799B05AE83EE20C7622";

var LOTTO_PUBLISH_ADDRESS = "0xD5A88ECA2195D1B2EAFB7AFB1A1D79F8EFFEC1676E1C12A86BE8410833927B55";
var LOTTO_COLLECT_ADDRESS = "MxG083Y0JUHYNR0YPSFZQQV4GEW2V0TA3VA93PYR4BESKGVAW0B0G94V06J6N0T";

var LOTTO_SCRIPT  = "LET round=STATE(0) LET prevround=PREVSTATE(0) ASSERT round EQ INC(prevround) LET playerpubkey=PREVSTATE(1) LET secret=PREVSTATE(2) LET odds=PREVSTATE(3) LET chance=1/odds LET lottopubkey=PREVSTATE(4) LET lottoaddress=PREVSTATE(5) IF round EQ 1 THEN IF SIGNEDBY(playerpubkey) THEN RETURN TRUE ENDIF ASSERT SAMESTATE(1 5) ASSERT ((HEX(STATE(6)) EQ STATE(6)) AND (LEN(STATE(6)) LTE 32)) LET requiredamount=@AMOUNT*odds RETURN VERIFYOUT(@INPUT @ADDRESS requiredamount @TOKENID TRUE) ELSEIF round EQ 2 THEN IF @COINAGE GT 64 AND SIGNEDBY(lottopubkey) THEN RETURN TRUE ENDIF LET preimage=STATE(7) LET checkhash=SHA3(preimage) ASSERT checkhash EQ secret LET decider=SHA3(CONCAT(preimage PREVSTATE(6))) LET hexsubset=SUBSET(0 6 decider) LET numvalue=NUMBER(hexsubset) LET maxvalue=NUMBER(0xFFFFFFFFFFFF) LET target=FLOOR(maxvalue*chance) LET iswin=numvalue LTE target IF iswin THEN RETURN SIGNEDBY(playerpubkey) ELSE RETURN VERIFYOUT(@INPUT lottoaddress @AMOUNT @TOKENID TRUE) ENDIF ENDIF";
var LOTTO_ADDRESS = "MxG080QFF578S7GRHSGRW80WUPPGFU7WFQBF581JESGE431ZKU0NRHUFBFGMTVK";

function startlotto(message,odds,min,max){
	
	//Construct the state variables
	var state = {};
	state[0] = LOTTO_PUBKEY;
	state[1] = "["+encodeURIComponent(message)+"]";
	state[2] = ""+odds;
	state[3] = ""+min;
	state[4] = ""+max;
	
	var statestr = JSON.stringify(state);
	
	//Now send out a txn publishing your Lotto..
	var sendcommand = "send amount:0.0000000000000000000001 address:0xD5A88ECA2195D1B2EAFB7AFB1A1D79F8EFFEC1676E1C12A86BE8410833927B55 state:"+statestr;

	MDS.cmd(sendcommand,function(resp){
		MDS.log(JSON.stringify(resp));
	});
}


function startgame(){
	
	//Create a random HASH and hash it....
	MDS.cmd("random",function(resp){
		MDS.log(JSON.stringify(resp));
		
		//var random = resp.response.random;
		//var secret = resp.response.hashed;
		var random = randomhash;
		var secret = hashsecret;
		
		//Construct the state variables
		var state = {};
		state[0] = ""+0;
		state[1] = PLAYER_PUBKEY;
		state[2] = secret;
		state[3] = "2";
		state[4] = LOTTO_PUBKEY;	
		state[5] = LOTTO_COLLECT_ADDRESS;
		
		var statestr = JSON.stringify(state);
		
		var sendcommand = "send amount:1 address:"+LOTTO_ADDRESS+" state:"+statestr;
		
		MDS.cmd(sendcommand, function(resp){
			MDS.log(JSON.stringify(resp));
		});
		
	});
}

function roundOne(coinid){
	
	MDS.cmd("coins coinid:"+coinid+" simplestate:true",function(resp){
		//Get the coin details
		var coin = resp.response[0];
		
		//Now create the next txn..
		var newstate = coin.state;
		newstate[6]  = "0x000000"
		
		//What are the odds..
		var odds = 
		
		//Create the txn..
		var creator = "";
		creator += "txncreate id:roundone;";
		creator += "txninput id:roundone coinid:"+coinid+";";
		
		//Add more..
		
		//And output
		creator += "txnoutput id:roundone amount:"+coin.amount+" address:"+coin.address+" storestate:true;";
		
	});
	
}