
var LOTTO_PUBKEY 	= "0x5FC0D79C24F81585578D0B73A7A6B3A93ED660283A7982BB7BA6C2221985A429";
var PLAYER_PUBKEY 	= "0xAE9C00464089C835ED73A9A7EC4375612E5CF0B4AE883FB6E9F32FF2C1A614DD";

var randomhash 	= "0x3D9DF83D2E053A04EB3B3D62CEEA2D6C0C85D050B1307AE5D339F4D1B9CF3CAA"; 
var hashsecret 	= "0x4A05726F61F3F046FCD3F542D8FCB3570B6DB8AC449C1799B05AE83EE20C7622";

var LOTTO_PUBLISH_ADDRESS = "0xD5A88ECA2195D1B2EAFB7AFB1A1D79F8EFFEC1676E1C12A86BE8410833927B55";
var LOTTO_COLLECT_ADDRESS = "MxG083Y0JUHYNR0YPSFZQQV4GEW2V0TA3VA93PYR4BESKGVAW0B0G94V06J6N0T";


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
		
		MDS.log(statestr);	
	});
}