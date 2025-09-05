/**
 * Get your details for this session..
 */

var AUTH_MAXIMA_NAME 		= "";
var AUTH_MAXIMA_ID 			= "";
var AUTH_MINIMA_ADDRESS 	= "";
var AUTH_MINIMA_PUBLICKEY 	= "";

function initAuthDetails(callback){
	
	//Who are you..
	MDS.cmd("maxima;getaddress",function(authret){
		
		//Get the MAXIMA details..
		AUTH_MAXIMA_NAME = authret[0].response.name;
		AUTH_MAXIMA_ID 	 = authret[0].response.publickey;
		
		//Get the address and pub key
		AUTH_MINIMA_ADDRESS 	= authret[1].response.miniaddress;
		AUTH_MINIMA_PUBLICKEY 	= authret[1].response.publickey;
		
		if(callback){
			callback();
		}		
	});
}

function getUserDetails(){
	
	var user 		= {};
	user.name 		= AUTH_MAXIMA_NAME;
	user.maximaid 	= AUTH_MAXIMA_ID;
	user.address 	= AUTH_MINIMA_ADDRESS;
	user.publickey 	= AUTH_MINIMA_PUBLICKEY;
	
	return user;
}