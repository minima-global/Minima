var SPHINCS_PUBLICKEY 	= "";
var SPHINCS_PRIVATEKEY 	= "";
var SPHINCS_ADDRESS 	= "";
var SPHINCS_SCRIPT 		= "";
var SPHINCS_MEAGMMR		= false;

function initSPHINCS(seed, callback){
	
	//Create a SPHINCS wallet
	MDS.cmd("sphincs action:generate seed:\""+seed+"\"",function(resp){
	
		//Get the keys..
		SPHINCS_PUBLICKEY 	= resp.response.publickey;
		SPHINCS_PRIVATEKEY 	= resp.response.privatekey;
		SPHINCS_ADDRESS		= resp.response.address;
		SPHINCS_SCRIPT		= resp.response.script;
		
		//Are we running a MegaMMR - if not track the script..
		MDS.cmd("status", function(resp){
			
			SPHINCS_MEAGMMR = resp.response.megammr;
			if(!SPHINCS_MEAGMMR){
				
				//Add the script.. so that we track it..
				//MDS.cmd();
				
			}
			
		});
		
		//Finished setup
		if(callback){
			callback();
		}
	});
}

function showWaitDialog(){
	//Show the Blackout panel..
	document.getElementById('id_blackoutdiv').style.display="block";
	document.getElementById('id_sendinfo_panel').style.display="block";	
}

function hideWaitDialog(){
	//Show the Blackout panel..
	document.getElementById('id_blackoutdiv').style.display="none";
	document.getElementById('id_sendinfo_panel').style.display="none";	
}

function sphincs_sendfunds(){
	
	//Show the Blackout panel..
	showWaitDialog();
	
	//Get the token
	var selector 	= document.getElementById("id_wallet_tokenselect");
	var tokenname  	= selector.options[selector.selectedIndex].text;
	var tokenid  	= selector.options[selector.selectedIndex].value;
	
	//Get the details..
	var amount 		= id_wallet_send_amount.value;
	var address		= id_wallet_send_address.value;
	
	//Construct the command..
	var sendcmd = "sphincs action:transaction amount:"+amount+" address:"+address+" tokenid:"+tokenid+" privatekey:"+SPHINCS_PRIVATEKEY;
	
	//MDS.log(sendcmd);
	MDS.cmd(sendcmd, function(resp){
		if(!resp.status){
			alert("Error sending funds : \n\n"+resp.error);	
		}else{
			
			//Clear inputs..
			id_wallet_send_amount.value = "";
			id_wallet_send_address.value ="";
			
			alert("Funds Sent!");
		}
		
		hideWaitDialog();
	});
}
