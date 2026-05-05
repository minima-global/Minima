
function initSphincsWallet(callback){
	
	//Show the Login dialog
	showLoginDialog();
	
}

function init_continueseed(){
	
	//Get the seed
	var seed = id_init_seed.value.trim();
	if(seed == ""){
		alert("Cannot have a blank seed!");
		return;
	}
	
	//Disable the button..
	id_init_button.disabled 	= true;
	id_generate_button.disabled = true;
		
	//Now init
	initSPHINCS(seed, function(){
						
		//Set the address
		receiveAddresQR();
	
		//Fetch the balance
		fetchBalance(function(){
			hideLoginDialog();	
		});	
	});
}

function init_generateseed(){
	
	MDS.cmd("random", function(resp){
		id_init_generate.innerHTML=	"<b>"+resp.response.keycode+"</b><br><br><span style='font-size:0.8em;'>BACKUP THIS VALUE!!</span>";
	});
	
}