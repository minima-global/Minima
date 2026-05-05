/**
 * Navigation buttons
 */

function navigate_clear(){
	id_view_balance.style.display="none";
	id_view_send.style.display="none";
	id_view_receive.style.display="none";
	id_view_help.style.display="none";
}

function navigate_balance(){
	fetchBalance(function(){
		navigate_clear();
		id_view_balance.style.display="block";	
	});
}

function navigate_send(){
	navigate_clear();
	id_view_send.style.display="block";
}

function navigate_receive(){
	navigate_clear();
	id_view_receive.style.display="block";
}

function navigate_help(){
	navigate_clear();
	id_view_help.style.display="block";
}

function showBlackout(){
	document.getElementById('id_blackoutdiv').style.display="block";	
}

function hideBlackout(){
	document.getElementById('id_blackoutdiv').style.display="none";	
}

function showWaitDialog(){
	showBlackout();
	document.getElementById('id_sendinfo_panel').style.display="block";	
}

function hideWaitDialog(){
	hideBlackout();
	document.getElementById('id_sendinfo_panel').style.display="none";	
}

function showLoginDialog(){
	showBlackout();
	document.getElementById('id_login_dialog').style.display="block";	
}

function hideLoginDialog(){
	//Show the Blackout panel..
	hideBlackout();
	document.getElementById('id_login_dialog').style.display="none";	
}





