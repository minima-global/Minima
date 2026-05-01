
function fetchBalance(callback){
	
	//Fetch the balance
	MDS.cmd("balance address:"+SPHINCS_ADDRESS, function(resp){
		//MDS.log(JSON.stringify(resp));
		
		//Update the table
		updateBalanceTable(resp.response);
		
		if(callback){
			callback();
		}
	});
}

function updateBalanceTable(balance){
	
	var baltable 		= document.getElementById('id_balance_table');
	baltable.innerHTML 	= "";
	
	var tokenselect	= document.getElementById('id_wallet_tokenselect');
	tokenselect.innerHTML 	= "";
	
	//Set the Headers
	var row   = baltable.insertRow(0);
	row.insertCell().outerHTML = "<th class='smalltableheadertext'>Token</th>";
	row.insertCell().outerHTML = "<th class='smalltableheadertext'>Amount</th>"; 
		
	//Get my Orders
	var len = balance.length;
	for(var i=0;i<len;i++) {
		
		var tokenbal=balance[i];
		
		//Insert row
		var row = baltable.insertRow();
		row.style.fontSize 	= "0.8em";
		
		var celltoken 		= row.insertCell();
		var cellamount 		= row.insertCell();
		
		var tokenname = "";
		if(tokenbal.tokenid == "0x00"){
			tokenname = "Minima";
		}else{
			tokenname = tokenbal.token.name;
		}
		
		celltoken.innerHTML = sanitize(tokenname);
		celltoken.style.width="100%";
		
		if(tokenbal.unconfirmed != "0"){
			cellamount.innerHTML 	= sanitize(maxlength(tokenbal.confirmed,16)+" ("+maxlength(tokenbal.unconfirmed,16)+")");
		}else{
			cellamount.innerHTML 	= sanitize(maxlength(tokenbal.confirmed, 16));	
		}
		
		//Insert row
		var rowid 					= baltable.insertRow();
		var celltokenid 			= rowid.insertCell();
		celltokenid.colSpan 		= "4";
		celltokenid.style.fontSize 	= "0.7em";
		celltokenid.style.color 	= "grey";
		celltokenid.innerHTML 		= sanitize(tokenbal.tokenid);
		
		//Final gap
		var rowgap 	= baltable.insertRow();
		var rowgap 	= rowgap.insertCell();
		rowgap.innerHTML = "&nbsp;";
		
		//And sort the select
		var opt 		= document.createElement('option');
        opt.value 		= tokenbal.tokenid;
        opt.innerHTML 	= tokenname;
        tokenselect.appendChild(opt);
	}
}

