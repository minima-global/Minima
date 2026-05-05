
function receiveAddresQR(){
	
	var qrcode = new QRCode("wallet_receiveqr", {
		    text: SPHINCS_ADDRESS,
		    width: 300,
		    height: 300,
		    colorDark : "#000000",
		    colorLight : "#ffffff",
		    correctLevel : QRCode.CorrectLevel.H
		});	
		
	document.getElementById('id_wallet_address').innerHTML=SPHINCS_ADDRESS;
	
}

function sanitize(str){
	return DOMPurify.sanitize(str); 
}

function maxlength(str, len){
	if(str.length>len){
		return str.substring(0,16)+"..";
	}
	
	return str;
}