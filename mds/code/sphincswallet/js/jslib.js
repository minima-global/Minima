
function receiveAddresQR(address){
	
	var qrcode = new QRCode("wallet_receiveqr", {
		    text: address,
		    width: 300,
		    height: 300,
		    colorDark : "#000000",
		    colorLight : "#ffffff",
		    correctLevel : QRCode.CorrectLevel.H
		});	
		
	document.getElementById('id_wallet_address').innerHTML=address;
	
}

function sanitize(str){
	return DOMPurify.sanitize(str); 
}