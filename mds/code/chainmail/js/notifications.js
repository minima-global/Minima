var NOTIFICATION_ENABLED = false;

function askNotificationPermission() {

	// Check if the browser supports notifications
	if (!("Notification" in window)) {
		console.log("This browser does not support notifications.");
		return;
	}
	
	Notification.requestPermission((result) => {
		if(result == "granted"){
			NOTIFICATION_ENABLED = true;		
		}
		console.log("Notifications : "+result);
	});
}

function showNotification(message){
	if(!NOTIFICATION_ENABLED){
		return;
	}
	
	const img = "./images/chainmail.webp";
	const txt = message;
	const notification = new Notification("ChainMail", { body: txt, icon: img });
}
