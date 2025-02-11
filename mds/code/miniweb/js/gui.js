
function showTitleOnAndroid(){
	if (window.navigator.userAgent.includes('Minima Browser')) {
		Android.showTitleBar();
	}
}

function jumpToHelp(){
	location.href="help.html?uid="+MDS.minidappuid;	
}

function jumpToHome(){
	location.href="index.html?uid="+MDS.minidappuid;	
}

function jumpToDev(){
	location.href="devmode.html?uid="+MDS.minidappuid;	
}

function jumpToMySite(){
	location.href="gensite.html?uid="+MDS.minidappuid;	
}

function startupButtons(){
	$( "#button-help" ).button({
		icon: "ui-icon-help",
		showLabel: false
	}).click(function(){jumpToHelp();});
	
    $( "#button-home" ).button({
		icon: "ui-icon-home",
		showLabel: false
	}).click(function(){jumpToHome();});
    
    $( "#button-dev" ).button({
		icon: "ui-icon-edit",
		showLabel: false
	}).click(function(){jumpToDev();});
	
	$( "#button-mysite" ).button({
		icon: "ui-icon-vcard",
		showLabel: false
	}).click(function(){jumpToMySite();});
}

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
	
	const img = "browser.png";
	const txt = message;
	const notification = new Notification("MiniWEB", { body: txt, icon: img });
}
