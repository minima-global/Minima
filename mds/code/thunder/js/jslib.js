
function startupButtons(){
	
	$( "#button-help" ).button({
		icon: "ui-icon-help",
		showLabel: false
	}).click(function(){jumpToHelp();});
	
    $( "#button-newchannel" ).button({
		icon: "ui-icon-circle-plus",
		showLabel: false
	}).click(function(){jumpToNewChannel();});
    
	$( "#button-history" ).button({
		icon: "ui-icon-history",
		showLabel: false
	}).click(function(){jumpToHistory();});
	
    $( "#button-home" ).button({
		icon: "ui-icon-home",
		showLabel: false
	}).click(function(){jumpToHome();});
}

function jumpToHome(){
	location.href="index.html?uid="+MDS.minidappuid;	
}

function jumpToHistory(){
	location.href="history.html?uid="+MDS.minidappuid;	
}

function jumpToNewChannel(){
	location.href="newchannel.html?uid="+MDS.minidappuid;	
}

function jumpToHelp(){
	location.href="help.html?uid="+MDS.minidappuid;	
}

function showTitleOnAndroid(){
	if (window.navigator.userAgent.includes('Minima Browser')) {
		Android.showTitleBar();
	}
}

/*
 * POPUP functions..
 */
var VALID_POPUP_RESPONSE 	= false;
var POPUP_TEXT 				= "";
function showPopup(showhide, text){
	//Show popup..
	if(showhide){
		main_popup.style.display="block";	
	}else{
		main_popup.style.display="none";
	}
	
	if(text){
		POPUP_TEXT = text;
		main_popup.innerHTML = text;
	}
}

function startTimerChecker(){
	setTimeout(function() { 
		if(!VALID_POPUP_RESPONSE){
			stopPopupDots();
			
			//Was not able to contact User.. 
			showPopup(true, "No response from User..<br><br><a href='' >Close Popup..</a>");	
		}
	}, 
	10000);
}

var DOTS_INTERVAL;
function startPopUpDots(){
	POPUP_TEXT +="<br><br>";
	DOTS_INTERVAL = setInterval(function(){
		showPopup(true, POPUP_TEXT+"<b>.</b>");
	},1000);
}

function stopPopupDots(){
	clearInterval(DOTS_INTERVAL);
}