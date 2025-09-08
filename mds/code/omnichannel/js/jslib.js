
function startupButtons(){
	
	$( "#button-help" ).button({
		icon: "ui-icon-help",
		showLabel: false
	}).click(function(){jumpToHelp();});
	
    $( "#button-newchannel" ).button({
		icon: "ui-icon-circle-plus",
		showLabel: false
	}).click(function(){jumpToNewChannel();});
    
    $( "#button-home" ).button({
		icon: "ui-icon-home",
		showLabel: false
	}).click(function(){jumpToHome();});
}

function jumpToHome(){
	location.href="index.html?uid="+MDS.minidappuid;	
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

//POPUP functions..
/*function showPopup(showhide, text){
	//Show popup..
	if(showhide){
		popup.style.display="block";	
	}else{
		popup.style.display="none";
	}
	
	if(text){
		popup.innerHTML = text;
	}
}

function refreshPage(){
	location.reload();
}
*/