
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
	location.href="playground.html?uid="+MDS.minidappuid;	
}

function jumpToHelp(){
	location.href="help.html?uid="+MDS.minidappuid;	
}

function showTitleOnAndroid(){
	if (window.navigator.userAgent.includes('Minima Browser')) {
		Android.showTitleBar();
	}
}

function logJSON(json, title){
	if(title){
		MDS.log(title+" : "+JSON.stringify(json,null,2));
	}else{
		MDS.log(JSON.stringify(json,null,2));	
	}
	
}

function genRandomHexString() {
    const hex = '0123456789ABCDEF';
    let output = '';
    for (let i = 0; i < 16; ++i) {
        output += hex.charAt(Math.floor(Math.random() * hex.length));
    }
    return output;
}

function getTimeMilli(){
	//Date as of NOW
	var recdate = new Date();
	return recdate.getTime();	
}
