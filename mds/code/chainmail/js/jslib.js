
function startupButtons(){
	
	$( "#button-help" ).button({
		icon: "ui-icon-help",
		showLabel: false
	}).click(function(){jumpToHelp();});
	
    $( "#button-newmail" ).button({
		icon: "ui-icon-mail",
		showLabel: false
	}).click(function(){jumpToNewMessage();});
    
    $( "#button-contacts" ).button({
		icon: "ui-icon-contact",
		showLabel: false
	}).click(function(){jumpToContacts();});
  
    $( "#button-home" ).button({
		icon: "ui-icon-home",
		showLabel: false
	}).click(function(){jumpToHome();});
}

function jumpToNewMessage(){
	location.href="newmessage.html?uid="+MDS.minidappuid;	
}

function jumpToContacts(){
	location.href="contacts.html?uid="+MDS.minidappuid;	
}

function jumpToHome(){
	location.href="index.html?uid="+MDS.minidappuid;	
}

function jumpToHelp(){
	location.href="help.html?uid="+MDS.minidappuid;	
}

function showTitleOnAndroid(){
	if (window.navigator.userAgent.includes('Minima Browser')) {
		Android.showTitleBar();
	}
}

function safeDecodeString(str){
	return DOMPurify.sanitize(decodeStringFromDB(str));
}

function makeDateString(timemilli){
	return new Date(+timemilli).toLocaleTimeString()+" "+new Date(+timemilli).toLocaleDateString();
}

function makeDateStringVertical(timemilli){
	return new Date(+timemilli).toLocaleTimeString()+"<br>"+new Date(+timemilli).toLocaleDateString();
}

function genRandomHexString(len) {
    const hex = '0123456789ABCDEF';
    let output = '';
    for (let i = 0; i < len; ++i) {
        output += hex.charAt(Math.floor(Math.random() * hex.length));
    }
    return output;
}

function checkMDSCOMMMS(msg){
	
	if(!msg.data.public){	
		try{
			//Parse the notification
			var notif = JSON.parse(msg.data.message);
			
			showNotification(notif.subject+"\n\n"
							+notif.from+" > "
							+notif.message);
			
		}catch(error){
			MDS.log("MDSCOMMS ERROR :"+error);
		}
	}
}

function checkValidMx(mxcheck, callback){
	MDS.cmd("convert from:mx to:hex data:"+mxcheck,function(resp){
		callback(resp.status);
	});
}