
function jumpToNewMessage(){
	location.href="newmessage.html?uid="+MDS.minidappuid;	
}

function jumpToContacts(){
	location.href="contacts.html?uid="+MDS.minidappuid;	
}

function jumpToHome(){
	location.href="index.html?uid="+MDS.minidappuid;	
}

function showTitleOnAndroid(){
	if (window.navigator.userAgent.includes('Minima Browser')) {
		Android.showTitleBar();
	}
}

function safeDecodeString(str){
	return DOMPurify.sanitize(decodeStringFromDB(str));
}