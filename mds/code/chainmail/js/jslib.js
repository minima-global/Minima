
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

function makeDateString(timemilli){
	return new Date(+timemilli).toLocaleTimeString()+" "+new Date(+timemilli).toLocaleDateString();
}

function genRandomHexString(len) {
    const hex = '0123456789ABCDEF';
    let output = '';
    for (let i = 0; i < len; ++i) {
        output += hex.charAt(Math.floor(Math.random() * hex.length));
    }
    return output;
}