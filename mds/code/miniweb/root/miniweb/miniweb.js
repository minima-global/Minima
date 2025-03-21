
/**
 * List of all MinWeb requests 
 */
var MINIWEB_REQUESTS = [];

/**
 * Initialise the MiniWEB library
 */
function miniweb_Init(){
	
	//listen for post messages..
	window.onmessage = function(event){
		
		//What was the message
		var msg = event.data;
		if(msg){
			
			//Get the randid..
			var randid 	= msg.randid;
			
			//Now cycle..
			var len 	= MINIWEB_REQUESTS.length;
			var found 	= false;
			for(var i=0;i<len;i++){
				var datauri = MINIWEB_REQUESTS[i];
				if(datauri.randid == randid){
					found 	= true;
					
					//Call back..
					if(datauri.callback){
						datauri.callback(msg.data);
					}
				}
			}
			
			if(!found){
				MDS.log("MinWEB Request not found : "+JSON.stringify(msg));
			}
			
			//Now remove that elemnt
			MINIWEB_REQUESTS = MINIWEB_REQUESTS.filter(function(item) {
			    return item.randid !== randid;
			});
		}
	};
	
	//Post the current location to MiniWEB Parent
	var msg 	= {};
	msg.action 	= "MINIWEB_LOCATION";
	msg.data 	= window.location.href;
	
	//Send this to the parent window..
	window.top.postMessage(msg, '*');
}

/**
 * Load the data from ANY MiniFS file as a Data URI
 */
function miniweb_GetDataURI(minifile,callback){
	
	//First store this request in thelist..
	var request 		= {};
	request.randid		= Math.floor(Math.random() * 1000000000); 
	request.callback 	= callback;
	
	//Push it on the stack
	MINIWEB_REQUESTS.push(request);
	
	//Make a request msg to the top window..
	var msg 	= {};
	msg.action 	= "MINIWEB_GETDATAURI";
	msg.data 	= minifile;
	msg.randid	= request.randid;
	
	//Send this to the parent window..
	window.top.postMessage(msg, '*');
}

/**
 * Jump to a specific miniweb://.. page
 */
function miniweb_JumpToURL(minifile,callback){
	
	//First store this request in thelist..
	var request 		= {};
	request.randid		= Math.floor(Math.random() * 1000000000); 
	if(callback){
		request.callback 	= callback;	
	}else{
		request.callback 	= null;
	}
		
	//Push it on the stack
	MINIWEB_REQUESTS.push(request);
	
	//Make a request msg to the top window..
	var msg 	= {};
	msg.action 	= "MINIWEB_JUMPTOURL";
	msg.data 	= minifile;
	msg.randid	= request.randid;
	
	//Send this to the parent window..
	window.top.postMessage(msg, '*');
}


