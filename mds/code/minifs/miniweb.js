
/**
 * List of ALL the Data requests 
 */
var DATA_URI_REQUESTS = [];

/**
 * Initialise thew MiniWEB library
 */
function miniweb_Init(){
	//listen for post messages..
	window.onmessage = function(event){
		
		//What was the message
		var msg = event.data;
		if(msg){
			
			MDS.log("Inside got message : "+JSON.stringify(msg)+" DATASIZE:"+DATA_URI_REQUESTS.length);
			
			//Get the randid..
			var randid 	= msg.randid;
			
			//Now cycle..
			var len 	= DATA_URI_REQUESTS.length;
			for(var i=0;i<len;i++){
				var datauri = DATA_URI_REQUESTS[i];
				if(datauri.randid == randid){
					
					MDS.log("Found Data URI");
					
					//Call back..
					if(datauri.callback){
						datauri.callback(msg.data);
					}
				}
			}
			
			//Now remove that elemnt
			DATA_URI_REQUESTS = DATA_URI_REQUESTS.filter(function(item) {
			    return item.randid !== randid;
			});
			
			console.log("Size : "+DATA_URI_REQUESTS.length);
		}
	};
}

function miniweb_GetURL(minifile,callback){
	
	//First store this request in thelist..
	var request 		= {};
	request.randid		= Math.floor(Math.random() * 1000000000); 
	request.callback 	= callback;
	
	//Push it on the stack
	DATA_URI_REQUESTS.push(request);
	
	//Make a request msg to the top window..
	var msg 	= {};
	msg.action 	= "MINIWEB_GETMINIWEBURL";
	msg.data 	= minifile;
	msg.randid	= request.randid;
	
	//Send this to the parent window..
	window.top.postMessage(msg, '*');
}

function miniweb_GetDataURI(minifile,callback){
	
	//First store this request in thelist..
	var request 		= {};
	request.randid		= Math.floor(Math.random() * 1000000000); 
	request.callback 	= callback;
	
	//Push it on the stack
	DATA_URI_REQUESTS.push(request);
	
	//Make a request msg to the top window..
	var msg 	= {};
	msg.action 	= "MINIWEB_GETDATAURI";
	msg.data 	= minifile;
	msg.randid	= request.randid;
	
	//Send this to the parent window..
	window.top.postMessage(msg, '*');
}

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
	DATA_URI_REQUESTS.push(request);
	
	//Make a request msg to the top window..
	var msg 	= {};
	msg.action 	= "MINIWEB_JUMPTOURL";
	msg.data 	= minifile;
	msg.randid	= request.randid;
	
	//Send this to the parent window..
	window.top.postMessage(msg, '*');
}


