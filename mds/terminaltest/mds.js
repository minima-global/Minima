/**
* MDS JS lib for MiniDAPPs..
* 
* @spartacusrex
*/

/**
 * The MAIN Minima Callback function 
 */
var MDS_MAIN_CALLBACK = null;

/**
 * Main MINIMA Object for all interaction
 */
var MDS = {
	
	//RPC Host for Minima
	rpchost : "",
	
	//Polling server for messages
	pollhost : "",
	
	//SQL host
	sqlhost : "",
	
	//The MiniDAPP UID
	minidappuid : "",
	
	//Is logging RPC enabled
	logging : false,
	
	/**
	 * Minima Startup - with the callback function used for all Minima messages
	 */
	init : function(callback){
		//Log a little..
		MDS.log("Initialising MDS..");
		
		//Store this for websocket push messages
		MDS_MAIN_CALLBACK = callback;

		//Get ther MiniDAPP UID
		MDS.minidappuid = MDS.form.getParams("uid");
		MDS.log("MDS UID  : "+MDS.minidappuid);
		
		//Get the host and port..
		var host = window.location.hostname;
		var port =  Math.floor(window.location.port);
		
		//HARD SET
		host="127.0.0.1";
		port=9003;
		
		var rpcport  	= port-1;
		var pollport 	= port+1;
		var sqlport 	= port+2;
		
		MDS.rpchost 	= "http://"+host+":"+rpcport+"/";
		MDS.log("MDS RPCHOST  : "+MDS.rpchost);
		
		MDS.log("MDS MDSHOST  : http://"+window.location.host);
		
		MDS.pollhost 	= "http://"+host+":"+pollport+"/";
		MDS.log("MDS POLLHOST : "+MDS.pollhost);
		
		MDS.sqlhost 	= "http://"+host+":"+sqlport+"/";
		MDS.log("MDS SQLHOST : "+MDS.sqlhost);
		
		//Start the Long Poll listener
		PollListener();
		
		//And Post a message
		MDSPostMessage({ "event": "inited" });
	},
	
	/**
	 * Log some data with a timestamp in a consistent manner to the console
	 */
	log : function(output){
		console.log("Minima @ "+new Date().toLocaleString()+" : "+output);
	},
	
	/**
	 * Runs a function on the Minima Command Line - same format as MInima
	 */
	cmd : function(command, callback){
		//Send via POST
		httpPostAsync(MDS.rpchost, command, callback);
	},
	
	/**
	 * Runs a SQL command on this MiniDAPPs SQL Database
	 */
	sql : function(command, callback){
		//Send via POST
		httpPostAsync(MDS.sqlhost+"uid="+MDS.minidappuid, command, callback);
	},
	
	/**
	 * Form GET / POST parameters..
	 */
	form : {
		
		//Return the GET parameter by scraping the location..
		getParams : function(parameterName){
			    var result = null,
		        tmp = [];
			    var items = location.search.substr(1).split("&");
			    for (var index = 0; index < items.length; index++) {
			        tmp = items[index].split("=");
			        //console.log("TMP:"+tmp);
				   if (tmp[0] === parameterName) result = decodeURIComponent(tmp[1]);
			    }
			    return result;
		}		
	}
};

/**
 * Post a message to the Minima Event Listeners
 */
function MDSPostMessage(json){
   //And dispatch
   if(MDS_MAIN_CALLBACK){
		MDS_MAIN_CALLBACK(json);	
   }      
}


var PollCounter = 0;
var PollSeries  = 0;
function PollListener(){
	
	//MDS.log("START POLL");
	httpGetAsyncPoll(MDS.pollhost+"series="+PollSeries+"&counter="+PollCounter,function(msg){
		//MDS.log("END POLL : "+JSON.stringify(msg));
		
		//Are we on the right Series..
		if(PollSeries != msg.series){
			
			//Reset to the right series.. 
			PollSeries  = msg.series;
			PollCounter = msg.counter;
			
		}else{
			
			//Get the current counter..
			PollCounter = msg.response.counter+1;
			
			//And Post the message..
			MDSPostMessage(msg.response.message);	
		}
		
		//And around we go again..
		PollListener();
	});
}

/**
 * Utility function for GET request
 * 
 * @param theUrl
 * @param callback
 * @param params
 * @returns
 */
function httpPostAsync(theUrl, params, callback){
	//Do we log it..
	if(MDS.logging){
		MDS.log("POST_RPC:"+theUrl+" PARAMS:"+params);
	}

	var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200){
			//Do we log it..
        	if(MDS.logging){
        		MDS.log("RESPONSE:"+xmlHttp.responseText);
        	}

        	//Send it to the callback function..
        	if(callback){
        		callback(JSON.parse(xmlHttp.responseText));
        	}
        }
    }
    xmlHttp.open("POST", theUrl, true); // true for asynchronous 
    //xmlHttp.setRequestHeader('Content-Type', 'application/json');    
	xmlHttp.send(params);
}

/**
 * Utility function for GET request (UNUSED for now..)
 * 
 * @param theUrl
 * @param callback
 * @returns
 */
function httpGetAsync(theUrl, callback)
{	
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200){
        	if(MDS.logging){
				console.log("RPC      : "+theUrl);
				console.log("RESPONSE : "+xmlHttp.responseText);
			}

			//Always a JSON ..
        	var rpcjson = JSON.parse(xmlHttp.responseText);
        	
        	//Send it to the callback function..
        	if(callback){
        		callback(rpcjson);
        	}
        }
    }
	xmlHttp.open("GET", theUrl, true); // true for asynchronous 
    xmlHttp.send(null);
}

function httpGetAsyncPoll(theUrl, callback)
{	
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200){
        	callback(JSON.parse(xmlHttp.responseText));
        }
    }
	xmlHttp.addEventListener('error', function(ev){
		MDS.log("Error Polling - reconnect in 10s");
		setTimeout(function(){PollListener();},10000);
	});
	xmlHttp.open("GET", theUrl, true); // true for asynchronous 
    xmlHttp.send(null);
}