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
	mainhost : "",
	
	//The MiniDAPP UID
	minidappuid : "",
	
	//Is logging RPC enabled
	logging : false,
	
	//When debuggin you can hard set the Host and port
	DEBUG_HOST : null,
	DEBUG_PORT : -1,
	
	//An allowed TEST Minidapp ID for SQL - can be overridden
	DEBUG_MINIDAPPID : "0x00",
	
	/**
	 * Minima Startup - with the callback function used for all Minima messages
	 */
	init : function(callback){
		//Log a little..
		MDS.log("Initialising MDS..");
		
		//Is logging enabled.. via the URL
		if(MDS.form.getParams("MDS_LOGGING") != null){
			MDS.logging = true;
		}
		
		//Get the host and port..
		var host = window.location.hostname;
		var port =  Math.floor(window.location.port);
		
		//Get ther MiniDAPP UID
		MDS.minidappuid = MDS.form.getParams("uid");
		
		//HARD SET if debug mode - running from a file
		if(MDS.DEBUG_HOST != null){
			MDS.log("DEBUG Settings Found..");
		
			host=MDS.DEBUG_HOST;
			port=MDS.DEBUG_PORT;	
		}
		
		if(MDS.minidappuid == null){
			MDS.minidappuid = MDS.DEBUG_MINIDAPPID;
		}
		
		//Is one specified..
		if(MDS.minidappuid == "0x00"){
			MDS.log("No MiniDAPP UID specified.. using test value");
		}
		
		//The ports..
		var mainport 	= port+1;
		
		MDS.log("MDS FILEHOST  : https://"+host+":"+port+"/");
		
		MDS.mainhost 	= "https://"+host+":"+mainport+"/";
		MDS.log("MDS MAINHOST : "+MDS.mainhost);
		
		//Store this for poll messages
		MDS_MAIN_CALLBACK = callback;
		
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
	 * Notify the User - on Phone it pops up in status bar. On desktop appears in Logs
	 */
	notify : function(output){
		//Send via POST
		httpPostAsync(MDS.mainhost+"notify?"+"uid="+MDS.minidappuid, output);
	},
	
	/**
	 * Cancel this MiniDAPPs notification
	 */
	notifycancel : function(){
		//Send via POST
		httpPostAsync(MDS.mainhost+"notifycancel?"+"uid="+MDS.minidappuid, "*");
	},
	
	/**
	 * Runs a function on the Minima Command Line - same format as MInima
	 */
	cmd : function(command, callback){
		//Send via POST
		httpPostAsync(MDS.mainhost+"cmd?"+"uid="+MDS.minidappuid, command, callback);
	},
	
	cmdsync : function(command){
		//Send via POST
		return httpPostSync(MDS.mainhost+"cmd?"+"uid="+MDS.minidappuid, command);
	},
	
	/**
	 * Runs a SQL command on this MiniDAPPs SQL Database
	 */
	sql : function(command, callback){
		//Send via POST
		httpPostAsync(MDS.mainhost+"sql?"+"uid="+MDS.minidappuid, command, callback);
	},
	
	/**	
	 * Network Commands
	 */
	net : {
		
		/**
		 * Make a GET request
		 */
		GET : function(url, callback){
			//Send via POST
			httpPostAsync(MDS.mainhost+"net?"+"uid="+MDS.minidappuid, url, callback);	
		},
		
		/**
		 * Make a POST request
		 */
		POST : function(url, data, callback){
			
			//Create the sinlg eline version..
			var postline = url+"&"+data;
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"netpost?"+"uid="+MDS.minidappuid, postline, callback);	
		}
		
	},
	
	/**	
	 * COMMS - send a message to ALL minidapps or JUST your own service.js
	 */
	comms : {
		
		/**
		 * PUBLIC message broadcast to ALL (callback is optional)
		 */
		broadcast : function(msg, callback){
			
			//Create the single line
			var commsline = "public&"+msg;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"comms?"+"uid="+MDS.minidappuid, commsline, callback);	
		},
		
		/**
		 * PRIVATE message send just to this MiniDAPP (callback is optional)
		 */
		solo : function(msg, callback){
			
			//Create the single line
			var commsline = "private&"+msg;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"comms?"+"uid="+MDS.minidappuid, commsline, callback);	
		}
		
	},
	
	/**
	 * File access
	 */
	file : {
		/**
		 * List file in a folder .. start at /
		 */
		list : function(folder, callback){
			
			//Create the single line
			var commsline = "list&"+folder;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Save text - can be text, a JSON in string format or hex encoded data
		 */
		save : function(filename, text, callback){
			
			//Create the single line
			var commsline = "save&"+filename+"&"+text;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Save Binary Data - supply as a HEX string
		 */
		savebinary : function(filename, hexdata, callback){
			
			//Create the single line
			var commsline = "savebinary&"+filename+"&"+hexdata;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Load text - can be text, a JSON in string format or hex encoded data
		 */
		load : function(filename, callback){
			
			//Create the single line
			var commsline = "load&"+filename;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Load Binary data - returns the HEX data
		 */
		loadbinary : function(filename, callback){
			
			//Create the single line
			var commsline = "loadbinary&"+filename;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Delete a file
		 */
		delete : function(filename, callback){
			
			//Create the single line
			var commsline = "delete&"+filename;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Get the full path - if you want to run a command on the file / import a txn / unsigned txn etc
		 */
		getpath : function(filename, callback){
			
			//Create the single line
			var commsline = "getpath&"+filename;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Make a directory
		 */
		makedir : function(filename, callback){
			
			//Create the single line
			var commsline = "makedir&"+filename;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Copy a file
		 */
		copy : function(filename, newfilename, callback){
			
			//Create the single line
			var commsline = "copy&"+filename+"&"+newfilename;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		},
		
		/**
		 * Move a file
		 */
		move : function(filename, newfilename, callback){
			
			//Create the single line
			var commsline = "move&"+filename+"&"+newfilename;		
			
			//Send via POST
			httpPostAsync(MDS.mainhost+"file?"+"uid="+MDS.minidappuid, commsline, callback);
		}
		
	}, 
	
	/**
	 * Function for GET parameters..
	 */
	form : {
		
		//Return the GET parameter by scraping the location..
		getParams : function(parameterName){
			    var result = null,
		        tmp = [];
			    var items = window.location.search.substr(1).split("&");
			    for (var index = 0; index < items.length; index++) {
			        tmp = items[index].split("=");
			        //console.log("TMP:"+tmp);
				   if (tmp[0] === parameterName) result = decodeURIComponent(tmp[1]);
			    }
			    return result;
		}		
	},
	
	/**
	 * UTILITY functions.. very useful
	 */
	util : {
		
		//Convert HEX to Base 64 - removes the 0x if necessary
		hexToBase64(hexstring) {
			//Check if starts with 0x
			var thex = hexstring;
			if(hexstring.startsWith("0x")){
				thex = hexstring.substring(2);
			}	
			
		    return btoa(thex.match(/\w{2}/g).map(function(a) {
		        return String.fromCharCode(parseInt(a, 16));
		    }).join(""));
		},
	
		//Convert Base64 to HEX	
		base64ToHex(str) {
			const raw = atob(str);
			let result = '';
			for (let i = 0; i < raw.length; i++) {
				const hex = raw.charCodeAt(i).toString(16);
				result += (hex.length === 2 ? hex : '0' + hex);
			}
			return result.toUpperCase();
		},
		
		//Convert Base64 to a Uint8Array - useful for Blobs
		base64ToArrayBuffer(base64) {
		    var binary_string = window.atob(base64);
		    var len = binary_string.length;
		    var bytes = new Uint8Array(len);
		    for (var i = 0; i < len; i++) {
		        bytes[i] = binary_string.charCodeAt(i);
		    }
		    return bytes.buffer;
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
	
	//The POLL host
	var pollhost = MDS.mainhost+"poll?"+"uid="+MDS.minidappuid;
	var polldata = "series="+PollSeries+"&counter="+PollCounter;
	
	httpPostAsyncPoll(pollhost,polldata,function(msg){
		
		//Are we on the right Series..
		if(PollSeries != msg.series){
			
			//Reset to the right series.. 
			PollSeries  = msg.series;
			PollCounter = msg.counter;
			
		}else{
			
			//Is there a message ?
			if(msg.status == true){
				
				//Get the current counter..
				PollCounter = msg.response.counter+1;
				
				//And Post the message..
				MDSPostMessage(msg.response.message);	
			}	
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
	xmlHttp.overrideMimeType('text/plain; charset=UTF-8');
    //xmlHttp.setRequestHeader('Content-Type', 'application/json');    
	xmlHttp.send(encodeURIComponent(params));
	//xmlHttp.send(params);
}

/**
 * Utility function for GET request
 * 
 * @param theUrl
 * @param callback
 * @param params
 * @returns
 */
function httpPostSync(theUrl, params){
	return new Promise((resolve, reject) => {
		var status = xmlHttp.status;
		if (xmlHttp.readyState == XMLHttpRequest.DONE){
			if (status === 0 || (status >= 200 && status < 400)) {
			
				//Do we log it..
	        	if(MDS.logging){
	        		MDS.log("RESPONSE:"+xmlHttp.responseText);
	        	}
	
	        	//Send it to the callback function..
	        	resolve(JSON.parse(xmlHttp.responseText));
	        	
			}else{
				//Some error..
				reject("ERROR");
			}
		}
	});
}

/**
 * Utility function for GET request (UNUSED for now..)
 * 
 * @param theUrl
 * @param callback
 * @returns
 */
/*function httpGetAsync(theUrl, callback)
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
}*/

function httpPostAsyncPoll(theUrl, params, callback){
	//Do we log it..
	if(MDS.logging){
		MDS.log("POST_POLL_RPC:"+theUrl+" PARAMS:"+params);
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
    xmlHttp.addEventListener('error', function(ev){
		MDS.log("Error Polling - reconnect in 10s");
		setTimeout(function(){PollListener();},10000);
	});
    xmlHttp.open("POST", theUrl, true); // true for asynchronous 
	xmlHttp.overrideMimeType('text/plain; charset=UTF-8');
    xmlHttp.send(encodeURIComponent(params));
}