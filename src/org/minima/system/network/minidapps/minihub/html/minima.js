/**
* Minima JS lib for MiniDAPPs..
* 
* Includes the Decimal.js lib for precise Maths.
* 
* @spartacusrex
*/

/**
 * The MAIN Minima Callback function 
 */
var MINIMA_MAIN_CALLBACK = null;

/**
 * The MiniDAPP interfce Callback function 
 */
var MINIMA_MINIDAPP_CALLBACK = null;

/**
 * The Web Socket Host for PUSH messages
 */
var MINIMA_WEBSOCKET = null;

/**
 * GET or POST request parameters
 */
var MINIMA_PARAMS = {};

/**
 * NET socket port and functions
 */
var MINIMA_SERVER_LISTEN = [];
var MINIMA_USER_LISTEN   = [];

/**
 * Main MINIMA Object for all interaction
 */
var Minima = {
	/**
	 * Current Minima Block Height
	 */
	block : 0,
	
	/** 
	 * The TxPoWID of the current top block
	 */
	txpowid : "0x00",

	/** 
	 * The Full TxPoW Top Block
	 */
	txpow : {},

	/**
	 * Current Balance of this User
	 */
	balance : {},
	
	//Web Host for Minima
	webhost : "http://127.0.0.1:9004",
	
	//RPC Host for Minima
	rpchost : "http://127.0.0.1:9002",
	
	//Web Socket Host for Minima
	wshost : "ws://127.0.0.1:9003",
	
	//Show RPC commands
	logging : false,
	
	//Are we in DEBUG mode - if so don't touch the host settings..
	debug : false,
	
	/**
	 * Minima Startup - with the callback function used for all Minima messages
	 */
	init : function(callback){
		//Log a little..
		Minima.log("Initialising..");
		
		//Store the callback
		if(callback){
			MINIMA_MAIN_CALLBACK = callback;	
		}else{
			Minima.log("No Main Minima Callback specified..");
		}
		
		//Are we running via a server - otherwise leave as is
		if(!Minima.debug){
			if(window.location.protocol.startsWith("http")){
				//The Port determives the WebSocket and RPC port..
				Minima.webhost = "http://"+window.location.hostname+":"+(window.location.port);
				Minima.rpchost = "http://"+window.location.hostname+":"+(window.location.port-2);
				Minima.wshost = "ws://"+window.location.hostname+":"+(window.location.port-1);	
			}	
		}
		
		//Info.. 
		Minima.log("WEBHOST : "+Minima.webhost);
		Minima.log("RPCHOST : "+Minima.rpchost);
		Minima.log("WCHOST  : "+Minima.wshost);
		
		//Any Parameters..
		var paramstring = Minima.webhost+"/params";
		httpGetAsync(paramstring, function(jsonresp){
			//Set it..
			MINIMA_PARAMS = jsonresp;	
		});
		
		//Do the first call..
		Minima.cmd("topblock;balance", function(json){
			//Store this..
		    Minima.block  = parseInt(json[0].response.txpow.header.block,10);
		    Minima.txpow  = json[0].response.txpow;
		    
			//Status is first..
			Minima.balance = json[1].response.balance;
			
		    //Start Listening for messages..
			MinimaWebSocketListener();
		});
	},
	
	/**
	 * Log some data with a timestamp in a consistent manner to the console
	 */
	log : function(output){
		console.log("Minima @ "+new Date().toLocaleString()+" : "+output);
	},
	
	/**
	 * Notify the user with a Pop up message
	 */
	notify : function(message,bgcolor){
		//Log it..
		Minima.log("Notify : "+message);
		
		//Show a little popup across the screen..
		if(bgcolor){
			MinimaCreateNotification(message,bgcolor);
		}else{
			MinimaCreateNotification(message);	
		}
	},
	
	/**
	 * Runs a function on the Minima Command Line
	 */
	cmd : function(minifunc, callback){
		MinimaRPC("cmd",minifunc,callback);
	},
	
	/**
	 * Run SQL in the Database created for this MiniDAPP
	 */
	sql : function(query, callback){
		MinimaRPC("sql",query,callback);
	},
	
	/**
	 * NETWORK Functions
	 */
	net : {
		
		//SERVER FUNCTIONS
		onInbound : function(port, onReceiveCallback){
			MINIMA_SERVER_LISTEN.push({ "port":port, "callback":onReceiveCallback });
		},
		
		start : function(port){
			MinimaRPC("net","listen "+port,null);
		},
		
		stop : function(port){
			MinimaRPC("net","stop "+port,null);
		},
		
		broadcast : function(port,text){
			MinimaRPC("net","broadcast "+port+" "+text,null);
		},
		
		//USER FUNCTIONS 
		onOutbound : function(hostport, onReceiveCallback){
			MINIMA_USER_LISTEN.push({ "port":hostport, "callback":onReceiveCallback });
		},
		
		connect : function(hostport){
			MinimaRPC("net","connect "+hostport,null);
		},
		
		disconnect : function(UID){
			MinimaRPC("net","disconnect "+UID,null);
		},
		
		send : function(UID, text){
			MinimaRPC("net","send "+UID+" "+text,null);
		},
		
		//Resend all the connection information
		info : function(){
			MinimaRPC("net","info", null);
		},

		//Receive all info in the callback
		stats : function(callback){
			MinimaRPC("net","stats",callback);
		},
				
		//GET an URL
		GET : function(url, callback){
			MinimaRPC("net","get "+url,callback);
		},
		
		//POST params to an URL 
		POST : function(url, params, callback){
			MinimaRPC("net","post "+url+" "+params,callback);
		}
		
	},
	
	
	/**
	 * FILE Functions - no spaces allowed in filenames
	 */ 
	file : {
		
		//Save & Load Text to a file 
		save : function(text, file,  callback) {
			MinimaRPC("file","save "+file+" "+text,callback);
		},
		
		load : function(file, callback) {
			MinimaRPC("file","load "+file,callback);
		},
		
		//Save and Load as HEX.. Strings with 0x..
		saveHEX : function(hextext, file,  callback) {
			MinimaRPC("file","savehex "+file+" "+hextext,callback);
		},
		
		loadHEX : function(file, callback) {
			MinimaRPC("file","loadhex "+file,callback);
		},
		
		//Copy file..
		copy : function(file, newfile, callback) {
			MinimaRPC("file","copy "+file+" "+newfile,callback);
		},
		
		//Rename a file in your folder
		move : function(file, newfile, callback) {
			MinimaRPC("file","move "+file+" "+newfile,callback);
		},
		
		//List the files in a directory
		list : function(file, callback) {
			MinimaRPC("file","list "+file,callback);
		},
		
		//Delete a File
		delete : function(file, callback) {
			MinimaRPC("file","delete "+file,callback);
		}
			
	},
	
	/**
	 * Form GET / POST parameters..
	 */
	form : {
		
		//BOTH POST and GET parameters.. and any files are uploaded to /upload folder
		//must set POST form to multipart/form-data to work.. 
		params : function(paramname){
			return MINIMA_PARAMS[paramname];
		},
		
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
	},
	
	/**
	 * Intra MiniDAPP communication
	 */
	minidapps : {
		//List the currently installed minidapps
		list : function(callback){
			Minima.cmd("minidapps list",callback);
		},
		
		//Function to call when an Intra-MiniDAPP message is received
		listen : function(onReceiveCallback){
			MINIMA_MINIDAPP_CALLBACK = onReceiveCallback;
		},
		
		//Send a message to a specific minidapp
		send : function(minidappid,message, callback){
			Minima.cmd("minidapps post:"+minidappid+" \""+message+"\"",callback);
		},
		
		//The replyid is in the original message
		reply : function(replyid,message){
			//Reply to a POST message..
			replymsg = { "type":"reply", "message": message, "replyid" : replyid };
			MINIMA_WEBSOCKET.send(JSON.stringify(replymsg));
		}

	},
	
	/**
	 * UTILITY FUNCTIONS
	 */
	util : {
			//Get the Balance string for a Tokenid..
			getBalance : function(tokenid){
				var ballen = Minima.balance.length;
				for(balloop=0;balloop<ballen;balloop++){
					if(Minima.balance[balloop].tokenid == tokenid){
						var bal     = Minima.balance[balloop].confirmed;
						var balsend = Minima.balance[balloop].sendable;
						var balun   = Minima.balance[balloop].unconfirmed;
						var mempool = Minima.balance[balloop].mempool;
						
						//Is there unconfirmed money coming..
						if(balun !== "0" || mempool !== "0" || balsend !== bal){
							return balsend+" ("+bal+") / "+balun+" / "+mempool;	
						}else{
							return ""+bal;
						}	
					}
				}
				
				//Not found..
				return "0";
			},
		
			checkAllResponses : function(responses){
				len = responses.length;
				for(i=0;i<len;i++){
					if(responses[i].status != true){
						alert(responses[i].message+"\n\nERROR @ "+responses[i].minifunc);
						Minima.log("ERROR in Multi-Command ["+i+"] "+JSON.stringify(responses[i],null,2));
						return false;
					}
				}
				
				return true;
			},
			
			getStateVariable : function(statelist, port){
				var pslen = statelist.length;
				for(psloop=0;psloop<pslen;psloop++){
					if(statelist[psloop].port == port){
						return statelist[psloop].data;
					}
				}
				
				//Not found
				return null;
			}
				
	}
	
};

/**
 * POST the RPC call - can be cmd/sql/file/net
 */
function MinimaRPC(type, data, callback){
	//And now fire off a call saving it 
	httpPostAsync(Minima.rpchost+"/"+type+"/", encodeURIComponent(data), callback);
}

/**
 * Post a message to the Minima Event Listeners
 */
function MinimaPostMessage(event, info){
   //Create Data Object
   var data = { "event": event, "info" : info };

   //And dispatch
   if(MINIMA_MAIN_CALLBACK){
		MINIMA_MAIN_CALLBACK(data);	
   }      
}

/**
 * Start listening for PUSH messages
 */
function MinimaWebSocketListener(){
	Minima.log("Starting WebSocket Listener @ "+Minima.wshost);
	
	//Check connected
	if(MINIMA_WEBSOCKET !== null){
		MINIMA_WEBSOCKET.close();
	}
	
	//Open up a websocket to the main MINIMA proxy..
	MINIMA_WEBSOCKET = new WebSocket(Minima.wshost);
	
	MINIMA_WEBSOCKET.onopen = function() {
		//Connected
		Minima.log("Minima WS Listener Connection opened..");	
		
		//Now set the MiniDAPPID
		uid = { "type":"uid", "uid": window.location.href };
		
		//Send your name.. set automagically but can be hard set when debugging
		MINIMA_WEBSOCKET.send(JSON.stringify(uid));
			
	    //Send a message
	    MinimaPostMessage("connected", "success");
	};
	
	MINIMA_WEBSOCKET.onmessage = function (evt) { 
		//Convert to JSON	
		var jmsg = JSON.parse(evt.data);
		
		if(jmsg.event == "newblock"){
			//Set the new status
			Minima.block   = parseInt(jmsg.txpow.header.block,10);
			Minima.txpow   = jmsg.txpow;
			
			//What is the info message
			var info = { "txpow" : jmsg.txpow };
			
			//Post it
			MinimaPostMessage("newblock", info);
			
		}else if(jmsg.event == "newtransaction"){
			//What is the info message
			var info = { "txpow" : jmsg.txpow, "relevant" : jmsg.relevant };
			
			//New Transaction
			MinimaPostMessage("newtransaction", info);
		
		}else if(jmsg.event == "newtxpow"){
			//What is the info message
			var info = { "txpow" : jmsg.txpow };
			
			//New TxPoW
			MinimaPostMessage("newtxpow", info);
			
		}else if(jmsg.event == "newbalance"){
			//Set the New Balance
			Minima.balance = jmsg.balance;
			
			//What is the info message
			var info = { "balance" : jmsg.balance };
			
			//Post it..
			MinimaPostMessage("newbalance", info);
		
		}else if(jmsg.event == "network"){
			//What type of message is it..
			if( jmsg.details.action == "server_start" || 
				jmsg.details.action == "server_stop"  || 
				jmsg.details.action == "server_error"){
					
				sendCallback(MINIMA_SERVER_LISTEN, jmsg.details.port, jmsg.details);
				
			}else if( jmsg.details.action == "client_new"  || 
					  jmsg.details.action == "client_shut" || 
					  jmsg.details.action == "message"){
						
				if(!jmsg.details.outbound){
					sendCallback(MINIMA_SERVER_LISTEN, jmsg.details.port, jmsg.details);
				}else{
					sendCallback(MINIMA_USER_LISTEN, jmsg.details.hostport, jmsg.details);
				}
			}else if( jmsg.details.action == "post"){ 
				//Call the MiniDAPP function..
				if(MINIMA_MINIDAPP_CALLBACK){
					MINIMA_MINIDAPP_CALLBACK(jmsg.details);	
				}else{
					Minima.minidapps.reply(jmsg.details.replyid, "ERROR - no minidapp interface found");
				}
				
			}else{
				Minima.log("UNKNOWN NETWORK EVENT : "+evt.data);
			}
							
		}else if(jmsg.event == "txpowstart"){
			Minima.notify("Mining Transaction Started..","#55DD55");	
			
		}else if(jmsg.event == "txpowend"){
			Minima.notify("Mining Transaction Finished","#DD5555");
		}
	};
		
	MINIMA_WEBSOCKET.onclose = function() { 
		Minima.log("Minima WS Listener closed... reconnect attempt in 10 seconds");
	
		//Start her up in a minute..
		setTimeout(function(){ MinimaWebSocketListener(); }, 10000);
	};

	MINIMA_WEBSOCKET.onerror = function(error) {
		//var err = JSON.stringify(error);
		var err = JSON.stringify(error, ["message", "arguments", "type", "name", "data"])
		
		// websocket is closed.
	    Minima.log("Minima WS Listener Error ... "+err); 
	};
}

function sendCallback(list, port, msg){
	var funclen = list.length;
	for(i=0;i<funclen;i++){
		if(list[i].port == port){
			list[i].callback(msg);
			return;
		}
	}
}

/**
 * Notification Div
 */
var TOTAL_NOTIFICATIONS     = 0;
var TOTAL_NOTIFICATIONS_MAX = 0;
function MinimaCreateNotification(text, bgcolor){
	//First add the total overlay div
	var notifydiv = document.createElement('div');
	
	//Create a random ID for this DIV..
	var notifyid = Math.floor(Math.random()*1000000000);
	
	//Details..
	notifydiv.id  = notifyid;
	notifydiv.style.position 	 = "absolute";
	
	notifydiv.style.top 		 = 20 + TOTAL_NOTIFICATIONS_MAX * 110;
	TOTAL_NOTIFICATIONS++;
	TOTAL_NOTIFICATIONS_MAX++;
	
	notifydiv.style.right 		 = "0";
	notifydiv.style.width 	     = "400";
	notifydiv.style.height 	     = "90";
	
	//Regular or specific color
	if(bgcolor){
		notifydiv.style.background   = bgcolor;
	}else{
		notifydiv.style.background   = "#bbbbbb";	
	}
	
	notifydiv.style.opacity 	 = "0";
	notifydiv.style.borderRadius = "10px";
	notifydiv.style.border = "thick solid #222222";
	
	//Add it to the Page
	document.body.appendChild(notifydiv);
	
	//Create an HTML window
	var notifytext = "<table border=0 width=400 height=90><tr>" +
	"<td style='width:400;height:90;font-size:16px;font-family:monospace;color:black;text-align:center;vertical-align:middle;'>"+text+"</td></tr></table>";

	//Now get that element
	var elem = document.getElementById(notifyid);
	
	//Set the Text..
	elem.innerHTML = notifytext;
	
	//Fade in..
	elem.style.transition = "all 1s";
	
	// reflow
	elem.getBoundingClientRect();

	// it transitions!
	elem.style.opacity = 0.8;
	elem.style.right   = 40;
	
	//And create a timer to shut it down..
	setTimeout(function() {
		TOTAL_NOTIFICATIONS--;
		if(TOTAL_NOTIFICATIONS<=0){
			TOTAL_NOTIFICATIONS=0;
			TOTAL_NOTIFICATIONS_MAX=0;
		}
		
		document.getElementById(notifyid).style.display = "none";  
	 }, 4000);
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
		
	var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200){
			//Do we log it..
        	if(Minima.logging){
        		Minima.log("RPC:"+theUrl+"\nPARAMS:"+params+"\nRESPONSE:"+xmlHttp.responseText);
        	}

        	//Send it to the callback function..
        	if(callback){
        		callback(JSON.parse(xmlHttp.responseText));
        	}
        }
    }
    xmlHttp.open("POST", theUrl, true); // true for asynchronous 
    xmlHttp.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');    
	xmlHttp.send(params);
}

/**
 * Utility function for GET request (UNUSED for now..)
 * 
 * @param theUrl
 * @param callback
 * @returns
 */
function httpGetAsync(theUrl, callback, logenabled)
{	
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200){
        	//Always a JSON ..
        	var rpcjson = JSON.parse(xmlHttp.responseText);
        	
        	//Do we log it..
        	if(Minima.logging && logenabled){
        		var logstring = JSON.stringify(rpcjson, null, 2).replace(/\\n/g,"\n");
        		Minima.log(theUrl+"\n"+logstring);
        	}
        	
        	//Send it to the callback function..
        	if(callback){
        		callback(rpcjson);
        	}
        }
    }
    xmlHttp.open("GET", theUrl, true); // true for asynchronous 
    xmlHttp.send(null);
}
