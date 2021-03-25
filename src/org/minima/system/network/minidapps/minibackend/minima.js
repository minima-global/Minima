/**
* Minima JS BackEND lib for MiniDAPPs..
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
	 * Current Balance of this User
	 */
	balance : {},
	
	/** 
	 * The Full TxPoW Top Block
	 */
	txpow : {},

	/**
	 * Show RPC commands
	 */
	logging : false,

	/**
	 * Minima Startup - with the callback function used for all Minima messages
	 */
	init : function(callback){
		//Store the callback
		if(callback){
			MINIMA_MAIN_CALLBACK = callback;	
		}else{
			Minima.log("No Main Minima Callback specified..");
		}
		
		//Do the first call..
		Minima.cmd("topblock;balance", function(json){
			if(json[0].status){
				//Store this..
			    Minima.block  = parseInt(json[0].response.txpow.header.block,10);
			    Minima.txpow  = json[0].response.txpow;
		    
				if(json[1].status){
					//Status is first..
					Minima.balance = json[1].response.balance;
				}	
			}
			
		    //Send a message
		    MinimaPostMessage("connected", "success");
		});
	},
	
	log : function(output){
		java.lang.System.out.println("Service @ "+new Date().toLocaleString()+" : "+output);
	},
	
	/**
	 * Notify the user with a Pop up message
	 */
	notify : function(message,bgcolor){
		//Log it.. no notification for now..
		Minima.log("Notify : "+message);
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
		
		//GET an URL resource
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
			//Reply to a POST message.. iuse the mesage
			replymsg = { "type":"reply", "message": message, "replyid" : replyid };
			
			//Special one off function..
			MinimaJSBridge.wspostreply(replyid, message);
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
 * Post a message to the internal JAVA runtime to process
 */
function MinimaRPC(type, data, callback){
	//Do we log it..
	if(Minima.logging){
        Minima.log("SERVICE_RPC : "+type+" "+data);	
	}
	
	//Call the Java Function to deal with this..
	MinimaJSBridge.post(type, data, callback);
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
 * Called by the Java when a message is sent rather than using a WebSocket
 */
function MinimaBackEndListener(jmsg){
			
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
			var info = { "transaction" : jmsg.transaction };
			MinimaPostMessage("miningstart", info);
				
	}else if(jmsg.event == "txpowend"){
		var info = { "transaction" : jmsg.transaction };
		MinimaPostMessage("miningstop", info);
	
	}	
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