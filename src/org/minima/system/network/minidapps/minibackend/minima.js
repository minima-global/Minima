/**
* Minima JS BackEND lib for MiniDAPPs..
* 
* @spartacusrex
*/

/**
 * Main MINIMA Object for all interaction
 */
var Minima = {
	//Current Minima Block
	block : 0,
	
	//TxPoWID of the current top block
	txpowid : "0x00",
	
	status : {},

	balance : {},
	
	//Show RPC commands
	logging : false,
	
	log : function(output){
		java.lang.System.out.println("Minima @ "+new Date().toLocaleString()+" : "+output);
	},
	
	/**
	 * Notify the user with a Pop up message
	 */
	notify : function(message,bgcolor){
		//Log it.. no notification for now..
		Minima.log("Notify : "+message);
	},
	
	cmd : function(minifunc, callback){
		MinimaRPC("cmd",minifunc,callback);
	},
	
	sql : function(query, callback){
		MinimaRPC("sql",query,callback);
	},
	
	net : {
		
		listen : function(port){
			MinimaRPC("net","listen "+port,null);
		},
		
		stop : function(port){
			MinimaRPC("net","stop "+port,null);
		},
		
		broadcast : function(port,jsonobject){
			MinimaRPC("net","broadcast "+port+" "+JSON.stringify(jsonobject),null);
		},
		
		connect : function(hostport){
			MinimaRPC("net","connect "+hostport,null);
		},
		
		disconnect : function(UID){
			MinimaRPC("net","disconnect "+UID,null);
		},
		
		send : function(UID, jsonobject){
			MinimaRPC("net","send "+UID+" "+JSON.stringify(jsonobject),null);
		},
		
		info : function(callback){
			MinimaRPC("net","info",callback);
		},
		
		get : function(url, callback){
			MinimaRPC("net","get "+url,callback);
		}
		
	},
	
	/**
	 * Intra MiniDAPP communication
	 */
	comms : {
		
		//List the currently installed minidapps
		list : function(callback){
			Minima.cmd("minidapp list",callback);
		},
		
		//Send a message to a specific minidapp
		send : function(minidappid,message, callback){
			Minima.cmd("minidapp post:"+minidappid+" \""+message+"\"",callback);
		},
		
		//The replyid is in the original message
		reply : function(replyid,message){
			//Reply to a POST message.. iuse the mesage
			replymsg = { "type":"reply", "message": message, "replyid" : replyid };
			
			//Special one off function..
			MinimaJSBridge.wspostreply(replyid, message);
		}
		
	},
	
	file : {
		
		save : function(jsonobject, file,  callback) {
			MinimaRPC("file","save "+file+" "+JSON.stringify(jsonobject),callback);
		},
		
		load : function(file, callback) {
			MinimaRPC("file","load "+file,callback);
		},
		
		list : function(file, callback) {
			MinimaRPC("file","list "+file,callback);
		},
		
		delete : function(file, callback) {
			MinimaRPC("file","delete "+file,callback);
		}
			
	},
	
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
 * Post a message to the internal JAVA runtime to process
 */
function MinimaRPC(type, data, callback){
	//Call the Java Function to deal with this..
	MinimaJSBridge.post(type, data, callback);
}

/**
 * Post a message to the Minima Event Listener in backend.js
 */
function MinimaPostMessage(event, info){
   //Create Data Object
   var data = { "event": event, "info" : info };

   //And dispatch - to the backend function..
   MinimaEvent({detail:data});
}

/**
 * Called by the Java when a message is sent rather than using a WebSocket
 */
function MinimaBackEndListener(jmsg){
			
	if(jmsg.event == "connected"){
		//Log a little..
		Minima.log("Initialisation..");
		
		//Do the first call..
		Minima.cmd("status;balance", function(json){
			//Status is first..
			Minima.status  = json[0].response;
			Minima.balance = json[1].response.balance;
			
		    //Store this..
		    Minima.txpowid = Minima.status.tip;
		    Minima.block   = parseInt(Minima.status.lastblock,10);

			//Post it
			MinimaPostMessage("connected","success");
		});
		
	}else if(jmsg.event == "newblock"){
		//Set the new status
		Minima.status  = jmsg.status;
		Minima.txpowid = jmsg.status.tip;
		Minima.block   = parseInt(jmsg.status.lastblock,10);
		
		//Post it
		MinimaPostMessage("newblock",jmsg.txpow);
		
	}else if(jmsg.event == "newtransaction"){
		//New Transaction
		MinimaPostMessage("newtransaction",jmsg.txpow);
		
	}else if(jmsg.event == "newbalance"){
		//Set the New Balance
		Minima.balance = jmsg.balance;
		
		//Post it..
		MinimaPostMessage("newbalance",jmsg.balance);
	
	}else if(jmsg.event == "network"){
		MinimaPostMessage("network",jmsg.details);
	
	}
	
}
