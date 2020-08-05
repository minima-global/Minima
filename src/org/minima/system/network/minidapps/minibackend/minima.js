/**
* Minima JS lib for MiniDAPPs..
* 
* Includes the Decimal.js lib for precise Maths.
* 
* @spartacusrex
*/

/**
 * The Web Socket Host for PUSH messages
 */
//var MINIMA_WEBSOCKET = null;

/**
 * Intra MiniDAPP communication
 */
//var MINIDAPP_FUNCSTORE_LIST = [];

/**
 * Main MINIMA Object for all interaction
 */
var Minima = {
	//Current Minima Block
	block : 0,
	
	//TxPoWID of the current top block
	txpowid : "0x00",
	
	//RPC Host for Minima
	rpchost : "http://127.0.0.1:9002",
	
	//Web Socket Host for Minima
	wshost : "ws://127.0.0.1:9003",
	
	status : {},

	balance : {},
	
	//Show RPC commands
	logging : false,
	
	init : function(){
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
		    
		    //Start Listening for messages..
			MinimaWebSocketListener();
		});
	},
	
	log : function(output){
		java.lang.System.out.println("Minima @ "+new Date().toLocaleString()+" : "+output);
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
			},
			
			notify : function(message,bgcolor){
				//Log it..
				Minima.log("Notify : "+message);
			},
			
			send : function(minidappid, message, callback){
				//Create a random number to track this function call..
				var funcid = ""+Math.floor(Math.random()*1000000000);
				
				//Construct a JSON object
				msg = { "type":"message", "to":minidappid, "funcid":funcid, "message":message };

				//Add this Funcid and this callback to the list.. when you receive a reply 
				//you can respond to the correct callback
				funcstore = { "functionid":funcid, "callback":callback };
				MINIDAPP_FUNCSTORE_LIST.push(funcstore);
				
				//And send it..
				//MINIMA_WEBSOCKET.send(JSON.stringify(msg));
			},
			
			reply : function(evt, message){
				//Get the reply id
				var replyid = evt.detail.info.replyid;
				var replyto = evt.detail.info.from;
				
				//Construct a JSON object
				msg = { "type":"reply", "to":replyto, "replyid":replyid, "message":message };

				//And send it..
				//MINIMA_WEBSOCKET.send(JSON.stringify(msg));
			},
			
			setUID : function(uid){
				//UID JSON Message
				uid = { "type":"uid", "location": window.location.href, "uid":uid };
				
				//Send your name.. normally set automagically but can be hard set when debugging
				//MINIMA_WEBSOCKET.send(JSON.stringify(uid));
			}
				
	}
	
};


function MinimaRPC(type, data, callback){
	Minima.log("Backend RPC : "+type+" "+data);
	
    //Call the Java Function to deal with this..
	MinimaJSBridge.post(type, data, callback);
}

/**
 * Post a message to the Minima Event Listeners
 */
function MinimaPostMessage(event, info){
   //Create Data Object
   var data = { "event": event, "info" : info };

   //And dispatch - to the backend function..
   MinimaEvent({detail:data});
}

function MinimaBackEndListener(jmsg){
			
	if(jmsg.event == "connected"){
		//Post it
		MinimaPostMessage("connected","success");
		
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
		//Forward it..
		MinimaPostMessage("network",jmsg.details);
		
	}else{
		//Unknown Message Type	
		var jsonstr = JSON.stringify(jmsg,null,2); 
		Minima.log("Unknown Message Type : "+jsonstr);
	}
}
