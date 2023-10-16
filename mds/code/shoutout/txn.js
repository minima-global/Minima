
var SHOUTOUT_ADDRESS = "0x73686F75746F7574"

function sendTxnMessage(category, title, message, user, callback){
	
	var state = {};
	state[0] = "["+category+"]";
	state[1] = "["+title+"]";
	state[2] = "["+message+"]";
	state[3] = "["+user+"]";
	
	var func = "send amount:0.01 address:"+SHOUTOUT_ADDRESS+" state:"+JSON.stringify(state);
	
	//run it..
	MDS.cmd(func,function(sendresp){
		if(callback){
			callback(sendresp);
		}
	});
}