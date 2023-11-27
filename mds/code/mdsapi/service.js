

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Do stuff.. from now..		
		MDS.api.call("mds api","servicehello",function(resp){
			MDS.log("API RETURNED: "+JSON.stringify(resp));
		});
	
	}else if(msg.event == "MDSAPI"){
		
		//get the request
		MDS.log("API REQUEST RECEIVED:"+JSON.stringify(msg));
		
		//Get the request
		var req = msg.data.message;
		
		//Do something..
		var result = "SOMETHING.."+req+req;
		
		//Reply..
		MDS.api.reply(msg.data.from,msg.data.id,result);
	}
});