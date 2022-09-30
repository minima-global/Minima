/**
* MAXSOLO backend service
* 
* @spartacusrex
*/

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
			
		MDS.log("MyTest inited");
					
		MDS.comms.broadcast("Service Public Message!", function(msg){
			MDS.log("Service COMMS public : "+JSON.stringify(msg));
		});
		
		MDS.comms.solo("Service Private Message!", function(msg){
			MDS.log("Service COMMS private : "+JSON.stringify(msg));
		});
		
		//Run this..
		//MDS.net.POST("http://127.0.0.1:9002", "status", function(msg){
	//		MDS.log("NET service POST! "+JSON.stringify(msg));
	//	});
	
	//Only interested in Maxima
	}else if(msg.event == "MDSCOMMS"){
		MDS.log("service.js : "+JSON.stringify(msg));
	}
	
});
