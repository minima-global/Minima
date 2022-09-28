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
					
		//Run this..
		MDS.net.POST("http://127.0.0.1:9002", "status", function(msg){
			MDS.log("NET service POST! "+JSON.stringify(msg));
		});
	
	//Only interested in Maxima
	}else{
		
		//MDS.log("service.js : "+msg.event);
		
	}
	
});
