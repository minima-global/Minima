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
		MDS.net.GET("https://www.google.com",function(msg){
			MDS.log("NET GET! "+JSON.stringify(msg));
		});
	
	//Only interested in Maxima
	}else{
		
		MDS.log("service.js : "+msg.event);
		
	}
	
});
