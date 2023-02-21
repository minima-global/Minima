/**
* CHATTER backend service
* 
* @spartacusrex
*/

//Load a file..
MDS.load("chatter.js");

//Convert HEX to UTF8
function hexToUtf8(s){
  return decodeURIComponent(
     s.replace(/\s+/g, '') // remove spaces
      .replace(/[0-9A-F]{2}/g, '%$&') // add '%' before each 2 characters
  );
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Create the DB if not exists
		createDB(function(msg){
			MDS.log("SQL DB inited");
		});
	
	//Only interested in Maxima
	}else if(msg.event == "MAXIMA"){
		
		//Is it for maxsolo..
		if(msg.data.application == "chatter"){
			
			//Maxima User 
			var pubkey 	= msg.data.from;
			
			//remove the leading 0x
			var datastr	= msg.data.data.substring(2);
				
			//Convert the data..
			var jsonstr = hexToUtf8(datastr);
			
			//And create the actual JSON
			var rantjson = JSON.parse(jsonstr);
			MDS.log(JSON.stringify(rantjson,null,2));
			
			//Check this rant
			checkRant(rantjson,function(valid){
				//Only add valif rants
				if(valid){
					
					//Do we already have it..
					checkInDB(rantjson,function(indb){
						if(!indb){
							
							//Add it to the DB
							addRantToDB(rantjson,function(){
								//And reload the main table
								MDS.comms.solo("NEWRANT");	
							});	
							
						}else{
							MDS.log("RANT Already in DB");
						}
					});
				}
			});				
		}
	}
});
