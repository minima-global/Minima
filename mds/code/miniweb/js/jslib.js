
/**
 * The Base internal and Web folders for the file packets
 */
var BASE_INTERNAL 	= "/extractfolder/";
var BASE_WEB 		= "/miniweb/";
	
/**
 * Utiulity function to concatenate URLS fixing forward slashes..
 */
function addDomains(base,addition){
		
	var bb  = ""+base;
	var add = ""+addition;
	
	if(bb.endsWith("/")){
		bb = bb.substring(0,bb.length-1);
	}
	
	if(!add.startsWith("/")){
		add="/"+add;
	}
	
	return bb+add;
}

/**
 * Get the BASE Domain of a miniweb URL
 */
function getBaseDomain(fulllink){
	
	//Make a copy to manipulate
	var ff = ""+fulllink;
	
	//Remove starter
	if(ff.startsWith("miniweb://")){
		ff = ff.substring(10);
	}
	
	//Now find the base folder..
	if(ff.startsWith("/")){
		ff = ff.substring(1);
	}
	
	//Find the next /..
	var index = ff.indexOf("/");
	if(index == -1){
		return ff;
	}
		
	var basefolder = ff.substring(0,index);
		
	return basefolder;
}

/**
 * Just Extract the Domain data to the internaL folder
 */
function extractDomain(domainreq, callback){
	
	//Clean up input - just in case
	var basedomain = getBaseDomain(domainreq);
	
	//File locations
	var internalfolder = addDomains(BASE_INTERNAL,basedomain);
	
	//Does it exist..
	MDS.file.list(internalfolder,function(listweb){
		MDS.log(JSON.stringify(listweb));
		
		if(listweb.response.exists){
			
			//Folder already exists leave it..
			callback(true);
			return;
			
		}else{
			
			//Extract the data if we have it..
			getFilePacket(basedomain,function(fp){
								
				//Did we find it!
				if(!fp){
					callback(false);
					return;
				}
				
				try{
					//Extract it..
					extractZIP(fp.data.file, internalfolder, function(){
						callback(true);
					});		
				}catch(error){
					MDS.log("ERROR unzipping : "+basedomain);
					callback(true);
				}
			});		
		}
	});
}

function extractDomainToWeb(domainreq, callback){
	
	//Get the base domain for this page
	var basedomain = getBaseDomain(domainreq);
	
	//File locations
	var internalfolder = addDomains(BASE_INTERNAL,basedomain);
	var webfolder 	   = addDomains(BASE_WEB,basedomain);
	
	//Does the WEB folder already exist
	MDS.file.listweb(webfolder,function(listweb){
		if(listweb.response.exists){
			callback(true);
		}else{
			
			//Extract the data if we have it..
			extractDomain(basedomain, function(extracted){
				if(extracted){
					//And copy to WEB folder..
					MDS.file.copytoweb(internalfolder, webfolder, function(copyweb){
						callback(true);
					});
						
				}else{
					//We don't have the data'
					callback(false);
				}
			});		
		}
	});	
}

function extractFilePacketToWeb(filepacket, callback){
	
	//Get the base domain for this page
	var basedomain = filepacket.data.name;
	
	//File locations
	var internalfolder = addDomains(BASE_INTERNAL,basedomain);
	var webfolder 	   = addDomains(BASE_WEB,basedomain);
	
	MDS.log("Internal   : "+internalfolder);
	MDS.log("Web folder : "+webfolder);
	
	//Delete all the old sites on refresh..
	MDS.file.deletefromweb(webfolder,function(delweb){
		MDS.file.delete(internalfolder,function(del){
		
			//Extract it to internal..
			extractZIP(filepacket.data.file, internalfolder, function(){
				
				//And copy to Web
				MDS.file.copytoweb(internalfolder, webfolder, function(copyweb){
					MDS.log("COPY TO WEB "+JSON.stringify(copyweb));
					callback();
				});
			});			
		});
	});
}

function convertDomainToURL(domainreq){
	
	domain = domainreq.trim();
	if(domain == ""){
		return;
	}
	
	if(domain.startsWith("miniweb://")){
		domain = domain.substring(10);
	}
	
	var finalpage = addDomains(BASE_WEB,domain);
	
	return finalpage;
}

function convertDomainToInternal(domainreq){
	
	domain = domainreq.trim();
	if(domain == ""){
		return;
	}
	
	if(domain.startsWith("miniweb://")){
		domain = domain.substring(10);
	}
	
	var finalpage = addDomains(BASE_INTERNAL,domain);
	
	return finalpage;
}
