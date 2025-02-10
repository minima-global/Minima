
/**
 * The Base internal and Web folders for the file packets
 */
var BASE_INTERNAL 	= "/extractfolder/";
var BASE_WEB 		= "/root/mxsites/";
	
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
 * Add ther restriced UID to the URL - So MDS works..
 */
function addURLParam(base,param,value){
	
	//Does it already contain this param..
	if(base.includes(param+"=")){
		return base;
	}
	
	//Does it already have some params..
	if(base.indexOf("?") == -1){
		
		return base+"?"+param+"="+value;
	}
	
	//There must already be some values..
	return base+"&"+param+"="+value;
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

/**
 * Does the current domain exist in WEB folder
 */
function webFolderExists(domain, callback){
	
	//Get the base web folder
	var basedomain  = getBaseDomain(domain);
	var webfolder 	= addDomains(BASE_WEB,basedomain);
		
	MDS.file.listweb(webfolder,function(listweb){
		callback(listweb.response.exists);
	});
}

/**
 * Load MiniFS Filepacket 
 */
function loadFilePacket(domain, callback){
	
	//Get the base domain
	var basedomain = getBaseDomain(domain);
	
	//Now lets load the file..
	var api 	= {};
	api.action 	= "LOAD";
	api.data 	= basedomain;
	
	//Send it to MiniWEB
	MDS.api.call("minifs",JSON.stringify(api),function(resp){
		//MDS.log("MiniFS response "+JSON.stringify(resp));
		
		//Call wqas replied to ?
		if(resp.status){
			
			//Parse the data
			var data = JSON.parse(resp.data);
			
			//Did we fiund it
			if(data.found){
				
				//Extract to WEB
				extractFilePacketToWeb(data.filepacket, function(resp){
					callback(true);
				});
				
			}else{
				//Did we get the data, did we find the MiniFS MIniDAPP, and did we request!
				callback(false,true,data.requested);
			}
			
		}else{
			//Could not find thew MiniFS MiniDAPP
			callback(false,false,false);
		}
	});
}

/**
 * Extract a ZIP filepacket to Internal and Web Folder
 */
function extractFilePacketToWeb(filepacket, callback){
	
	//Get the base domain for this page
	var basedomain = filepacket.data.name;
	
	//File locations
	var internalfolder = addDomains(BASE_INTERNAL,basedomain);
	var webfolder 	   = addDomains(BASE_WEB,basedomain);
	
	//Delete all the old sites on refresh..
	MDS.file.deletefromweb(webfolder,function(delweb){
		MDS.file.delete(internalfolder,function(del){
		
			//Extract it to internal..
			extractZIP(filepacket.data.file, internalfolder, function(){
				
				//And copy to Web
				MDS.file.copytoweb(internalfolder, webfolder, function(copyweb){
					callback();
				});
			});			
		});
	});
}

function loadDataURI(domainreq,callback){
	
	//Get the internal File..
	var internalfile = convertDomainToInternal(domainreq);
	
	//Now load it
	MDS.file.loadbinary(internalfile, function(loader){
		
		//Find it ?
		if(loader.status){
			
			//get the HEX data
			var hexdata = loader.response.load.data;
			
			//Convert to base 64..
			var b64 = MDS.util.hexToBase64(hexdata);
			
			//Consrtruct the data URI
			callback(true,"data:"+getMimeTypeFromExtension(internalfile)+";base64,"+b64);
			
			return;
		}

		callback(false);
	});
}

