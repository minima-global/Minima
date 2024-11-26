
/**
 * Extract zip HEX data to an MDS folder 
 */
function extractZIP(zipdata, mdsfolder, callback){
	
	//Delet the old folder if exists	
	MDS.file.delete(mdsfolder,function(){
		
		//Convert to base64..
		var b64 = MDS.util.hexToBase64(zipdata);
		
		//Now save to a folder..
		var zip = new JSZip();
		zip.loadAsync(b64, {base64: true}).then(function(zip){
			
			const numberOfCallbacks = Object.keys(zip.files).length - 1;
			var counter=0;
			
			//Cycle through each file
			zip.forEach(function(relpath, file){
				zip.file(relpath).async("base64").then(function(data){
					
					//Convert the base 64 data to HEX
					var conv 		= MDS.util.base64ToHex(data);
					var fullfile 	= mdsfolder+"/"+relpath;
					
					//Save the HEX data
					MDS.file.savebinary(fullfile,conv,function(saveresp){
						
						//If we reach the last file - call the callback
						counter++;
						if(counter>numberOfCallbacks){
							callback();
						}	
					});
				});
			});
		});	
	});
}