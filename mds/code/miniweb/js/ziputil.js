
/**
 * Extract zip HEX data to an MDS folder 
 */
function extractZIP(zipdata, mdsfolder, callback){
		
	MDS.file.delete(mdsfolder,function(respdel){
		
		//Convert to base64..
		var b64 = MDS.util.hexToBase64(zipdata);
		
		//Now save to a folder..
		var zip = new JSZip();
		zip.loadAsync(b64, {base64: true}).then(function(zip){
			
			const numberOfCallbacks = Object.keys(zip.files).length - 1;
			var counter=0;
			//MDS.log("START.. "+numberOfCallbacks);
			zip.forEach(function(relpath, file){
				zip.file(relpath).async("base64").then(function(data){
					
					var conv 		= MDS.util.base64ToHex(data);
					var fullfile 	= mdsfolder+"/"+relpath;
					
					//console.log(counter+" FILE : "+fullfile);
					
					MDS.file.savebinary(fullfile,conv,function(saveresp){
						console.log(counter+" FILE : "+fullfile);
						counter++;
						if(counter>numberOfCallbacks){
							//MDS.log("END.. ");
							callback();
						}	
					});
				});
			});
		});	
	});
}