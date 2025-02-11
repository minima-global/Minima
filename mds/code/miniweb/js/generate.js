
//All the random backdrops in miniweb
var BASE_ADDRESS = "../../miniweb/images/surface/";
var RANDOM_BACKDROPS = ["Beach.jpg","Black.jpg","BlackHills.jpg","Blue Sky.jpg","Blue.jpg","Burgundy.jpg","Cobalt.jpg","Explode.jpg","Ice Blue.jpg",
"Matte Black.jpg","Mountains.jpg","Night Sky.jpg","Platinum.jpg","Rainbow.jpg","Red.jpg","RedBlack.jpg","Sandstone.jpg","Surf.jpg","Wave.jpg","Yellow.jpg"];

function makeDateString(timemilli){
	return new Date(+timemilli).toLocaleTimeString()+" "+new Date(+timemilli).toLocaleDateString();
}

function makeDateStringNow(){
	var dd = new Date();
	return dd.toLocaleTimeString()+"_"+dd.toLocaleDateString();
}

function loadWebFile(filename, callback){
	var client = new XMLHttpRequest();
	client.open('GET', filename);
	client.onreadystatechange = function() {
		if (client.readyState == XMLHttpRequest.DONE){
			//MDS.log("onreadystatechange "+client.readyState+" "+client.status)
			callback(client.responseText);	
		}
	}
	client.send();
}

function downloadGenSite(b64data, filename) {

	//Consrtruct the data URI
	var datauri = "data:application/zip;base64,"+b64data;  
		
	var link 		= document.createElement("a");
	link.download 	= filename;
	link.href 		= datauri;
	
	document.body.appendChild(link);
	link.click();
	document.body.removeChild(link);
	delete link;
}

function replaceBasics(temp, jsonsite){
	
	//First the Title.
	var newtemp = temp.replaceAll("#TEMPLATE_TITLE",decodeStringFromDB(jsonsite.name));
	
	//And the COntact info
	newtemp = newtemp.replaceAll("#CONTACT_INFO",decodeStringFromDB(jsonsite.contact));
	
	return newtemp;
}
	
/**
 * Generate a ZIP site from the json site details..!
 */
function generateSiteAsZip(jsonsite){
	
	//Load the Template..
	loadWebFile("./gensite/template.html",function(template){
		loadWebFile("./gensite/style.css",function(style){
			
			//Load a random backdrop..
			var rand 		= Math.floor(Math.random() * RANDOM_BACKDROPS.length);
			var randback 	= BASE_ADDRESS+RANDOM_BACKDROPS[rand];
			style = style.replaceAll("#BACKDROP",randback);
			
			loadWebFile("./gensite/jslib.js",function(jslib){
		
				//Now.. lets make the main pages
				var index 	= replaceBasics(template,jsonsite);
				var blog 	= replaceBasics(template,jsonsite);
				var extra 	= replaceBasics(template,jsonsite);
				var links 	= replaceBasics(template,jsonsite);
				
				//FIX Index..
				index = index.replaceAll("#TEMPLATE_PAGE",decodeStringFromDB(jsonsite.description));
				
				//FIX LINKS..
				links = links.replaceAll("#TEMPLATE_PAGE",createLinks(jsonsite));
				
				//Now lets put it all in a ZIP file..
				var zip = new JSZip();
				zip.file("index.html", index);
				zip.file("blog.html", blog);
				zip.file("extra.html", extra);
				zip.file("links.html", links);
				zip.file("style.css", style);
				zip.file("jslib.js", jslib);
				
				zip.generateAsync({type:"base64"}).then(function(content) {
					downloadGenSite(content,"gensite_"+makeDateStringNow()+".zip");
				});
			});
		});
	});
}

function createLinks(jsonsite){
	
	var html = ""
	for(var i=0;i<5;i++){
		if(jsonsite.links[i].name != ""){
			html += "<a href=# onclick='miniweb_JumpToURL(\""+jsonsite.links[i].address
						+"\");return false;'>"+jsonsite.links[i].name+"</a> <b>"+jsonsite.links[i].address+"</b><br><br>"
		}
	}
	
	return html;
}

