
//GLOBAL version of the Gen Site Files..
var INDEX_HTML = "";
var BLOG_HTML = "";
var EXTRA_HTML = "";
var LINKS_HTML = "";
var JSLIB_JS = "";

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
 * Create the ALL Base pages
 */
function createBasePages(jsonsite, testsite, callback){
	//Load the Template..
	loadWebFile("./gensite/template.html",function(template){
		
		//Load backdrop..
		template 		= template.replaceAll("#BACKDROP",jsonsite.background);
		
		//And set the colors..
		template 		= template.replaceAll("#BORDERCOLOR",jsonsite.border_color);
		template 		= template.replaceAll("#ICONCOLOR",jsonsite.icon_color);
		
		loadWebFile("./gensite/jslib.js",function(jslib){
	
			//INDEX
			INDEX_HTML 	= replaceBasics(template,jsonsite);
			INDEX_HTML = INDEX_HTML.replaceAll("#TEMPLATE_PAGE",decodeStringFromDB(jsonsite.description));
			
			//BLOG
			BLOG_HTML 	= replaceBasics(template,jsonsite);
			
			//EXTRAS
			EXTRA_HTML 	= replaceBasics(template,jsonsite);
			
			//LINKS
			LINKS_HTML 	= replaceBasics(template,jsonsite);
			LINKS_HTML  = LINKS_HTML.replaceAll("#TEMPLATE_PAGE",createLinks(jsonsite,testsite));
			
			//JSLIB
			JSLIB_JS	= jslib;
			
			if(callback){
				callback();
			}
		});
	});	
}
	
/**
 * Generate a ZIP site from the json site details..!
 */
function generateSiteAsZip(jsonsite){
	
	createBasePages(jsonsite,false,function(){
		
		//Now lets put it all in a ZIP file..
		var zip = new JSZip();
		zip.file("index.html", INDEX_HTML);
		zip.file("blog.html", BLOG_HTML);
		zip.file("extra.html", EXTRA_HTML);
		zip.file("links.html", LINKS_HTML);
		zip.file("jslib.js", JSLIB_JS);
		
		zip.generateAsync({
				type:"base64",
				compression: "DEFLATE",
    			compressionOptions: {
        			level: 9
    			}}).then(function(content) {
			downloadGenSite(content,"minisite_"+makeDateStringNow()+".zip");
		});
	});			
}

function createLinks(jsonsite, testsite){
	
	var html = "<br>";
	for(var i=0;i<5;i++){
		if(jsonsite.links[i].name != ""){
			if(testsite){
				html += jsonsite.links[i].name+"<br><a href=# onclick='alert(\"Links will work once you push this site Live!\")'>"+jsonsite.links[i].address+"</a><br><br>"
			}else{
				html += jsonsite.links[i].name+"<br><a href=# onclick='miniweb_JumpToURL(\""+jsonsite.links[i].address
						+"\");return false;'>"+jsonsite.links[i].address+"</a><br><br>"	
			}
		}
	}
	
	return html;
}


/** 
 * The DEFAULT locations of the test gensite folder
 */
var DEFAULT_WEB_GENSITE = "/root/gensite/usersite";
var DEFAULT_MDS_GENSITE = "/gensite";

function wipeGenSite(callback){
	MDS.file.delete(DEFAULT_MDS_GENSITE, function(delresp){
		MDS.file.deletefromweb(DEFAULT_WEB_GENSITE, function(delwebresp){
			if(callback){
				callback();
			}
		});
	});
}

function writeToWeb(webfile, data, callback){
	//Now create thefile..
	var mdsfile = DEFAULT_MDS_GENSITE+"/"+webfile;
	MDS.file.save(mdsfile, data, function(indsave){
		MDS.file.copytoweb(mdsfile,webfile,function(copyresp){
			if(callback){
				callback();
			}
		});
	});
}
	
/**
 * Create the site for testing and viewing..
 */
function createGenSite(jsonsite, callback){
	
	createBasePages(jsonsite,true,function(){
		
		//Main index page
		var indexpage = DEFAULT_WEB_GENSITE+"/"+"index.html";
		
		//Now save these files in the gensite test folder..
		wipeGenSite(function(){
			writeToWeb(indexpage, INDEX_HTML, function(){
				writeToWeb(DEFAULT_WEB_GENSITE+"/"+"jslib.js", JSLIB_JS, function(){
					writeToWeb(DEFAULT_WEB_GENSITE+"/"+"blog.html", BLOG_HTML, function(){
						writeToWeb(DEFAULT_WEB_GENSITE+"/"+"links.html", LINKS_HTML, function(){
							writeToWeb(DEFAULT_WEB_GENSITE+"/"+"extra.html", EXTRA_HTML, function(){
								if(callback){
									callback(indexpage);	
								}			
							});	
						});
					});
				});	
			});
		});	
	});
}
