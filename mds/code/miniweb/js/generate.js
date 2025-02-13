
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

function lineDecode(text){
	return decodeStringFromDB(text).replaceAll("\n","<br>");
}

function replaceBasics(temp, jsonsite){
	
	//First the Title.
	var newtemp = temp.replaceAll("#TEMPLATE_TITLE",decodeStringFromDB(jsonsite.name));
	
	//And the Contact info
	newtemp = newtemp.replaceAll("#CONTACT_INFO",decodeStringFromDB(jsonsite.contact));
	
	//Load backdrop..
	newtemp = newtemp.replaceAll("#BACKDROP",jsonsite.background);
	
	//And set the colors..
	newtemp = newtemp.replaceAll("#BORDERCOLOR",jsonsite.border_color);
	
	//Button
	newtemp = newtemp.replaceAll("#BUTTONTEXT",jsonsite.button_color);
	newtemp = newtemp.replaceAll("#BUTTONCOLOR",jsonsite.button_background);
	newtemp = newtemp.replaceAll("#ICONCOLOR",jsonsite.icon_color);
	
	//The Font
	newtemp = newtemp.replaceAll("#TITLEFONT",jsonsite.title_font);
	newtemp = newtemp.replaceAll("#TITLECOLOR",jsonsite.title_color);
	newtemp = newtemp.replaceAll("#TITLEBACKGROUND",jsonsite.title_background);
	newtemp = newtemp.replaceAll("#TITLEBORDER",jsonsite.title_border);
	
	newtemp = newtemp.replaceAll("#MAINFONT",jsonsite.main_font);
	newtemp = newtemp.replaceAll("#MAINCOLOR",jsonsite.main_color);
	newtemp = newtemp.replaceAll("#MAINBACKGROUND",jsonsite.main_background);
		
	return newtemp;
}

/**
 * Create the ALL Base pages
 */
function createBasePages(jsonsite, testsite, callback){
	//Load the Template..
	loadWebFile("./gensite/template.html",function(template){
		loadWebFile("./gensite/jslib.js",function(jslib){
	
			//INDEX
			INDEX_HTML 	= replaceBasics(template,jsonsite);
			INDEX_HTML = INDEX_HTML.replaceAll("#TEMPLATE_PAGE",lineDecode(jsonsite.description));
			
			//BLOG
			BLOG_HTML 	= replaceBasics(template,jsonsite);
			BLOG_HTML   = BLOG_HTML.replaceAll("#TEMPLATE_PAGE",createBlog(jsonsite));
			
			//EXTRAS
			EXTRA_HTML 		= replaceBasics(template,jsonsite);
			EXTRA_HTML  	= EXTRA_HTML.replaceAll("#TEMPLATE_PAGE",lineDecode(jsonsite.extrapage));
			
			//LINKS
			LINKS_HTML 	= replaceBasics(template,jsonsite);
			LINKS_HTML  = LINKS_HTML.replaceAll("#TEMPLATE_PAGE",createLinksPage(jsonsite,testsite));
			
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

/**
 * Create the Blog Page..
 */
function createBlog(jsonsite){
	
	var html = "<br>";
	var pos  = jsonsite.blog.length-1;
	
	for(var i=pos;i>=0;i--){
		
		var blog = jsonsite.blog[i];
		
		var title 	 = decodeStringFromDB(blog.title);
		var blogtext = decodeStringFromDB(blog.text);
		var linetext = blogtext.replaceAll("\n","<br>");
		
		html += "<span class=large_black_text><b>"+title+"</b></span><br>"
				+"<span class=small_grey_text><i>"+makeDateString(blog.date)+"</i></span><br><br>"
				+linetext+"<br><br>";
	}
	
	return html;
}

/**
 * Create the Pages..
 */
function createLinksPage(jsonsite, testsite){
	
	var html = "<br>";
	var size = jsonsite.links.length;
	for(var i=0;i<size;i++){
		if(jsonsite.links[i].name != ""){
			
			var title = lineDecode(jsonsite.links[i].name);
			
			if(testsite){
				html += title+"<br><a href=# onclick='alert(\"Links will work once you push this site Live!\")'>"+jsonsite.links[i].address+"</a><br><br>"
			}else{
				html += title+"<br><a href=# onclick='miniweb_JumpToURL(\""+jsonsite.links[i].address
						+"\");return false;'>"+jsonsite.links[i].address+"</a><br><br>"	
			}
		}
	}
	
	return html;
}