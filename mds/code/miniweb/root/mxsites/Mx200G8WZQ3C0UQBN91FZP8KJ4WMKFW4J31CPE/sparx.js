
var BASE_URL = "";

function goPage(page){
	location.href=BASE_URL+page+"?uid="+MDS.minidappuid;
}

function goHome(){
	goPage("index.html");
}

function goBlog(){
	goPage("blog.html");
}

function goLinks(){
	goPage("links.html");
}

/**
 * Write the header
 */
function writeHeader(baseurl){
	
	BASE_URL = baseurl;
	
	var gap = "&nbsp;&nbsp;&nbsp;";
	
	document.write(""
		+"<center><br><h1>Spartacus Rex</h1>"
		+"<button class='solobutton' onclick='goHome();'>Home</button>"+gap
		+"<button class='solobutton' onclick='goBlog();'>Blog</button>"+gap
		+"<button class='solobutton' onclick='goLinks();'>Links</button>"+gap
		+"<br><br>"
		+"");
} 

/**
 * Write the footer
 */
function writeFooter(){
	
	//Finish the top center block
	document.write("</center>");
	
	//MUST initialise the MiniWEB system.. IF you want to use miniweb_getDataURI functions..
 	miniweb_Init();

	//Main MDS message handler..
	MDS.init(function(msg){
		if(msg.event == "inited"){
			MDS.log("MDS Inited..");
		}
	});
} 
