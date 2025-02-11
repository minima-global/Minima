
function jumpPage(pagename){
	location.href=pagename+"?uid="+MDS.minidappuid;
}

function jumpHome(){
	jumpPage("index.html");
}

function jumpLinks(){
	jumpPage("links.html");
}

function jumpBlog(){
	jumpPage("blog.html");
}

function jumpExtra(){
	jumpPage("extra.html");
}

function startupButtons(){
	
	var showlabel = true;
	if ($(window).width() < 500) {
	   showlabel=false;
	}
	
	$( "#homebutton" ).button({
		icon: "ui-icon-home",
		showLabel: showlabel
	}).click(function(){jumpHome();});
	
    $( "#blogbutton" ).button({
		icon: "ui-icon-rss",
		showLabel: showlabel
	}).click(function(){jumpBlog();});
	
	$( "#extrabutton" ).button({
		icon: "ui-icon-shuttle",
		showLabel: showlabel
	}).click(function(){jumpExtra();});
    
    $( "#linksbutton" ).button({
		icon: "ui-icon-book-b",
		showLabel: showlabel
	}).click(function(){jumpLinks();});
}

function jumpToMiniWeb(mxaddress){
	
}