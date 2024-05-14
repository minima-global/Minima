function loadScript(url) {
  var head 		= document.getElementsByTagName("head")[0];
  var script 	= document.createElement("script");
  script.type 	= "text/javascript";
  script.src 	= url;
  head.appendChild(script);
}

var selectedchat = -1;

function createChatList(){
	//Chat list loaded
	var thetable = document.getElementById("table_chatlist");
	thetable.innerHTML="";
	
	for(var i=0;i<20;i++){
		var tablerow 	= thetable.insertRow(i);
		var cell1 	 	= tablerow.insertCell(0);
		
		cell1.innerHTML = writeChatListWindow(i);	
	}
}

function showAlert(chatid){
	selectedchat = chatid;
	createChatWindow("Some text : "+chatid);
	createChatList();
}

function writeChatListWindow(chatid){
	
	
	if(chatid == selectedchat){
		return "<table onclick='showAlert("+chatid+");' width=100% height=100% cellspacing=0 cellpadding=0 class=view_chatlistitemselected>"
			+"<tr>"
			+"	<td width=100%>Paddy "+chatid+"</td>"
			+"	<td>Time&nbsp;&nbsp;&nbsp;</td>"
			
			+"<tr>"
			+"<tr>"
			+"	<td colspan=2><span class=span_ellipsis>"
					+"Lots and lots of text stuff lkj as dasd as asd asd saslkd jdlks jf</span></td>"
			+"<tr>"
			+"</table>";
	}else{
		return "<table onclick='showAlert("+chatid+");' width=100% height=100% cellspacing=0 cellpadding=0 class=view_chatlistitem>"
			+"<tr>"
			+"	<td width=100%>Paddy "+chatid+"</td>"
			+"	<td>Time&nbsp;&nbsp;&nbsp;</td>"
			
			+"<tr>"
			+"<tr>"
			+"	<td colspan=2><span class=span_ellipsis>"
					+"Lots and lots of text stuff lkj as dasd as asd asd saslkd jdlks jf</span></td>"
			+"<tr>"
			+"</table>";	
	}
}

function createChatWindow(text){
	//Chat list loaded
	var chatwindow = document.getElementById("id_chatwindow");
	
	chatwindow.innerHTML = text; 
}