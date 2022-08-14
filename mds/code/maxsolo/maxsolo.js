/**
* MAXCHAT JS lib for MiniDAPPs..
* 
* @spartacusrex
*/

var MY_NAME 				= "noname"; 

var CURRENT_ROOM_PUBLICKEY 	= "";
var CURRENT_ROOM_NAME 		= "";

var RENDER_STYLE 			= "sidebar";
var ROOMS_SHOWING 			= true;

var IMAGE_WIDTH				= 300;

function processMaximaEvent(msg){
	
	//Is it for us.. ?
	if(msg.data.application != "maxsolo"){
		return;
	} 
	
	//Who is this message from..
	var pubkey = msg.data.from;
	
	//Get the data packet..
	var datastr = msg.data.data;
	if(datastr.startsWith("0x")){
		datastr = datastr.substring(2);
	}
	
	//The JSON
	var jsonstr = hexToUtf8(datastr);
	
	//And create the actual JSON
	var maxjson = JSON.parse(jsonstr);
	
	//Are we in this room..
	if(CURRENT_ROOM_PUBLICKEY == pubkey){
		loadMessages(CURRENT_ROOM_PUBLICKEY);
	}else{
		if(ROOMS_SHOWING){
			loadRooms();
		}
	}
	
	//Show a notification
	if(maxjson.type=="text"){
		showNotification(maxjson.username, maxjson.message);	
	}else{
		showNotification(maxjson.username, "IMAGE");
	}
	
	//Log it..
	//MDS.log(jsonstr);
}

//Insert into the DB
function insertMessage(roomname, frompubkey, name, type, message, filedata, callback){
	
	//Url encode the message
	let encoded = encodeURIComponent(message).replace("'", "%27");
	
	var fullsql = "INSERT INTO messages (roomname, publickey,username,type,message,filedata,date) VALUES "
			+"('"+roomname+"','"+frompubkey+"','"+name+"','"+type+"','"+encoded+"','"+filedata+"', "+Date.now()+")";
	
	MDS.sql(fullsql, function(resp){
		//WE have the reply..
		callback();	
	});
}

function loadRooms(){
	
	//Load the last message in each room..
	MDS.sql("SELECT * from messages WHERE ID in "
		+"( SELECT max(ID) FROM messages GROUP BY publickey ) ORDER BY ID DESC", function(sqlmsg){
		
		//Get the data
		var sqlrows = sqlmsg.rows;
		
		//Create a table..
		var rowtable = "<table width=100%>"
		
		//Create the Room List..
		for(var i = 0; i < sqlrows.length; i++) {
		    var sqlrow = sqlrows[i];
		
			//Is it selected..
			var selected = "";
			if(CURRENT_ROOM_PUBLICKEY == sqlrow.PUBLICKEY){
				selected = "style='background-color: #cff;'";
			}
		
			let decoded = decodeURIComponent(sqlrow.MESSAGE).replace("%27", "'");;
		
			//Start
			rowtable += "<tr><td "+selected+" class='sidebarroom' onclick='sidebarclick(\""+sqlrow.PUBLICKEY+"\",\""+sqlrow.ROOMNAME+"\")'><font size='+3'><b>"+sqlrow.ROOMNAME+"</b></font><br><br>";
			
			//Is it unread
			if(sqlrow.TYPE == "text"){
				if(sqlrow.READ == 0){
					rowtable += "<b>"+decoded+"</b></td></tr>"	
				}else{
					rowtable += decoded+"</td></tr>"
				}	
			}else{
				rowtable += "IMAGE</td></tr>"	
			}
		}
		
		//Finish off the table
		rowtable += "</table>"
		
		//Set this as the room list
		if(RENDER_STYLE=="sidebar"){
			document.getElementById("sidebar").innerHTML = rowtable;
		}else{
			document.getElementById("allsidebar").innerHTML = rowtable;	
		}
			
		//MDS.log(JSON.stringify(sqlmsg));
	});
}

function sidebarclick(publickey, roomname){
	
	//Are we there already
	if(CURRENT_ROOM_PUBLICKEY == publickey){
		return;
	}
	
	if(RENDER_STYLE=="sidebar"){
		//Just render into the Main View
		loadMessages(publickey);
	}else{
		//Urlencode the name..
		encroomname = encodeURIComponent(roomname);
		
		//Jump to a new MainView Page
		window.location = "chatwindow.html?publickey="+publickey+"&uid="+MDS.minidappuid+"&roomname="+encroomname;	
	}
}

function loadMessages(publickey){
	
	//Store for later
	CURRENT_ROOM_PUBLICKEY = publickey;

	//Load the last message in each room..
	MDS.sql("SELECT * from messages WHERE publickey='"+publickey+"' ORDER BY ID DESC LIMIT 200", function(sqlmsg){
		
		//Get the data
		var sqlrows = sqlmsg.rows;
		
		//Reverse them ( I use a limit..)
		sqlrows.reverse();
		
		//Create a table..
		var chat = "";
		
		//Create the Room List..
		for(var i = 0; i < sqlrows.length; i++) {
		    var sqlrow = sqlrows[i];

			let decoded = decodeURIComponent(sqlrow.MESSAGE).replace("%27", "'");;
		
			if(sqlrow.TYPE == "text"){
				chat += "<b>"+sqlrow.USERNAME+"</b> : "+decoded+"<br><br>";
			}else if(sqlrow.TYPE == "image"){
				chat += "<b>"+sqlrow.USERNAME+"</b> : <br><img src='"+sqlrow.FILEDATA+"' max-width=300><br><br>";
			}else{
				chat += "<b>"+sqlrow.USERNAME+"</b> : UNKNOWN TYPE.. <br><br>";
			}
			
			//Store this for later
			CURRENT_ROOM_NAME = sqlrow.ROOMNAME;
		}
		
		//Set this as the room list
		document.getElementById("chatwindow").innerHTML = chat;
		
		//Scroll ot the bottom
		setTimeout(function(){ 
			var obj 		= document.getElementById("chatwindow");
			obj.scrollTop 	= obj.scrollHeight; 
		}, 10);
		
		//MDS.log(JSON.stringify(sqlmsg));
	});
	
	//Set all messages to read
	MDS.sql("UPDATE messages SET read=1 WHERE publickey='"+publickey+"'");
	
	if(RENDER_STYLE=="sidebar"){
		//Load the rooms .. again.. inefficient..
		loadRooms();
	}
}

function sendMessage(){
	
	//Are we empty
	if(CURRENT_ROOM_PUBLICKEY == ""){
		alert("No room chosen..");
		return;
	}
	
	//Get the message..
	var msg = document.getElementById('chatline').value.trim();
	document.getElementById('chatline').value = "";
	
	if(msg == ""){
		return;
	}
	
	var data = {};
	data.username 	= MY_NAME;
	data.type 		= "text";
	data.message  	= msg;
	data.filedata  	= "";
	
	//Disable send until finished
	document.getElementById("sendbutton").disabled = true;
	
	//Send this..
	sendData(data)
}

function sendImage(imagedata){
	
	//Are we empty
	if(CURRENT_ROOM_PUBLICKEY == ""){
		alert("No room chosen..");
		return;
	}
	
	var data = {};
	data.username 	= MY_NAME;
	data.type 		= "image";
	data.message 	= "";
	data.filedata  	= imagedata;
	//data.filedata  	= "data:image/png;base64, iVBORw0KGgoAAAANSUhEUgAAAAUAAAAFCAYAAACNbyblAAAAHElEQVQI12P4//8/w38GIAXDIBKE0DHxgljNBAAO9TXL0Y4OHwAAAABJRU5ErkJggg==";
	
	//Disable send until finished
	document.getElementById("sendbutton").disabled = true;
	
	//Send this..
	sendData(data)
}

function sendData(jsondata){
	
	//Convert to a string..
	var datastr = JSON.stringify(jsondata);
	
	//And now convert to HEX
	var hexstr = "0x"+utf8ToHex(datastr).toUpperCase().trim();
	
	//Create the function..
	fullfunc = "maxima action:send publickey:"+CURRENT_ROOM_PUBLICKEY+" application:maxsolo data:"+hexstr;
	
	//Send the message via Maxima!..
	MDS.cmd(fullfunc, function(resp){
		if(resp.status == false){
			alert(resp.error);
			MDS.log(JSON.stringify(resp));
		}else if(resp.response.delivered == false){
			alert(resp.response.error);
			MDS.log(JSON.stringify(resp));
		}else{
			//And add it to Our DB..
			insertMessage(CURRENT_ROOM_NAME, CURRENT_ROOM_PUBLICKEY, MY_NAME, 
					jsondata.type, jsondata.message, jsondata.filedata, function(){
				
				//Load all the messages
				loadMessages(CURRENT_ROOM_PUBLICKEY);	
				
			});			
		}
		
		//Enable send button again
		document.getElementById("sendbutton").disabled = false;
	});
}

function startChat(){
	
	//Get the username and the publickey..
	var selobj;
	
	if(RENDER_STYLE=="sidebar"){
		selobj = document.getElementById('maxcontacts');
	}else{
		selobj = document.getElementById('maxcontacts2');
	}	
	
	pkey 	= selobj.value;
	pname 	= selobj.options[selobj.selectedIndex].text;
	
	if(CURRENT_ROOM_PUBLICKEY != pkey){
		//Insert a message to start the room ( Could check if room already started!)
		insertMessage(pname, pkey, MY_NAME, "text", "Start Room..","", function(){
			if(RENDER_STYLE=="sidebar"){
		
				//And Set that room..
				loadMessages(pkey)
				
				//Load the rooms..
				loadRooms();
		
			}else{
				//Urlencode the name..
				encroomname = encodeURIComponent(pname);
			
				//Jump to a new MainView Page
				window.location = "chatwindow.html?publickey="+pkey+"&uid="+MDS.minidappuid+"&roomname="+encroomname;
			}	
		});	
	}
}

function sendMoney(){
	
	//Are we empty
	if(CURRENT_ROOM_PUBLICKEY == ""){
		alert("No room chosen..");
		return;
	}

	//Get the details..
	selobj = document.getElementById('tokens');
	
	tokenid 	= selobj.value;
	tokenname 	= selobj.options[selobj.selectedIndex].text;
	tokenamount = document.getElementById('amountofmoney').value;
	if(tokenamount=="" || tokenamount < 0){
		alert("Invalid amount..");
		return;
	}
	
	//Disable send until finished
	document.getElementById("sendbutton").disabled = true;
			
	//First load the full contact details..
	MDS.cmd("maxcontacts action:search publickey:"+CURRENT_ROOM_PUBLICKEY,function(resp){
		
		//did we find him..
		if(resp.status == false){
			alert("User not found ?");
			document.getElementById("sendbutton").disabled = false;
			return;
		}
	
		//Get the contact
		contact = resp.response.contact;
		address = contact.extradata.minimaaddress;
		
		sendfunction = "send tokenid:"+tokenid+" amount:"+tokenamount+" address:"+address
		
		//Log 
		MDS.log("SEND MONEY : "+sendfunction);
	
		MDS.cmd(sendfunction, function(resp){
			
			//Did it work..
			if(resp.status == false){
				
				//Is it pending..
				if(resp.pending){
					alert("This transaction is now pending!");
				}else{
					MDS.log(JSON.stringify(resp));
					alert("ERROR : "+resp.message);
					document.getElementById("sendbutton").disabled = false;
					return;	
				}	
			}
			
			var data = {};
			data.username 	= MY_NAME;
			data.type 		= "text";
			data.message  	= "!! I just sent you "+tokenamount+" "+tokenname+" !!";
			data.filedata  	= "";
		
			//Send this..
			sendData(data)	
		});
	
		showDiv("sendmoney",false);
	});
}