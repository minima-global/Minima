/**
* RANTR Utility Functions
* 
* @spartacusrex
*/

/**
 * Draw the main Table view
 */
var MESSAGE_MAXTIME = 0;
var MESSAGE_NUMBER  = 0;
var VIEW_NUMBER  	= 25;
function createMainTable(maxtime,callback){
	var table = document.getElementById("mainranttable");
	table.innerHTML = "";
	selectRecentMessages(maxtime,VIEW_NUMBER,function(sqlmsg){
		drawCompleteMainTable(table,sqlmsg.rows,callback);
	});
}

function drawCompleteMainTable(thetable,allrows,callback){
	
	//Get all the super chatters
	selectAllSuperChatters(function(superchatters){
		var len = allrows.length;
		for(var i=0;i<len;i++){
			var tablerow 	= thetable.insertRow(i);
			var cell1 	 	= tablerow.insertCell(0);
			cell1.innerHTML = createMessageTable(allrows[i], superchatters, true);	
		}
		
		if(callback){
			callback();
		}
	});
}

function createMessageTable(messagerow, allsuperchatters, showactions){

	//Sanitize and clean the input - allow our custom youtube tag
	var dbmsg 		= decodeStringFromDB(messagerow.MESSAGE).replaceAll("\n","<br>");
	var msg 		= DOMPurify.sanitize(dbmsg,{ ADD_TAGS: ["youtube","spotify_track","spotify_artist","spotify_album","spotify_playlist"]});
	
	var parentid 	= DOMPurify.sanitize(messagerow.PARENTID+"");
	var baseid 		= DOMPurify.sanitize(messagerow.BASEID+"");
	var messageid	= DOMPurify.sanitize(messagerow.MESSAGEID+"");
	var publickey	= DOMPurify.sanitize(messagerow.PUBLICKEY+"");
	
	var dd 		= new Date(+messagerow.RECDATE);
	var datestr = dd.toDateString()+" "+dd.toLocaleTimeString()+"&nbsp;";
	
	//Are they a SUPER CHATTER
	var un = decodeStringFromDB(messagerow.USERNAME);
	var usernameorig = DOMPurify.sanitize(un);
	
	var username;
	if(checkInSuperChatters(publickey,allsuperchatters)){
		username = "[*] "+un;
	}else{
		username = un;
	}
	username = DOMPurify.sanitize(username+"");
	
	//Now start making the Table..
	var userline = "<table width=100%><tr><td class=namefont><a href='superchatter.html?uid="+MDS.minidappuid
					+"&username="+usernameorig
					+"&publickey="+publickey+"'>"+username+"</a></td><td style='text-align:right;'>"+datestr+"</td></tr></table>";
	
	var msgtable = "<table border=0 class=messagetable>"
					+"<tr><td class=messagetableusername>"+userline+"</td></tr>"
					+"<tr><td class=messagetablemessage><div class=messagetablemessagediv>"+msg+"</div></td></tr>";
	
	//Is this a reply..
	if(parentid != "0x00"){
		
		//Creatge a unique id..
		var uniqueid = parentid+Math.random();
		
		//Add a reply row..
		msgtable += "<tr><td class=messagetablereply id="+uniqueid+"></td></tr>";
		
		fillInReply(uniqueid,parentid);
	}
	
	//Finish up the table
	msgtable += "</table>";
	
	//Are we showing the actions..
	if(showactions){
	
		//The VIEW buton
		var viewbutton 	= "<button class=solobutton onclick=\"document.location.href='docview.html?uid="
						+MDS.minidappuid+"&baseid="+baseid+"&msgid="+messageid+"'\">VIEW ALL</button>";
		
		//The reply page
		var replybutton  = "<button class=solobutton onclick=\"document.location.href='reply.html?uid="
						+MDS.minidappuid+"&msgid="+messageid+"'\">REPLY</button>";
		
		//Rerant link
		var remsg = "RE-CHATTER";
		if(messagerow.RECHATTER !=0 ){
			remsg = "[X] RE-CHATTER";
		}
		var rerantbutton = "<button class=solobutton onclick='requestReChatter(\""+messageid+"\")'>"+remsg+"</button>";
		
		var delbutton = "";
		if(parentid == "0x00"){
			delbutton 	 = "<button class=solobutton onclick='requestDelete(\""+baseid+"\")'>DELETE ALL</button>";
		}	
				
		//Actions..
		msgtable += "<table class=messagetableactions><tr><td>"+viewbutton+" "+replybutton+" "+rerantbutton+"</td>"
					+"<td style='text-align:right;'>"+delbutton+"</td></tr></table>";	
	}
	
	//Add a break line
	msgtable+="<br>";
	
	//Store the latest time
	MESSAGE_MAXTIME = messagerow.RECDATE;
	MESSAGE_NUMBER++;
	
	//Convert SPECIAL tags
	msgtable = convertYouTube(msgtable);
	
	//Sptify
	msgtable = convertSpotify("track",msgtable);
	msgtable = convertSpotify("artist",msgtable);
	msgtable = convertSpotify("album",msgtable);
	msgtable = convertSpotify("playlist",msgtable);
	
	return msgtable;
}

function checkInSuperChatters(publickey,all){
	var len = all.length;
	for(var i=0;i<len;i++){
		var pubk = all[i].PUBLICKEY;
		if(pubk == publickey){
			return true;
		}
	}
	return false;
}

function fillInReply(htmlid,parentid){
	//Now run this async - is fine as does not interact withj anything else
	selectMessage(parentid,function(found,sqlrow){
		var tabletd = document.getElementById(htmlid);
		if(found){
			var reply = "In reply to.. "+decodeStringFromDB(sqlrow.USERNAME)+":"+decodeStringFromDB(sqlrow.MESSAGE);
			
			//Sanitize it..
			reply = DOMPurify.sanitize(reply);
		
			//Strip tags..
			reply = reply.replace(/(<([^>]+)>)/gi, "");

			if(reply.length > 180){
				reply = reply.substring(0,180)+"..";
			}
			tabletd.innerHTML=reply;	
		}else{
			tabletd.innerHTML="Reply not found..";	
		}
	});	
}

function requestReChatter(msgid){
	if(confirm("This will post this to all your Maxima Contacts ?")){
		updateRechatter(msgid,function(){
			insertReChatter(msgid,function(){
				
				//refresh the page
				window.location.reload();				
			});
		});
	}
}

function requestDelete(baseid){
	if(confirm("This will delete the whole thread ?")){
		deleteAllThread(baseid,function(){
			//Jump to home..
			document.location.href="index.html?uid="+MDS.minidappuid;
		});
	}
}

/**
 * Convert youtube tags
 */
function convertYouTube(msg){
	
	//Search and replace the youtube tags..
	var actual =  msg.replaceAll("<youtube>",
			"<div class='youtubecontainer'><iframe src='https://www.youtube.com/embed/");
	
	//And the end tags
	actual =  actual.replaceAll("</youtube>","' title='YouTube video player' frameborder='0'"
				+" allow='accelerometer; autoplay; clipboard-write; encrypted-media; "
				+"gyroscope; picture-in-picture; web-share' allowfullscreen class='youtubevideo'>"
				+"</iframe></div>");
	
	return actual;
}

/**
 * Works with track,artist,album,playlist
 */
function convertSpotify(type,msg){
	
	//Which tag
	var tag = "spotify_"+type;
	
	//Search and replace the youtube tags..
	var starttag 	= "<"+tag+">";
	var endtag 		= "</"+tag+">";
	
	var actual =  msg.replaceAll(starttag,
			"<iframe style='border-radius:12px' src='https://open.spotify.com/embed/"+type+"/");
	
	//And the end tags
	actual =  actual.replaceAll(endtag,"?utm_source=generator' width='100%' height='352' "
			+"frameBorder='0' allowfullscreen=''; allow='clipboard-write; encrypted-media; "
			+"fullscreen; picture-in-picture' loading='lazy'></iframe>");
	
	return actual;
}

function createReplyTable(baseid, callback){
	MDS.sql("SELECT * FROM MESSAGES WHERE baseid='"+baseid+"' ORDER BY recdate ASC", function(sqlmsg){
		//The complete Chat object
		var treechat = {};
		
		//The top post id the starter
		treechat.toprant = findRows(sqlmsg.rows,"0x00")[0];
		
		//And now recurse through the whole tree
		recurseReply(sqlmsg.rows,treechat.toprant);
		
		//AND.. finally return the Tree object
		callback(treechat);
	});
}

function recurseReply(allrows,current){
	//Get all the replies..
	current.replies = findRows(allrows,current.MESSAGEID);
	
	//And cycle through them..
	var len = current.replies.length;
	for(var i=0;i<len;i++){
		//recurse..
		recurseReply(allrows,current.replies[i]);
	} 
}

function findRows(allrows,parentid){
	var retarray = [];
	var len = allrows.length;
	for(var i=0;i<len;i++){
		if(allrows[i].PARENTID == parentid){
			retarray.push(allrows[i]);
		}
	}
	
	return retarray;
}

var MAX_IMAGE_SIZE = 400;
function scaleImageFile(file,callback){
	
	//What to do when file is loaded	
	let reader = new FileReader();
	reader.onload = function() {
		
		var image = new Image();
        image.onload = function (imageEvent) {

            // Resize the image
            var canvas 		= document.createElement('canvas'),
                max_size 	= MAX_IMAGE_SIZE,
                width 		= image.width,
                height 		= image.height;
			
			
			//New width and height
			if(width>MAX_IMAGE_SIZE || height>MAX_IMAGE_SIZE){
	            if (width > height) {
	                if (width > max_size) {
	                    height *= max_size / width;
	                    width = max_size;
	                }
	            } else {
	                if (height > max_size) {
	                    width *= max_size / height;
	                    height = max_size;
	                }
	            }
			}
			
			//Set the size and draw
            canvas.width  = width;
            canvas.height = height;
            canvas.getContext('2d').drawImage(image, 0, 0, width, height);
            
			//Send this RESIZED image
			callback(canvas.toDataURL("image/jpeg",0.9));
        }

		//Set the Image src
        image.src = reader.result;
	};
	
	reader.onerror = function() {
		console.log(reader.error);
	};

	//Read the file..
	reader.readAsDataURL(file);
}

function embedFile(){
	input = document.createElement('input');
	input.type = 'file';
	input.onchange = function(){
		files 	= Array.from(input.files);
		file 	= files[0]; 
		
		//Is it an image..
		MDS.log(file.name);
		var mmessage = document.getElementById("mainmessage");
		var filename = file.name.toLowerCase(); 
		if(filename.endsWith(".png")   || 
			filename.endsWith(".jpg")  ||
			filename.endsWith(".webp")  ||
			filename.endsWith(".jfif") ||
			filename.endsWith(".bmp")){
			
			scaleImageFile(file,function(imagedata){
				MDS.log("IMAGE:"+file.name+" SIZE:"+imagedata.length);
				mmessage.value 	= mmessage.value+"<div style='width:100%;text-align:center;'><img src='"+imagedata+"'></div>";
				//Move to the end
			    mmessage.focus();
			    mmessage.setSelectionRange(mmessage.value.length,mmessage.value.length);
			});
		
		}
		
		/*else if(filename.endsWith(".gif")){
			
			var reader = new FileReader();
		    reader.readAsDataURL(file);
		    reader.onload = function () {
		      console.log(reader.result);
		      var link = "<div style='width:100%;text-align:center;'><img src='"+imagedata+"'></div>";
		      mmessage.value = mmessage.value+" "+link;
		      
		      //Move to the end
		      mmessage.focus();
		      mmessage.setSelectionRange(mmessage.value.length,mmessage.value.length);
		    };
		    reader.onerror = function (error) {
		      console.log('Error: ', error);
		    };
		
		}else{
			
			//Check size..
			if(file.size >= MAX_MESSAGE_LENGTH){
				alert("File too big ("+file.size+")! MAX:"+MAX_MESSAGE_LENGTH+" ");
				return;
			}
			
			var reader = new FileReader();
		    reader.readAsDataURL(file);
		    reader.onload = function () {
		      console.log(reader.result);
		      var link = "<a download='"+filename+"' href='"+reader.result+"'>"+filename+"</a>";
		      mmessage.value = mmessage.value+" "+link;
		      
		      //Move to the end
		      mmessage.focus();
		      mmessage.setSelectionRange(mmessage.value.length,mmessage.value.length);
		    };
		    reader.onerror = function (error) {
		      console.log('Error: ', error);
		    };
		}*/
		
	};
	input.click();
}