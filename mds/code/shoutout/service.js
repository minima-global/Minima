/**
* SHOUTOUT backend service
* 
* @spartacusrex
*/

//Load js libs
MDS.load("puresha1.js");
MDS.load("xregexp-all.js");
MDS.load("jslib.js");
MDS.load("sql.js");
MDS.load("txn.js");

//Main Shoutout Address
var SHOUTOUT_ADDRESS = "0x73686F75746F7574"

//Are we logging data
var logs = false;

function addStartupGroup(filters,category){
	var userpubkey  = "0x00";
	var title 		= "Start Here!";
	var cattitid 	= getCategoryTitleID(category,title);
	if(!checkMsgBlockedSQL(filters,userpubkey,cattitid,category)){
		insertMessage(category, title, "Shout Out", userpubkey, "Mx999", "Blah Blah Blah!", 199, 0, function(res){});	
	}
}

//Main message handler..
MDS.init(function(msg){
	
	//Do initialisation
	if(msg.event == "inited"){
		
		//Init the DB		
		createDB(function(){
	
			//Notify of new messages..
			MDS.cmd("coinnotify action:add address:"+SHOUTOUT_ADDRESS,function(startup){});
	
			loadAllFilters(function(filters){
				
				//Insert some start up Groups
				addStartupGroup(filters,"minima");
				addStartupGroup(filters,"minima.developer");
				addStartupGroup(filters,"adult");
				addStartupGroup(filters,"art");
				addStartupGroup(filters,"business");
				addStartupGroup(filters,"crypto");
				addStartupGroup(filters,"crypto.bitcoin");
				addStartupGroup(filters,"crypto.ethereum");
				addStartupGroup(filters,"entertainment");
				addStartupGroup(filters,"entertainment.tv");
				addStartupGroup(filters,"entertainment.books");
				addStartupGroup(filters,"entertainment.films");
				addStartupGroup(filters,"entertainment.music");
				addStartupGroup(filters,"food");
				addStartupGroup(filters,"food.recipies");
				addStartupGroup(filters,"food.restaurants");
				addStartupGroup(filters,"games");
				addStartupGroup(filters,"health");
				addStartupGroup(filters,"personal");
				addStartupGroup(filters,"politics");
				addStartupGroup(filters,"sport");
				addStartupGroup(filters,"sport.football");
				addStartupGroup(filters,"sport.cricket");
				addStartupGroup(filters,"sport.rugby");
				addStartupGroup(filters,"technology");
				
				MDS.log("Service Inited")	
			});
		});
	
	}else if(msg.event == "NOTIFYCOIN"){
			
		//Is it the one that matters
		if(msg.data.address ==  SHOUTOUT_ADDRESS){
			
			//Check is Valid amount..
			if(msg.data.coin.tokenid != "0x00"){
				MDS.log("Message not sent as Minima.. ! "+msg.data.coin.tokenid);
				return;
			}else if(+msg.data.coin.amount < 0.01){
				MDS.log("Message below 0.01 threshold.. ! "+msg.data.coin.amount);
				return;
			}
			
			//Add to the DB..
			var msg_category = stripBrackets(msg.data.coin.state[0]);
			if(!checkCategory(msg_category)){
				//ERROR
				MDS.log("Category Invalid : "+msg_category);
				return;
			}
			
			var msg_title 	 = stripBrackets(msg.data.coin.state[1]);
			var msg_message  = stripBrackets(msg.data.coin.state[2]);
			var msg_user 	 = stripBrackets(msg.data.coin.state[3]);
			var msg_randid 	 = stripBrackets(msg.data.coin.state[4]);
			var msg_pubkey 	 = stripBrackets(msg.data.coin.state[5]);
			var msg_sign 	 = stripBrackets(msg.data.coin.state[6]);
			
			//Check non null..
			if(	msg_title 		=="" ||
				msg_category 	=="" ||
				msg_message 	=="" ||
				msg_user 		=="" ||
				msg_randid 		=="" ||
				msg_pubkey	 	=="" ||
				msg_sign 		==""){
				MDS.log("Cannot have blank details in message..");
				return;
			}
			
			//get the address..
			var msg_address  = stripBrackets(msg.data.coin.state[7]);
			
			//Get the categorytitleid
			var cattitid = getCategoryTitleID(msg_category,msg_title);
			
			//Check if message is blocked..
			checkMsgBlocked(msg_pubkey,cattitid,msg_category,function(blocked){
				
				//Is it blocked..
				if(!blocked){
					
					//Check the signature..
					checkMessageSig(msg_category, msg_title, msg_message, 
								msg_user, msg_pubkey, msg_address, msg_randid, msg_sign, function(valid){
									
						if(!valid){
							MDS.log("Invalid signature for "+msg_user);
						}else{
							//Insert unread message - if not already added
							insertMessage(msg_category, msg_title, 
								msg_user, msg_pubkey,msg_address, msg_message, msg_randid, 0, function(inserted){
								
								//Do we notify the User
								if(inserted){
									var cattitleid = getCategoryTitleID(msg_category, msg_title);
									isNotify(cattitleid,function(notify){
										if(notify){
											var notmsg = msg_user+" : "+msg_message;
											if(notmsg.length>30){
												notmsg = notmsg.substring(0,30)+"..";
											}
											MDS.notify(notmsg);		
										}
									});
								}	
							});					
						}
					});	
				}
			});
		}
	}
});		
