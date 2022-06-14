/**
* MAXSOLO backend message handler
* 
* @spartacusrex
*/

//Convert the Event to a JSON
var maximaevent = JSON.parse(window.nodeEvent);

//Convert HEX to UTF8
function hexToUtf8(s)
{
  return decodeURIComponent(
     s.replace(/\s+/g, '') // remove spaces
      .replace(/[0-9A-F]{2}/g, '%$&') // add '%' before each 2 characters
  );
}

//Is it for maxsolo
if(maximaevent.data.application == "maxsolo" ){
	//Relevant data
	var pubkey 	= maximaevent.data.from;
	
	//remove the leading 0x
	var datastr	= maximaevent.data.data.substring(2);
		
	//Convert the data..
	var jsonstr = hexToUtf8(datastr);
		
	//And create the actual JSON
	var maxjson = JSON.parse(jsonstr);
		
	//insert into the DB
	window.executeSQL("INSERT INTO messages (roomname,publickey,username,type,message,filedata,date) VALUES "
			+"('"+maxjson.username+"','"+pubkey+"','"+maxjson.username+"','"+maxjson.type+"','"+maxjson.message+"','"+maxjson.filedata+"', "+Date.now()+")");	
}
