/**
 * SCRIPT IDE JS
 */

function runScript(){
	var txt = document.getElementById("scriptarea").value.trim();
	
	//Check for Killer Characters..
	if(txt.indexOf(",")!=-1){alert("NO commas Allowed in Scripts!");return;}
	if(txt.indexOf("\"")!=-1 || txt.indexOf("'")!=-1){alert("NO Quotes Allowed in Scripts!");return;}
	if(txt.indexOf(":")!=-1 || txt.indexOf(";")!=-1){alert("NO semi-colons Allowed in Scripts!");return;}
	
	//Save if you run..
	if(document.getElementById("autosave").checked){
		save();	
	}
	
	//Get the script
	var script     	= txt.replace(/\n/g," ").trim();
	script 			= parseComments(script).trim();
	if(script == ''){return;}
	
	var state = document.getElementById("state").value.trim();
	if(state != ""){state = "state:"+state;}
	
	var prevstate  = document.getElementById("prevstate").value.trim();
	if(prevstate != ""){prevstate = "prevstate:"+prevstate;}
	
	var globals    = "globals:"+getGlobals();
	
	var sigs       = document.getElementById("sigs").value.trim();
	if(sigs != ""){sigs = "signatures:"+sigs;}
	
	var scripts    = document.getElementById("mastscripts").value.trim();
	if(scripts != ""){scripts = "extrascripts:"+sigs;}
	
	MDS.cmd("runscript script:\""+script+"\" "+state+" "+prevstate+" "+globals+" "+sigs+" "+scripts,function(json){
		
		//console.log("RESULT : "+JSON.stringify(json));
		
		var brkscr = json.response.trace.replace(/\n/g,"<br>");
		
		var res = "---------------------------------<br>";
		if(json.response.parseok){
			res += "PARSE OK : ";	
		}else{
			res += "PARSE FAIL : ";	
		}
		
		if(json.response.success){
			res += "RUN OK : ";	
		}else{
			res += "RUN FAIL : ";	
		}
		
		if(json.response.monotonic){
			res += "MONOTONIC ";	
		}else{
			res += "NOT MONOTONIC ";	
		}
		
		//Set it
		document.getElementById("parse").innerHTML = res+"<br>---------------------------------<br>"+brkscr;	
		document.getElementById("parse").scrollTop = 0;
		
		//Set some detais
		document.getElementById("clean").innerHTML = json.response.clean.script;
		document.getElementById("clean").scrollTop = 0;
		
		//Set the global address
		document.getElementById("cleanaddress").innerHTML = json.response.clean.address;
		document.getElementById("cleanmxaddress").innerHTML = json.response.clean.mxaddress;
		document.getElementById("@ADDRESS").value = json.response.clean.address;
	});
	
	return;
	 
}

var globals = {};
function addGlobalIfValid(globalname){
	if(document.getElementById(globalname).value.trim() !== ""){
		globals[globalname+""] = document.getElementById(globalname).value.trim();
		//globals += globalname+":"+document.getElementById(globalname).value.trim()+"#";
	} 
}
function getGlobals(){
	globals = {};
	
	addGlobalIfValid("@BLOCK");
	addGlobalIfValid("@CREATED");
	addGlobalIfValid("@INPUT");
	addGlobalIfValid("@AMOUNT");
	addGlobalIfValid("@TOKENID");
	addGlobalIfValid("@COINID");
	addGlobalIfValid("@TOTIN");
	addGlobalIfValid("@TOTOUT");
	addGlobalIfValid("@COINAGE");
	addGlobalIfValid("@CREATED");
	
	return JSON.stringify(globals);
}

function clearGlobals(){
	document.getElementById("@BLOCK").value = "";
	document.getElementById("@CREATED").value = "";
	document.getElementById("@INPUT").value = "";
	document.getElementById("@ADDRESS").value = "";
	document.getElementById("@AMOUNT").value = "";
	document.getElementById("@TOKENID").value = "";
	document.getElementById("@COINID").value = "";
	document.getElementById("@TOTIN").value = "";
	document.getElementById("@TOTOUT").value = "";
	document.getElementById("@COINAGE").value = "";
	document.getElementById("@CREATED").value = "";
}

function getOutputString(){
	var outputs    = "";
	var table = document.getElementById("outputtable");
	var rows  = table.getElementsByTagName("tr")
	var len   = rows.length;
	for(i=2;i<len;i++){
		var address = getTableValue( document.getElementById("table_address_"+i).innerHTML );
		var amount  = getTableValue( document.getElementById("table_amount_"+i).innerHTML );
		var tokenid = getTableValue( document.getElementById("table_tokenid_"+i).innerHTML );
		
		outputs += address+":"+amount+":"+tokenid+"#";
	}
	
	return outputs;
}

//Deafult - Save the selected 
function save(){
	saveScript(document.getElementById("scripts").selectedIndex);
}

function saveScript(sel){
	//Get all this different parts..
	var scriptarea = document.getElementById("scriptarea").value;
	var state      = document.getElementById("state").value;
	var prevstate  = document.getElementById("prevstate").value;
	var sigs       = document.getElementById("sigs").value;
	var scripts    = document.getElementById("mastscripts").value;
	var outputs    = "";
	var globals    = getGlobals();
	
	//Create a JSON out of it..
	var storejson = { "script":scriptarea, 
						"state":state, 
						"prevstate":prevstate, 
						"sigs":sigs, 
						"outputs":outputs,
						"scripts":scripts,
						"globals":globals
					};
	
	//Convert the whole thing to a tring
	var jsontext = JSON.stringify(storejson);
	console.log("SAVE JSON:"+JSON.stringify(storejson,null,2));
	
	//Now store it..
    window.localStorage.setItem('ScriptIDE'+sel,jsontext);
}

var prevsel = 0;
function loadScript(){
	//Load cached if available..
	var sel = document.getElementById("scripts").selectedIndex;
	
	//Save the OLD
	if(document.getElementById("autosave").checked){
		saveScript(prevsel);
	}
	prevsel = sel;
	
	//Load the JSON
	var jsontext = window.localStorage.getItem('ScriptIDE'+sel);
	//console.log("LOAD JSON:"+jsontext);
	
	if(jsontext != null){
		//Convert to a JSON
		var json = JSON.parse(jsontext);
		
		document.getElementById("scriptarea").value 	= json.script;
		document.getElementById("state").value      	= json.state;
		document.getElementById("prevstate").value  	= json.prevstate;
		document.getElementById("mastscripts").value  	= json.scripts;
		document.getElementById("sigs").value       	= json.sigs;
		
		//Load the OUTPUTS..
		/*clearAllOutputs();
		var outs = json.outputs.split("#");
		outlen   = outs.length;
		for(i=0;i<outlen;i++){
			if(outs[i] !== ""){
				var out = outs[i].split(":");
				addOutput(out[0],out[1],out[2]);	
			}
		}*/
		
		//Load the GLOBALS..
		clearGlobals();
		var globs = json.globals.split("#");
		
		//Create a JSON and put the details in the IDE
		var globobj = JSON.parse(globs);
		for(data in globobj){
			document.getElementById(data).value = globobj[data];
		}
		
	}else{
		//Reset the values..
		document.getElementById("scriptarea").value = "";
		document.getElementById("state").value      = "";
		document.getElementById("prevstate").value  = "";
		document.getElementById("sigs").value       = "";
		
		clearGlobals();
		//clearAllOutputs();
	}
	
	//reset..
	document.getElementById("parse").innerHTML = "";
	document.getElementById("clean").innerHTML = "";
}

/**
* The OUTPUTS table
*/
function addDefault(){
	var addr = document.getElementById("output_address").value;
	var amt  = document.getElementById("output_amount").value;
	var tok  = document.getElementById("output_tokenid").value;
	addOutput(addr,amt,tok);
}

function addOutput(addr,amt,tok){
	var table = document.getElementById("outputtable");
	var rows  = table.getElementsByTagName("tr")
	var len   = rows.length;
	var out   = len-2;
	
	var row     = table.insertRow(len);
	var output  = row.insertCell(0);
	var address = row.insertCell(1);
	var amount  = row.insertCell(2);
	var tokenid = row.insertCell(3);
	
	//Data..
	output.id = "table_output_"+len;
	output.innerHTML = ""+out;
	
	address.id = "table_address_"+len;
	address.innerHTML = "<input size=30 type=text value=\""+addr+"\">";
	
	amount.id = "table_amount_"+len;
	amount.innerHTML = "<input size=20 type=number value=\""+amt+"\">";
	
	tokenid.id = "table_tokenid_"+len;
	tokenid.innerHTML = "<input size=30 type=text value=\""+tok+"\">";
}

function getTableValue(fullhtml){
	start = fullhtml.indexOf("value=\"")+7;
	end   = fullhtml.indexOf("\"",start);
	ret   = fullhtml.substring(start,end); 
	return ret; 
}

function deleteOutput(){
	var table = document.getElementById("outputtable");
	var rows  = table.getElementsByTagName("tr")
	var len   = rows.length;
	
	if(len>2){
		table.deleteRow(len-1);	
	}
}

function clearAllOutputs(){
	var table = document.getElementById("outputtable");
	var rows  = table.getElementsByTagName("tr")
	var len   = rows.length;
	
	for(i=2;i<len;i++){
		deleteOutput();
	}
	
	document.getElementById("output_address").value = "0x00";
	document.getElementById("output_amount").value = "0";
	document.getElementById("output_tokenid").value = "0x00";
}

function parseComments(code){
    // state
    var isInRegExp = false;
    var isInString = false;
    var terminator = null; // to hold the string terminator
    var escape = false; // last char was an escape
    var isInComment = false;

    var c = code.split(""); // code

    var o = []; // output
    for(var i = 0; i < c.length; i++){
        if(isInString) {  // handle string literal case
             if(c[i] === terminator && escape === false){
                  isInString = false;
                  o.push(c[i]);
             } else if (c[i] === "\\") { // escape
                  escape = true;
             } else {
                  escape = false;
                  o.push(c[i]); 
             }
        } else if(isInRegExp) { // regular expression case
             if(c[i] === "/" && escape === false){
                 isInRegExp = false;
                 o.push(c[i]);
             } else if (c[i] === "\\") {
                 escape = true;
             } else { 
                escape = false;
                o.push(c[i]);
             }
        } else if (isInComment) { // comment case
              if(c[i] === "*" && c[i+1] === "/"){
                  isInComment = false;
                  i++;
                  // Note - not pushing comments to output
              }
        } else {   // not in a literal
              if(c[i] === "/" && c[i+1] === "/") { // single line comment
                   while(c[i] !== "\n" && c[i] !== undefined){ //end or new line
                       i++;
                   }
              } else if(c[i] === "/" && c[i+1] === "*"){ // start comment
                    isInComment = true;
                    o.push(" "); // add a space per spec
                    i++; // don't catch /*/
              //} else if(c[i] === "/"){ // start regexp literal
              //      isInRegExp = true;
              //      o.push(c[i]);
              } else if(c[i] === "'" || c[i] === '"'){ // string literal
                    isInString = true;
                    o.push(c[i]);
                    separator = c[i];
              } else { // plain ol' code
                    o.push(c[i]);
              }
        }
    }
    return o.join("");
}