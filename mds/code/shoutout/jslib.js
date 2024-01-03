
//Main Shoutout Address
var SHOUTOUT_ADDRESS = "0x73686F75746F7574206D696E696D61"

//Valid topic RegExp
const VALID_CATEGORY_REGEX = new XRegExp("^[\\p{L}\\p{N}.]*$");

function cleanCategory(incategory){
	
	if(!incategory){
		return "";
	}
	
	//Remove spaces
	var newcat = incategory.split(" ").join("");
	
	//Remove double dots
	while(newcat.indexOf("..") != -1){
		newcat = newcat.split("..").join(".");
	}
	
	if(newcat.startsWith(".")){
		newcat = newcat.substring(1);
	}
	
	if(newcat.endsWith(".")){
		newcat = newcat.substring(0,newcat.length-1);
	}
	
	newcat = newcat.replace(/<\/?[^>]+(>|$)/g, "");
	
	return newcat.toLowerCase();
}

function striptags(text){
	return text.replace(/<\/?[^>]+(>|$)/g, "");
}

function checkCategory(category){
	
	if(!category){
		return false;
	}
	
	//Check is clean
	var valid = true;
	if(cleanCategory(category) != category){
		valid = false;
	}else{
		valid = VALID_CATEGORY_REGEX.test(category);
	}
	
	//Log it..
	if(!valid){
		MDS.log("Invalid check category : "+category);
	}
	
	return valid;
}

function stripBrackets(coinstr){
	
	if(!coinstr){
		return "";
	}
	
	var str = coinstr.trim();
	if(str.startsWith("[")){
		str = str.substring(1);
	}
	
	if(str.endsWith("]")){
		str = str.substring(0,str.length-1);
	}
	
	return str;
}
