
const VALID_CATEGORY_REGEX = new XRegExp("^[\\p{L}\\p{N}._]*$");

function cleanCategory(incategory){
	
	//Remove spaces
	var newcat = incategory.replaceAll(" ","");
	while(newcat.indexOf("..") != -1){
		newcat = newcat.replaceAll("..",".");
	}
	
	if(newcat.startsWith(".")){
		newcat = newcat.substring(1);
	}
	
	if(newcat.endsWith(".")){
		newcat = newcat.substring(0,newcat.length-1);
	}
	
	return newcat;
}

function checkCategory(category){
	var valid = VALID_CATEGORY_REGEX.test(category);
	MDS.log("REGEX : "+category+" "+valid);
	return valid;
}

function stripBrackets(coinstr){
	
	var str = coinstr.trim();
	if(str.startsWith("[")){
		str = str.substring(1);
	}
	
	if(str.endsWith("]")){
		str = str.substring(0,str.length-1);
	}
	
	return str;
}
