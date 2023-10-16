
const VALID_CATEGORY_REGEX = new XRegExp("^[\\p{L}\\p{N}._]*$");

function cleanCategory(category){
	
	//Remove spaces
	var newcat = category.replaceAll(" ","");
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
