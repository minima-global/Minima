
//const VALID_CATEGORY_REGEX = new RegExp("^[\p{L}0-9.]*$");
//const VALID_CATEGORY_REGEX = new XRegExp("^[\\p{L}0-9._]*$");
const VALID_CATEGORY_REGEX = new XRegExp("^[\\p{L}\\p{N}._]*$");

//const VALID_CATEGORY_REGEX = new XRegExp("^[a-z0-9.]*$");

function checkCategory(category){
	var valid = VALID_CATEGORY_REGEX.test(category);
	MDS.log("REGEX : "+category+" "+valid);
	return valid;
}