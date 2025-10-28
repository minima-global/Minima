/**
 * Utility functions
 */

//SET Decimal JS precision
Decimal.set({precision: 60,defaults: true});

//A ZERO value
var DECIMAL_ZERO = new Decimal(0);

//MAX decimal places..
var MAX_DECIMALS	 = 16;

function genRandomHexString() {
    const hex = '0123456789ABCDEF';
    let output = '';
    for (let i = 0; i < 24; ++i) {
        output += hex.charAt(Math.floor(Math.random() * hex.length));
    }
    return "0x"+output;
}

function getTimeMilli(){
	//Date as of NOW
	var recdate = new Date();
	return recdate.getTime();	
}

function trimToSize(str, len){
	
	//Is it undefined..
	if(!str){
		return "undefined..";
	}
	
	if(len){
		return str.substring(0,len)+"..";	
	}else{
		return str.substring(0,10)+"..";
	}
}

function encodeStringForDB(str){
	return encodeURIComponent(str).split("'").join("%27");
}

function decodeStringFromDB(str){
	return decodeURIComponent(str).split("%27").join("'");
}

//Check this is a safe and valid hashid
function checkSafeHashID(hashid){
	var regex = new RegExp("^[A-Za-z0-9]*$");
	return regex.test(hashid); 
}

function logJSON(json, title){
	if(title){
		MDS.log(title+" : "+JSON.stringify(json,null,2));
	}else{
		MDS.log(JSON.stringify(json,null,2));	
	}
}


function calculateNewValues(sqlrow, amount, touser){
	
	var ret={};
	ret.valid = true;
	
	var amount 	= new Decimal(amount);
	var user1 	= new Decimal(sqlrow.USER1AMOUNT);
	var user2 	= new Decimal(sqlrow.USER2AMOUNT);
	var total 	= new Decimal(sqlrow.TOTALAMOUNT);
	
	if(touser == 1){
		ret.useramount1	= user1.plus(amount);
		ret.useramount2	= user2.sub(amount);
	}else{
		ret.useramount1	= user1.sub(amount);
		ret.useramount2	= user2.plus(amount);
	}

	//And now do some checks
	if(amount.isNegative()){
		ret.valid = false;
	}else if(ret.useramount1.isNegative() || ret.useramount2.isNegative()){
		ret.valid = false;
	}
		
	return ret;	
}

function checkStartValues(amount1, amount2){
	 
	var user1 	= new Decimal(amount1);
	var user2 	= new Decimal(amount2);
	
	//Check ONE is not zero..
	if(user1.equals(DECIMAL_ZERO) && user2.equals(DECIMAL_ZERO)){
		return false; 
	}
	
	///Check neither is below xero
	if(user1.lessThan(DECIMAL_ZERO) || user2.lessThan(DECIMAL_ZERO)){
		return false; 
	}
		
	return true;	
}

function getValidDecimalNumber(num){
	return new Decimal(num).toDecimalPlaces(MAX_DECIMALS);
}

function removeItemOnce(arr, value) {
  var index = arr.indexOf(value);
  if (index > -1) {
    arr.splice(index, 1);
  }
  return arr;
}

function checkBalance(amount, tokenid, callback){
	
	MDS.cmd("balance",function(bal){
		var required = new Decimal(amount);
		
		if(required.equals(DECIMAL_ZERO)){
			callback(true);
			return;
		}
		
		for(var i=0;i<bal.response.length;i++){
			var balance = bal.response[i];
			
			if(balance.tokenid == tokenid){
				if(required.lessThanOrEqualTo(new Decimal(balance.confirmed))){
					callback(true);
					return;
				}	
			}
		}
		
		callback(false);
	});
}

