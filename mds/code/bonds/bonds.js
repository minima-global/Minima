
var BOND_SCRIPT = "LET yourkey=PREVSTATE(100) IF SIGNEDBY(yourkey) THEN RETURN TRUE ENDIF LET maxblock=PREVSTATE(101) LET youraddress=PREVSTATE(102) LET maxcoinage=PREVSTATE(104) LET fcfinish=STATE(1) LET fcpayout=STATE(2) LET fcmilli=STATE(3) LET fccoinage=STATE(4) ASSERT fcpayout EQ youraddress ASSERT fcfinish LTE maxblock ASSERT fccoinage LTE maxcoinage LET fcaddress=0xEA8823992AB3CEBBA855D68006F0D05B0C4838FE55885375837D90F98954FA13 LET fullvalue=@AMOUNT*1.1 RETURN VERIFYOUT(@INPUT fcaddress fullvalue @TOKENID TRUE)";

function requestBond(amount,bondtype){
	
	
	//Get one of your addresses
	MDS.cmd("getaddress;status",function(resp){
		//MDS.log(JSON.stringify(resp));	
		
		//What is the current block
		var block = resp[1].response.chain.block;
		
		//Calculate the max values..
		var maxblock 	= block+100000;
		var maxcoinage 	= 100000;
		var timemilli 	= (new Date()).getTime();
		
		var address 	= resp[0].response.miniaddress;
		var pubkey  	= resp[0].response.publickey;
		 
		var statevars = "{\"100\":\""+pubkey+"\","
						+"\"101\":\""+maxblock+"\","
						+"\"102\":\""+address+"\","
						+"\"103\":\""+timemilli+"\","
						+"\"104\":\""+maxcoinage+"\","
						+"\"105\":\""+bondtype+"\""
						+"}";	
			
		var cmd = "send amount:"+amount
				+" address:MxG084WU2W8JUFFKWP4WUSYKGMY1VZTR1MUY7KP9AAMAG85Q7W10NQ80R2A15PU "
				+" state:"+statevars;	
		
		MDS.cmd(cmd,function(resp){
			//MDS.log(JSON.stringify(resp));
			
		});
	});
	
}

function cancelBond(coinid,amount,pubkey){

	MDS.cmd("getaddress",function(resp){
	
		//Get an address
		var address = resp.response.miniaddress;
	
		//Random ID
		var randid = Math.floor(Math.random() * 1000000000)+"";
		
		//Construct and spend back txn..
		var txn = "txncreate  id:"+randid
				 +";txninput  id:"+randid+" coinid:"+coinid
				 +";txnoutput id:"+randid+" address:"+address+" amount:"+amount+" storestate:false"
				 +";txnsign   id:"+randid+" publickey:"+pubkey
				 +";txnpost   id:"+randid+" auto:true"
				 +";txndelete id:"+randid;
				
		MDS.cmd(txn,function(resp){
			MDS.log(JSON.stringify(resp));
			
		});			
	});
	
	
}