var staticTests = require('./staticTests.js');

require('chai')
.use(require('chai-as-promised'));
require('chai').assert;

const nbNodes = 10;


function sleep(ms) {
    return new Promise((resolve) => {
      setTimeout(resolve, ms);
    });
} 

test_star_static = function () {
    // number of nodes, list of tests
    staticTests.start_static_network_tests("star", nbNodes, function (ip_addrs) {
        console.log("tests collection");

        // test 0: healthcheck - are we connected?
        // curl -s 127.0.0.1:9002/status | jq '.response.connections'
        staticTests.run_some_tests_get(ip_addrs["1"], '/status', "", function (response) {
            response.connections.should.be.above(0);
            //response.connections.should.be.equal((nbNodes-1)); // be at least connected to each node
        });

        staticTests.run_some_tests_get(ip_addrs["3"], '/status', "", function (response) {
            response.connections.should.be.above(0);
            //response.connections.should.be.equal((nbNodes-1));
        });
        //1. send funds with no money and assert failure
        //staticTests.run_some_tests_get(ip_addrs["1"], '/send', {"amount": 1, "address": "0xFF", "tokenid": "0x00"}, 
        staticTests.run_some_tests_get(ip_addrs["1"], '/send', params="+1+0xFF", 
            tests=function (response) {
                console.log("chai tests not written yet for this test, just printing answer.");
                console.log("send response: " + JSON.stringify(response));
        });

        // 2. generate 50 coins
        staticTests.run_some_tests_get(ip_addrs["1"], '/gimme50', "", tests = function (response) {
            //        response.connections.should.be.above(0); 
            //        response.chainlength.should.be.above(1);
                console.log("chai tests not written yet for this test, just printing answer.");
                console.log("gimme50 response: " + JSON.stringify(response));
        });

        // 3. send funds with money
        //staticTests.run_some_tests_get(ip_addrs["1"], '/send', {"amount": 1, "address": "0xFF", "tokenid": "0x00"}, 
        setTimeout(
                function () { 
                    staticTests.run_some_tests_get(
                        ip_addrs["1"], 
                        '/send', 
                        params="+1+0xFF", 
                        tests=function (response) {
                            //console.log("send response: " + JSON.stringify(response.txpow.body.txn));
                            console.log("received minima response to send tx, verifying tx fields.");
                            response.txpow.body.txn.inputs[0].amount.should.be.equal("25");
                            response.txpow.body.txn.outputs[0].amount.should.be.equal("1");
                            response.txpow.body.txn.outputs[1].amount.should.be.equal("24");
                            response.txpow.body.txn.outputs[0].address.should.be.equal("0xFF");
                        }
                    )
                }, 
                10000);
        
        // await sleep(30*1000);
        setTimeout(
            function() {
                for(child = 1; child < nbNodes; child++) {
                    console.log("connecting to node " + child + " to verify status.");
                    staticTests.run_some_tests_get(
                        ip_addrs[child.toString()], 
                        '/status', 
                        "", 
                        function (response) {
                            console.log("status for node " + child + " received, verifying correct number of Minima socket connections.");
                            response.connections.should.be.above(0);
                            response.connections.should.be.equal((nbNodes-1)*2);
                        }
                    );
                }
            }, 
            30000
        );

     });
    
}

module.exports = test_star_static
