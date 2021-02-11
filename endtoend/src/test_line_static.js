var staticTests = require('./staticTests.js');

const nbNodes = 3;

test_line_static = function () {
    // number of nodes, list of tests
    staticTests.start_static_network_tests(nbNodes, function (ip_addrs) {
        console.log("tests collection");

        // test 0: healthcheck - are we connected?
        // curl -s 127.0.0.1:9002/status | jq '.response.connections'
        staticTests.run_some_tests_get(ip_addrs["1"], '/status', function (response) {
            response.connections.should.be.above(0);
            response.connections.should.be.equal(nbNodes-1);
            response.chainlength.should.be.above(3);
        });

        //todo: send funds with no money and assert failure

        // 2. generate 50 coins
        staticTests.run_some_tests_get(ip_addrs["1"], '/gimme50', function (response) {
            //        response.connections.should.be.above(0); 
            //        response.chainlength.should.be.above(1);
            console.log("gimme50 response: " + JSON.stringify(response));
        });

        // 3. send funds with money
    }
    );

}

module.exports = test_line_static
