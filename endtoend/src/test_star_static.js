var staticTests = require('./staticTests.js');

require('chai')
.use(require('chai-as-promised'));
require('chai').assert;

var { Minima_API } = require('minima-api');

const nbNodes = parseInt(process.env.nbNodes);

console.log("test===>", nbNodes)

function sleep(ms) {
    return new Promise((resolve) => {
      setTimeout(resolve, ms);
    });
} 

test_star_static = function () {
    // number of nodes, list of tests
    staticTests.start_static_network_tests("star", nbNodes, async function (ip_addrs) {
        console.log("tests collection");

        await sleep(30000);

        for(child = 1; child < nbNodes+1; child++) {
            console.log("connecting to node " + child + " to verify status.");
            await Minima_API.init(ip_addrs[child.toString()]);

            await Minima_API.status().then(res => {
                console.log("status====>", res)
            });
    
            await Minima_API.network().then(res => {
                console.log("network====>", res)
            }); 
        }
    })
}


module.exports = test_star_static
