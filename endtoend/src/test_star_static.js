var staticTests = require('./staticTests.js');

require('chai')
.use(require('chai-as-promised'));
require('chai').assert;

var { Minima_API } = require('minima-api');
var fs = require('fs');

const topology = process.env.topology;
const nbNodes = parseInt(process.env.nbNodes);
const nodeFailure = parseInt(process.env.nodeFailure);

console.log("test===>", topology, nbNodes, nodeFailure)

function sleep(ms) {
    return new Promise((resolve) => {
      setTimeout(resolve, ms);
    });
} 

function writeFunction(data) {
    
    return new Promise((resolve, reject) => {
        fs.appendFile("./log/result.txt", data, function(err) {
            if (err) {
                reject(err);
            } else {
                resolve(null)
            }
        });
    })
}

test_star_static = function () {
    staticTests.start_static_network_tests(topology, nbNodes, nodeFailure, async function (flag, ip_addrs) {
        console.log("tests collection");

        //wait for processing(should depend on nbNodes but also system performance)
        await sleep(60000);

        if (!fs.existsSync("./log")){
            fs.mkdirSync("./log");
        }

        if (flag == 0) {
            await writeFunction("The Result before stopping node.\n")
        }

        if(flag == 1) {
            await writeFunction("\n\n\nThe Result after stopping node " + nodeFailure + "\n");
        }

        for(child = 1; child < nbNodes+1; child++) {
            if (flag == 1 && child == nodeFailure) continue;
            console.log("connecting to node " + child + " to verify status.");

            await Minima_API.init(ip_addrs[child.toString()]);

            var status = await Minima_API.status();
            var data = "\n " + ip_addrs[child.toString()] + ":\nStatus: \n" + JSON.stringify(status) + "\n";
            await writeFunction(data);
    
            var network = await Minima_API.network();
            data = "network: \n" + JSON.stringify(network) + "\n";
            await writeFunction(data);
        }
    })
}


module.exports = test_star_static
