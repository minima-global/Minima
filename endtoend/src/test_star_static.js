var staticTests = require('./staticTests.js');

require('chai')
.use(require('chai-as-promised'));
require('chai').assert;

var { Minima_API } = require('minima-api');
var fs = require('fs');
const createCsvWriter = require("csv-writer").createObjectCsvWriter;

const topology = process.env.topology;
const nbNodes = parseInt(process.env.nbNodes);
const nodeFailure = parseInt(process.env.nodeFailure);

console.log("test===>", topology, nbNodes, nodeFailure)

function sleep(ms) {
    return new Promise((resolve) => {
      setTimeout(resolve, ms);
    });
}

test_star_static = function () {
    staticTests.start_static_network_tests(topology, nbNodes, nodeFailure, async function (flag, ip_addrs) {
        console.log("tests collection");

        //wait for processing(should depend on nbNodes but also system performance)
        await sleep(100000);

        if (!fs.existsSync("./results")){
            fs.mkdirSync("./results");
        }

        let current = new Date();

        let csvWriter = createCsvWriter({
            path: flag == 0 ? `./results/result-${current.getDate()}_${current.getMonth()+1}_${current.getFullYear()}-${current.getHours()}_${current.getMinutes()}-part1.csv` 
                            : `./results/result-${current.getDate()}_${current.getMonth()+1}_${current.getFullYear()}-${current.getHours()}_${current.getMinutes()}-part2.csv`,
            header: [
              { id: "node", title: "Node" },
              { id: "ip", title: "IP" },
              { id: "request", title: "Request" },
              { id: "response", title: "Response" },
            ],
        });

        let data = [];

        for(child = 1; child < nbNodes+1; child++) {
            if (flag == 1 && child == nodeFailure) continue;
            console.log("connecting to node " + child + " to verify status.");

            await Minima_API.init(ip_addrs[child.toString()]);

            let midData = {};
            midData["node"] = child;
            midData["ip"] = ip_addrs[child.toString()];

            var status = await Minima_API.status();
            midData["request"] = "status";
            midData["response"] = JSON.stringify(status);
            data.push(midData);
            
            let tempData = {}
            var network = await Minima_API.network();
            tempData["node"] = child;
            tempData["ip"] = ip_addrs[child.toString()];
            tempData["request"] = "network";
            tempData["response"] = JSON.stringify(network);
            data.push(tempData);
        }
        await csvWriter.writeRecords(data)
    })
}

module.exports = test_star_static
