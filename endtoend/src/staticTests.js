var Docker = require('dockerode');
var docker = new Docker({socketPath: '/var/run/docker.sock'});
const axios = require('axios').default;
const BN = require('bn.js');

require('chai')
        .use(require('chai-as-promised'))
//        .use(require('chai-bn')(require('bn.js')))
.use(require('chai-bn')(BN))
//        .use(require('chai-bignumber')(BigNumber))
        .should();

require('chai').assert;

// *** config ***
const cfg = {
    image: 'minima:latest',  // docker image name to run -> can be customised
    docker_net: "minima-e2e-testnet", // docker private network name -> MUST BE CREATED MANUALLY
    node1_args: ["-private"], // only node 1 should be started with -private
    node_prefix: "minima-node-",
    HTTP_TIMEOUT: 30000,
    DELAY_BEFORE_TESTS: 5000,
    hostConfig1: {  
	    AutoRemove: true, // comment this out to inspect stopped containers
            NetworkMode: "minima-e2e-testnet",  
	    'Binds': ['/Users/jeromerousselot/src/minima/Minima/node1/p2p:/root/.minima/p2p'],
            CpuShares: 10,   // node 1 in private mode uses auto-mining and aim for 100% CPU usage, so we throttle it
    },
    hostConfig:  {  
            AutoRemove: true, // comment this out to inspect stopped containers
            NetworkMode: "minima-e2e-testnet",
            CpuShares: 10,
    },
    // unused - can be applied on a node to expose its RPC port on localhost - not needed for our tests
    hostCfgExpose: { NetworkMode: "minima-e2e-testnet", PortBindings: {"9002/tcp": [ { "HostPort": "9002"} ] } },
    host_port: 9002,
    TOPO_STAR: "star",
    TOPO_LINE: "line"
}

var containers = {};
var ip_addrs = {};
var p2pdiscoveryaddr = "";
var p2penr = "";

createMinimaContainer = async function(cmd, name, hostConfig) {
    return await docker.createContainer({
        AttachStderr: false, AttachStdin: false, AttachStdout: false,
        Cmd: cmd,
        Image: cfg.image,
        OpenStdin: false, StdinOnce: false, Tty: false,
        name: name,
        HostConfig: hostConfig
    });
}

function sleep(ms) {
  return new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
} 

const start_docker_node_1 = async function (topology, nbNodes, tests_collection) {
    console.log("Creating container 1");
    // Create the container.
    containers["1"] = await createMinimaContainer(cfg.node1_args, cfg.node_prefix + "1", cfg.hostConfig1);
    // Start the container.
    await containers["1"].start();
    process.stdout.write("Trying to sleep for 5 seconds...");
    await sleep(10000);
//    process.stdout.write("Did I sleep 5 seconds?");

    console.log("Hello1");
    await runContainerInspect(topology, nbNodes, tests_collection);
}

const runContainerInspect = (topology, nbNodes, tests_collection) => {
    return new Promise((resolve, reject) => {
        containers["1"].inspect(async function(err, data) {
            ip_addrs["1"] = data.NetworkSettings.Networks[cfg.docker_net].IPAddress;
            console.log("Started node 1," + " IP:  " + JSON.stringify(data.NetworkSettings.Networks[cfg.docker_net].IPAddress));
            get_node_p2p_params(ip_addrs["1"], function() {
                resolve(start_other_nodes(topology, nbNodes, tests_collection));    
            });
        })
    })
}

get_node_args = function(topology, pos) {
    var parent = 0;
    if(topology === cfg.TOPO_STAR) {
        parent = ip_addrs["1"];
    } else if (topology === cfg.TOPO_LINE) {
        parent = ip_addrs['' + (pos - 1)];
    }
    p2p = true;
    var node_args;
    if(p2p) {

         // these two fields must be retrieved programmatically from node1
         // node_args = ["-p2p-static","/ip4/172.18.0.2/udp/11522/p2p/16Uiu2HAkvSYiDo3G4Cw7XVicPK5BMjgm8vMHKYtGNCWQvskV3RdQ","-p2p-bootnode","enr:-Iu4QDirGhMYfgvNha7PVhMshqn1INf8ZjV2As0YkMgszLR1OlglWz68HjTLNxUml_BHbNGmq1C9zM3OyQiJzjX6YJYBgmlkgnY0gmlwhKwSAAKJc2VjcDI1NmsxoQIPFQyakHo15u_GazoWP_L3Qboxkjgpv2gK-Des9SMZj4N0Y3CCLQKDdWRwgi0C"];
         node_args = ["-p2p-static", p2pdiscoveryaddr, "-p2p-bootnode", p2penr];
    } else {
         node_args = ["-connect", parent, "9001"];
    }
    return node_args;
}

start_other_nodes_star = async function(nbNodes, tests_collection) {
    for (let pos = 2; pos < nbNodes+1; pos++) {
        var node_args = get_node_args(cfg.TOPO_STAR, pos);
        console.log("topo star node " + pos + " args: " + node_args);
        containers[pos] = await createMinimaContainer(node_args, cfg.node_prefix + pos, cfg.hostConfig);
        // Start the container.

        await containers[pos].start();

        await sleep(10000);
        console.log("node " + pos);
        await starContainerInspect(pos, nbNodes, tests_collection);
      }
}

const starContainerInspect = (pos, nbNodes, tests_collection) => {
    return new Promise((resolve) => {
        containers[''+pos].inspect(async function(err, data) {
            console.log("Started node " + pos + " IP:  " + JSON.stringify(data.NetworkSettings.Networks[cfg.docker_net].IPAddress));
            ip_addrs[pos] = data.NetworkSettings.Networks[cfg.docker_net].IPAddress;
            if(pos == nbNodes) {
                await sleep(5000);
                resolve(tests_collection(0, ip_addrs))
            } else {
                resolve(null)
            }
        })
    })
}

start_other_nodes_line = async function (nbNodes, pos, tests_collection) {
    if (pos < 2 || pos > nbNodes) {
        return;
    }
    var node_args = get_node_args(cfg.TOPO_LINE, pos);
    console.log("topo line node " + pos + " args: " + node_args);
    containers[pos] = await createMinimaContainer(node_args, cfg.node_prefix + pos, cfg.hostConfig);

    // Start the container.
    await containers[pos].start();
    await sleep(10000);

    await lineContainerInspect(pos, nbNodes, tests_collection);
}

const lineContainerInspect = (pos, nbNodes, tests_collection) => {
    return new Promise((resolve) => {
        containers[''+pos].inspect(async function(err, data) {
            console.log("Started node " + pos + " IP:  " + JSON.stringify(data.NetworkSettings.Networks[cfg.docker_net].IPAddress));
            ip_addrs[pos] = data.NetworkSettings.Networks[cfg.docker_net].IPAddress;
            if(pos == nbNodes) {
                await sleep(5000);
                resolve(tests_collection(0, ip_addrs))
            } else {
                resolve(start_other_nodes_line(nbNodes, pos+1, tests_collection))
            }
        })
    })
}

start_other_nodes = async function (topology, nbNodes, tests_collection) {
    if(topology === cfg.TOPO_STAR) {
        await start_other_nodes_star(nbNodes, tests_collection);
    } else if(topology === cfg.TOPO_LINE) {
        await start_other_nodes_line(nbNodes, 2, tests_collection);
    } else {
        console.log("Unsupported topology! This error should be caught earlier.");
        console.log("    topology=" + topology);
    }
}

get_node_p2p_params = function(host, cb) {
    const url =  "http://" + host + ":" + cfg.host_port + '/status';
    var disc = '';
    var enr = '';
    axios.get(url, {timeout: cfg.HTTP_TIMEOUT}, {maxContentLength: 3000},  {responseType: 'plain'})
    .then(function (response) {
      // handle success
      if(response && response.status == 200) {
          console.log("received axios answer code 200");
          //console.log("response: " + JSON.stringify(response));
          console.log("response.data.response: " + response.data.response);
          //data = JSON.parse(response.data);
          console.log("response.data.status: " + response.data.status);
          console.log("response.data.response.p2pEnr: " + response.data.response.p2pEnr);
          //TODO: check values for p2pEnr and p2pDiscoveryAddr, block if empty
          //console.log("data: " + data);
          //console.log("data.status: " + data.status);
          //console.log("data.response.p2penr: " + data.response.p2penr);
          if(response.data.status == true) {
	        console.log("received data with status = true, extracting p2p fields");
            disc =  response.data.response.p2pDiscoveryaddr;
            enr  =  response.data.response.p2pEnr;
            console.log("disc=" + disc + " enr=" + enr);
            p2pdiscoveryaddr = disc;
            p2penr = enr;
            cb();
          } else {
	        console.log("json data incorrect, not calling callback");
            console.log("data.status was: " + response.data.status);
          }
      }
     })
    .catch(function (error) {
      // handle error
      console.log("axios error handler:");
      console.log(error);
    })
    .then(function () {
      // always executed
    });
}

stop_docker_nodes = async function() {
    console.log("stop_docker_nodes");
    // iterate over all running containers and stop them if their name starts with node_prefix
    docker.listContainers({ all:false }, function (err, containers) {
        if(containers) { 
            containers.forEach(function (containerInfo) {
                if(containerInfo.Names[0].startsWith("/" + cfg.node_prefix)) {
                    console.log ("Found a minima node(" + containerInfo.Names[0] + "), stopping.");
                    docker.getContainer(containerInfo.Id).stop();
                }
            });
        } else {
            console.log("Found no running docker instances\n");
        }
    });
}

// setup a network of nbNodes minima nodes in star topology and runs tests_collection on it with argument ip_addrs[node_prefix+ "01"] .
start_static_network_tests = async function (topology, nbNodes, nodeFailure, tests_collection) {
    if(!(topology === cfg.TOPO_STAR || topology === cfg.TOPO_LINE)) {
        console.log("Error! Unsupported topology: " + topology);
        return;
    }
    if(tests_collection == null) {
        console.log("Error! Missing tests callback.");
        return;
    }
    if(nbNodes < 1) {
        console.log("Error! Unsupported number of nodes:" + nbNodes);
        return;
    }
    if(nbNodes > 10) {
        console.log("Warning! High number of nodes, tests may fail due to unresponsive nodes.\n Proceeding anyway.\n\n");
    }
    await stop_docker_nodes();
    // give 5 seconds to stop all docker nodes (should depend on nbNodes but also system performance)
    await sleep(5000);
    await start_docker_node_1(topology, nbNodes, tests_collection);    

    //stop one node and run tests
    await containers[''+nodeFailure].stop();
    console.log("node " + nodeFailure + " stopping...");
    await sleep(10000);
    await tests_collection(1, ip_addrs);
}

exports.cfg = cfg;
exports.start_static_network_tests = start_static_network_tests;
