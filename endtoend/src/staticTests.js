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
    node1_args: ["-private", "-clean"], // only node 1 should be started with -private
    node_prefix: "minima-node-",
    HTTP_TIMEOUT: 30000,
    DELAY_BEFORE_TESTS: 5000,
    hostCfg: {  AutoRemove: true, NetworkMode: "minima-e2e-testnet" },
    // unused - can be applied on a node to expose its RPC port on localhost - not needed for our tests
    hostCfgExpose: { AutoRemove: true, NetworkMode: "minima-e2e-testnet", PortBindings: {"9002/tcp": [ { "HostPort": "9002"} ] } },
    host_port: 9002,
    TOPO_STAR: "star",
    TOPO_LINE: "line"
}

var containers = {};
var ip_addrs = {};

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

const start_docker_node_1 = async function (topology, nbNodes, tests_collection) {
    console.log("Creating container 1");
    // Create the container.
    containers["1"] = await createMinimaContainer(cfg.node1_args, cfg.node_prefix + "1", cfg.hostCfg);
    // Start the container.
    await containers["1"].start();
    containers["1"].inspect(function (err, data) {
        ip_addrs["1"] = data.NetworkSettings.Networks[cfg.docker_net].IPAddress;
        start_other_nodes(topology, nbNodes, tests_collection);
    });
}

get_node_args = function(topology, pos) {
    var parent = 0;
    if(topology === cfg.TOPO_STAR) {
        parent = ip_addrs["1"];
    } else if (topology === cfg.TOPO_LINE) {
        parent = ip_addrs['' + (pos - 1)];
    }
    const node_args = ["-connect", parent, "9001"];
    return node_args;
}

start_other_nodes_star = async function(nbNodes, tests_collection) {
    for (let pos = 2; pos < nbNodes+1; pos++) {
        var node_args = get_node_args(cfg.TOPO_STAR, pos);
        console.log("topo star node " + pos + " args: " + node_args);
        containers[pos] = await createMinimaContainer(node_args, cfg.node_prefix + pos, cfg.hostCfg);
        // Start the container.
        await containers[pos].start();
        containers[pos].inspect(function (err, data) {
            console.log("Started node " + pos + " IP:  " + JSON.stringify(data.NetworkSettings.Networks[cfg.docker_net].IPAddress));
            ip_addrs[pos] = data.NetworkSettings.Networks[cfg.docker_net].IPAddress;
            if(pos == nbNodes) { // run tests after we created last node
                // need to sleep to let node sync with others
                setTimeout(function () { tests_collection(ip_addrs) }, cfg.DELAY_BEFORE_TESTS);
            }
        });
      }
}

start_other_nodes_line = async function (nbNodes, pos, tests_collection) {
    if (pos < 2 || pos > nbNodes) {
        return;
    }
    var node_args = get_node_args(cfg.TOPO_LINE, pos);
    console.log("topo line node " + pos + " args: " + node_args);
    containers[pos] = await createMinimaContainer(node_args, cfg.node_prefix + pos, cfg.hostCfg);
    // Start the container.
    await containers[pos].start();
    containers[pos].inspect(function (err, data) {
        console.log("Started node " + pos + " IP:  " + JSON.stringify(data.NetworkSettings.Networks[cfg.docker_net].IPAddress));
        ip_addrs[pos] = data.NetworkSettings.Networks[cfg.docker_net].IPAddress;
        if (pos == nbNodes) { // run tests after we created last node
            // need to sleep to let node sync with others
            setTimeout(function () { tests_collection(ip_addrs) }, cfg.DELAY_BEFORE_TESTS);
        } else {
            start_other_nodes_line(nbNodes, pos+1, tests_collection);
        }
    });
}

start_other_nodes = async function (topology, nbNodes, tests_collection) {
    if(topology === cfg.TOPO_STAR) {
        start_other_nodes_star(nbNodes, tests_collection);
    } else if(topology === cfg.TOPO_LINE) {
        start_other_nodes_line(nbNodes, 2, tests_collection);
    } else {
        console.log("Unsupported topology! This error should be caught earlier.");
        console.log("    topology="+topology);
    }
}

// this function calls HTTP GET on host:endpoint, expects a minima answer, and runs tests_to_run if server success.
run_some_tests_get = async function(host, endpoint, params="", tests) {
    const url =  "http://" + host + ":" + cfg.host_port + endpoint + params;
    
    axios.get(url, {timeout: cfg.HTTP_TIMEOUT}, {maxContentLength: 3000},  {responseType: 'plain'})
    .then(function (response) {
      // handle success
      if(response && response.status == 200) {
          console.log(response.data);
          if(response.data.status == true) {
            tests(response.data.response);
          }
      }
     })
    .catch(function (error) {
      // handle error
      console.log(error);
    })
    .then(function () {
      // always executed
    });
}

// TODO / DO NOT USE
// this function calls HTTP POST on host:endpoint, expects a minima answer, and runs tests_to_run if server success.
run_some_tests_post = async function(host, endpoint, tests_to_run) {
    const url =  "http://" + host + ":" + cfg.host_port + endpoint;
    axios.post(url, {timeout: cfg.HTTP_TIMEOUT}, {maxContentLength: 3000},  {responseType: 'plain'})
    .then(function (response) {
      // handle success
      if(response && response.status == 200) {
          console.log(response.data);
          if(response.data.status == true) {
            tests_to_run(response.data.response);
          }
      }
     })
    .catch(function (error) {
      // handle error
      console.log(error);
    })
    .then(function () {
      // always executed
    });
}

stop_docker_nodes = async function() {
    console.log("stop_docker_nodes");
    // iterate over all running containers and stop them if their name starts with node_prefix
    docker.listContainers({ all:false },
      function (err, containers) {
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
start_static_network_tests = async function (topology, nbNodes, tests_collection) {
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
    setTimeout(function() { start_docker_node_1(topology, nbNodes, tests_collection); }, 5000);    
}

exports.cfg = cfg;
exports.start_static_network_tests = start_static_network_tests;
exports.run_some_tests_get = run_some_tests_get;