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
const image = 'minima:latest';  // docker image name to run -> can be customised
const docker_net = "minima-e2e-testnet"; // docker private network name -> MUST BE CREATED MANUALLY
const node1_args = ["-private", "-clean"]; // only node 1 should be started with -private
const node_prefix = "minima-node-";
const HTTP_TIMEOUT = 30000;
const hostCfg =  {  AutoRemove: true, NetworkMode: docker_net };

// unused - can be applied on a node to expose its RPC port on localhost - not needed for our tests
const hostCfgExpose = { AutoRemove: true, NetworkMode: docker_net, PortBindings: {"9002/tcp": [ { "HostPort": "9002"} ] } };

const host_port = 9002;

var nodes_args; // all other nodes get same args

var container01;

var containers = {};
var ip_addrs = {};

createMinimaContainer = async function(cmd, name, hostConfig) {
    return await docker.createContainer({
        AttachStderr: false, AttachStdin: false, AttachStdout: false,
        Cmd: cmd,
        Image: image,
        OpenStdin: false, StdinOnce: false, Tty: false,
        name: name,
        HostConfig: hostConfig
    });
}

const start_docker_node_01 = async function (nbNodes, tests_collection) {
    console.log("Creating container 01");
    // Create the container.
    container01 = await createMinimaContainer(node1_args, node_prefix + "01", hostCfg);
    containers[node_prefix + "01"] = container01; 
    // Start the container.
    await containers[node_prefix + "01"].start();
    containers[node_prefix + "01"].inspect(function (err, data) {
        ip_addrs[node_prefix+ "01"] = data.NetworkSettings.Networks[docker_net].IPAddress;
        start_other_nodes(nbNodes, tests_collection);
    });
}

start_other_nodes = async function (nbNodes, tests_collection) {
    console.log("Creating other containers");
    // Create the container.
    nodes_args = ["-connect", ip_addrs[node_prefix+ "01"], "9001"];
    console.log("node args:" + nodes_args);
    for (let i = 2; i < nbNodes+1; i++) {
        console.log("Creating node " + i);
        containers[node_prefix + i] = await createMinimaContainer(nodes_args, node_prefix + i, hostCfg);
        // Start the container.
        await containers[node_prefix + i].start();
        containers[node_prefix + i].inspect(function (err, data) {
            console.log("Node " + i + " IP:  " + JSON.stringify(data.NetworkSettings.Networks[docker_net].IPAddress));
            ip_addrs[node_prefix + i] = data.NetworkSettings.Networks[docker_net].IPAddress;
            if(i == nbNodes) { // run tests after we created last node
                // need to sleep to let node sync with others
                setTimeout(function () { tests_collection(ip_addrs) }, 2000);
            }
        });
    }
}

// this function calls HTTP GET on host:endpoint, expects a minima answer, and runs tests_to_run if server success.
run_some_tests_get = async function(host, endpoint, tests_to_run) {
    const url =  "http://" + host + ":" + host_port + endpoint;
    axios.get(url, {timeout: HTTP_TIMEOUT}, {maxContentLength: 3000},  {responseType: 'plain'})
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

// TODO / DO NOT USE
// this function calls HTTP POST on host:endpoint, expects a minima answer, and runs tests_to_run if server success.
run_some_tests_post = async function(host, endpoint, tests_to_run) {
    const url =  "http://" + host + ":" + host_port + endpoint;
    axios.post(url, {timeout: HTTP_TIMEOUT}, {maxContentLength: 3000},  {responseType: 'plain'})
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
                    if(containerInfo.Names[0].startsWith("/" + node_prefix)) {
                        console.log ("Found a dangling minima node(" + containerInfo.Names[0] + "), stopping.");
                        docker.getContainer(containerInfo.Id).stop();
                    }
                });
            } else {
                console.log("Found no running docker instances\n");
            }
      });
}

// setup a network of nbNodes minima nodes in star topology and runs tests_collection on it with argument ip_addrs[node_prefix+ "01"] .
start_static_network_tests = async function (nbNodes, tests_collection) {
    await stop_docker_nodes();
    // give 5 seconds to stop all docker nodes (should depend on nbNodes but also system performance)
    setTimeout(function() { start_docker_node_01(nbNodes, tests_collection); }, 5000);    
}

// number of nodes, list of tests
start_static_network_tests(3, function (ip_addrs) {
        console.log("tests collection");
        // run RPC call - needs port mapping
        // curl -s 127.0.0.1:9002/status | jq '.response.connections'

        run_some_tests_get(ip_addrs[node_prefix+ "01"], '/status', function (response) {
            response.connections.should.be.above(0);
            response.chainlength.should.be.above(1);
        });

        // send funds with no money

        run_some_tests_get(ip_addrs[node_prefix+ "01"], '/gimme50', function (response) {
            //        response.connections.should.be.above(0); 
            //        response.chainlength.should.be.above(1);
            console.log("gimme50 response: " + JSON.stringify(response));
        });
        // send funds with money
    }
);
