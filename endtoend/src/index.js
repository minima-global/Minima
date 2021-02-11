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
const options1 = {
    Name: "minima-node-01",
    HostConfig: {
        AutoRemove: true,
        NetworkMode: docker_net
    }
};

const options2 = {
    name: "minima-node-02",
    HostConfig: {
        AutoRemove: true,
        NetworkMode: docker_net,
        PortBindings: {
            "9002/tcp": [
                {
                "HostPort": "9002"
                }
            ]
        }
    }
};

const host_port = 9002;

var nodes_args; // all other nodes get same args

var container01;
var container02;

var ip1,ip2,ip3;


const start_docker_node_01 = async function (nbNodes, tests_collection) {
    console.log("Creating container 01");
    // Create the container.
    container01 = await docker.createContainer({
        AttachStderr: false,
        AttachStdin: false,
        AttachStdout: false,
        Cmd: node1_args,
        Image: image,
        OpenStdin: false,
        StdinOnce: false,
        Tty: false,
        name: "minima-node-01",
        HostConfig: options1.HostConfig
    });

    console.log("Starting container 01");
    // Start the container.
    await container01.start();
    container01.inspect(function (err, data) {
        const IPnode01 = data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress;
        console.log("IPnode01: " + IPnode01);
        start_other_nodes(IPnode01, nbNodes, tests_collection);
    });
}

start_other_nodes = async function(IPnode01, nbNodes, tests_collection) {
    console.log("Creating container 02");
    // Create the container.
    nodes_args = ["-connect", IPnode01, "9001"];
    container02 = await docker.createContainer({
      AttachStderr: false,
      AttachStdin: false,
      AttachStdout: false,
      Cmd: nodes_args,
      Image: image,
      OpenStdin: false,
      StdinOnce: false,
      Tty: false,
      name: options2.name,
      HostConfig: options2.HostConfig
    });
  
    console.log("Starting container 02");
    // Start the container.
    await container02.start(); 
    container02.inspect(function (err, data) {
            console.log("IP:  " + JSON.stringify(data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress));
            const IPnode02 = data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress;
            console.log("IPnode01: " + IPnode01);
            console.log("IPnode02: " + IPnode02);
            // need to sleep
            setTimeout(function () { tests_collection(IPnode01)}, 3000);
          });        
}

// this function calls HTTP GET on host:endpoint, expects a minima answer, and runs tests_to_run if server success.
run_some_tests_get = async function(host, endpoint, tests_to_run) {
    const url =  "http://" + host + ":" + host_port + endpoint;
    axios.get(url, {timeout: 5000}, {maxContentLength: 3000},  {responseType: 'plain'})
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
    axios.post(url, {timeout: 5000}, {maxContentLength: 3000},  {responseType: 'plain'})
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

start_docker_net = async function(nbNodes, tests_collection) {
    await start_docker_node_01(nbNodes, tests_collection);
}

stop_docker_nodes = async function() {
    console.log("stop_docker_nodes");
    // iterate over all running containers and stop them if their name starts with minima-node-
    docker.listContainers({ all:false },
      function (err, containers) {
            if(containers) { 
                containers.forEach(function (containerInfo) {
                    //console.log("Found running container " + containerInfo.Id);
                    //console.log( "   names: " + JSON.stringify(containerInfo.Names));
                    if(containerInfo.Names[0].startsWith("/minima-node-")) {
                        console.log ("Found a dangling minima node(" + containerInfo.Names[0] + "), its time to say goodbye.");
                        docker.getContainer(containerInfo.Id).stop();
                    }
                });
            } else {
                console.log("Found no running docker instances\n");
            }
      });
}

start_static_network_tests = async function (nbNodes, tests_collection) {
    stop_docker_nodes();
    // give 5 seconds to stop all docker nodes
    setTimeout(function() { start_docker_net(nbNodes, tests_collection); }, 5000);    
}

start_static_network_tests(2,
    function (IPnode01) {
        console.log("tests collection");
        // run RPC call - needs port mapping
        // curl -s 127.0.0.1:9002/status | jq '.response.connections'

        run_some_tests_get(IPnode01, '/status', function (response) {
            response.connections.should.be.above(0);
            response.chainlength.should.be.above(1);
        });

        // send funds with no money

        run_some_tests_get(IPnode01, '/gimme50', function (response) {
            //        response.connections.should.be.above(0); 
            //        response.chainlength.should.be.above(1);
            console.log("gimme50 response: " + JSON.stringify(response));
        });
        // send funds with money
    }
);

