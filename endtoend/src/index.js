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

// config
const image = 'minima:latest';  // docker image name to run
const docker_net = "minima-e2e-testnet"; // docker private network name -> MUST BE CREATED MANUALLY
const node1_args = ["-private", "-clean"];
const options1 = {
    name: "minima-node-01",
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

// start node 1 with docker run

// docker.run(image, node1_args, process.stdout, options1, function (err, data, container) {
//     console.log("***** Finished node-01 from dockerode *****");
//     console.log(data);
// }).on('container', function (container) {
//     console.log("***** Started node-01 from dockerode *****");
//     console.log("***** What is my IP?");
//     container.inspect(function (err, data) {
// //        console.log("data: " + JSON.stringify(data));
//         console.log("IP:  " + JSON.stringify(data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress));
// //        data[0].NetworkSettings.Networks["minima-e2e-testnet"].IPAddress
//       });
      
// //    container.defaultOptions.start.Binds = ["/tmp:/tmp:rw"];
//   });

// start node 1 with docker createContainer and start

var container01;
var container02;

const start_docker_node_01 = async function () {
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
    Name: options1.name,
    HostConfig: options1.HostConfig
  });

  console.log("Starting container 01");
  // Start the container.
  await container01.start(); 
  console.log("***** Started node-01 from dockerode *****");
  console.log("What is my config?");
  container01.inspect(function (err, data) {
        console.log("data.NetworkSettings: " + JSON.stringify(data.NetworkSettings));
  });
  console.log("***** What is my IP?");
  container01.inspect(function (err, data) {
// //        console.log("data: " + JSON.stringify(data));
    console.log("IP:  " + JSON.stringify(data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress));
    IPnode01 = data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress;
    console.log("IPnode01" + IPnode01);
    start_other_nodes(IPnode01);
  });
}

start_other_nodes = async function(IPnode01) {
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
      Name: options2.name,
      HostConfig: options2.HostConfig
    });
  
    console.log("Starting container 02");
    // Start the container.
    await container02.start(); 
    container02.inspect(function (err, data) {
        // //        console.log("data: " + JSON.stringify(data));
            console.log("IP:  " + JSON.stringify(data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress));
            const IPnode02 = data.NetworkSettings.Networks["minima-e2e-testnet"].IPAddress;
            console.log("IPnode02: " + IPnode02);
            //start_other_nodes(IPnode01);
            // need to sleep
            setTimeout(function (IPnode01) { health_check(IPnode01)}, 3000);
          });        
}

health_check = async function() {
    // run RPC call - needs port mapping
    // curl -s 127.0.0.1:9002/status | jq '.response.connections'
    const url = "http://" + IPnode01 + ":" + host_port + "/status";
    console.log("Attempting GET " + url);

    axios.get(url, {timeout: 5000}, {maxContentLength: 3000},  {responseType: 'plain'})
    .then(function (response) {
      // handle success
      if(response && response.status == 200) {
          console.log(response.data);
          
          if(response.data.response.connections == 0) {
              console.log(" Not ready - no peer connected to master node.");
          } else {
              console.log(" Ready - " + response.data.response.connections + " nodes connnected.");
          }
          console.log("If connections=1 then success");
          response.data.response.connections.should.be.above(0);  // chai test
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

run_tests = async function() {

}

start_docker_net = async function() {
    await start_docker_node_01();
    await run_tests();
}

stop_docker_nodes = async function() {
    // list all active containers minima-node-xx
    // stop them
    // remove all stopped containers minima-node-xx
}

stop_docker_nodes();
start_docker_net();


