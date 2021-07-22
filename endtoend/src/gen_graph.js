const { CanvasRenderService } = require("chartjs-node-canvas");
// const chartjs = require('chart.js');
const fs = require("fs");
var Excel = require("exceljs");
var workbook = new Excel.Workbook();

var configuration = {
  type: "line",
  data: {
    labels: [1, 2, 3, 4, 5],
    datasets: [
      {
        label: "Part1",
        fill: false,
        data: [2478, 5267, 734, 784, 433],
        backgroundColor: "rgba(255, 99, 132, 1)",
        borderColor: "rgba(255,99,132,1)",
      },
      {
        label: "Part2",
        fill: false,
        data: [1233, 3467, 7634, 384, 4533],
        backgroundColor: "rgba(162, 99, 132, 1)",
        borderColor: "rgba(162,99,132,1)",
      },
    ],
  },
  options: {
    title: {
      display: true,
      text: 'Chart for p2p Test',
      fontColor: "#07BDA7"
    },
    scales: {
      xAxes: [{
        scaleLabel: {
          display: true,
          labelString: "number of Minima nodes",
          fontColor: "#028B7B"
        },
        ticks: {
            beginAtZero: true
        }
      }],
      yAxes: [{
        scaleLabel: {
          display: true,
          labelString: "number of p2p neighbours",
          fontColor: "#028B7B"
        },
        ticks: {
            beginAtZero: true
        }
      }]
    }
  }
};

const mkChart = async (params) => {
  const canvasRenderService = new CanvasRenderService(400, 400);
  return await canvasRenderService.renderToBuffer(configuration);
};

const test_graph_gen = async () => {
  await readFiles("./results/");
  var image = await mkChart("test");

  fs.writeFile("./results/graph.png", image, "base64", function (err) {
    console.log(err);
  });
};

const readFiles = (dirname) => {
  let label1 = [],
    label2 = [],
    set1 = [],
    set2 = [];
  return new Promise((resolve, reject) => {
    fs.readdir(dirname, async function (err, filenames) {
      if (err) {
        console.log(err);
        return;
      }
      for (let key in filenames) {
        var nodes = [],
          data = [],
          sign = false;
        var filename = filenames[key];
        var arr = filename.split(".");
        if(arr[arr.length-1] != "csv") continue;
        var worksheet = await workbook.csv.readFile(dirname + filename);
        var partName = filename.split("-")[3].split(".")[0];

        var column1 = worksheet.getColumn(1);
        column1.eachCell(function (cell, rowNumber) {
          if (cell.value != null) {
            var temp = cell.value;
            nodes.push(temp);
          }
        });
        nodes = nodes.slice(1);
        let nodeCnt = nodes.length / 2;
        if (partName === "part1") {
          if (label1.indexOf(nodeCnt) > -1) {
            sign = true;
          } else {
            label1.push(nodeCnt);
          }
        } else if (partName === "part2") {
          if (label2.indexOf(nodeCnt) > -1) sign = true;
          else label2.push(nodeCnt);
        }

        var column2 = worksheet.getColumn(4);
        column2.eachCell(function (cell, rowNumber) {
          if (cell.value != null) {
            var temp = cell.value;
            data.push(temp);
          }
        });
        data = data.slice(1);
        const reg = /\"p2pPeercount\"\:([0-9]+)\}/;
        const count = parseInt(data[0].match(reg)[1]);

        if (!sign) {
          if (partName === "part1") set1.push(count);
          else if (partName === "part2") set2.push(count);
        }
      }

      //sorting values of x Axe
      let temp = [];
      for (let key in label1) {
        let pair = {};
        pair.x = label1[key];
        pair.y = set1[key];
        temp.push(pair);
      }
      temp.sort((a,b) => {
        return a.x - b.x
      });
      label1 = temp.map(item => item.x);
      set1 = temp.map(item => item.y);

      temp = [];
      for (let key in label2) {
        let pair = {};
        pair.x = label2[key];
        pair.y = set2[key];
        temp.push(pair);
      }
      temp.sort((a,b) => {
        return a.x - b.x
      });
      label2 = temp.map(item => item.x);
      set2 = temp.map(item => item.y);
      console.log("label1: ", label1);
      console.log("label2: ", label2);
      console.log("set1: ", set1);
      console.log("set2: ", set2);
      configuration.data.labels = label1;
      configuration.data.datasets[0].data = set1;
      configuration.data.datasets[1].data = set2;
      resolve();
    });
  });
};

module.exports = test_graph_gen

// test_graph_gen()