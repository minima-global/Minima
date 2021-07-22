const graph = process.env.graph;

if (graph === "true") {
    var test_graph_gen = require('./gen_graph.js');
    test_graph_gen();
} else if(graph === "false") {
    var test_star_static = require('./test_star_static.js');
    test_star_static();
}





