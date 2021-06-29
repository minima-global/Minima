const axios = require("axios");

const cfg = {
  HTTP_TIMEOUT: 30000
}

var Minima_API = {

  rpchost : "http://127.0.0.1:9002",

  init: function(host) {
    Minima_API.rpchost = `http://${host}:9002`
  },

  help: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/help", params)
  },

  tutorial: function() {
    return get_minima_endpoint(Minima_API.rpchost, "/tutorial", "")
  },

  status: function() {
      return get_minima_endpoint(Minima_API.rpchost, "/status", "")
  },

  topblock: function() {
    return get_minima_endpoint(Minima_API.rpchost, "/topblock", "")
  },

  history: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/history", params)
  },

  backup: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/backup", params)
  },

  flushmempool: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/flushmempool", params)
  },

  check: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/check", params)
  },

  printdb: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/printdb", params)
  },

  printtree: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/printtree", params)
  },

  automine: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/automine", params)
  },

  trace: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/trace", params)
  },

  network: function() {
    return get_minima_endpoint(Minima_API.rpchost, "/network", "")
  },

  connect: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/connect", params)
  },

  disconnect: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/disconnect", params)
  },

  reconnect: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/reconnect", params)
  },

  weblink: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/weblink", params)
  },

  gimme50: function() {
      return get_minima_endpoint(Minima_API.rpchost, "/gimme50", "")
  },

  send: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/send", params)
  },

  sendpoll: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/sendpoll", params)
  },

  newaddress: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/newaddress", params)
  },

  balance: function(params="") {
      return get_minima_endpoint(Minima_API.rpchost, "/balance", params)
  },

  keys: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/keys", params)
  },

  exportkey: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/exportkey", params)
  },

  importkey: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/importkey", params)
  },

  coins: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/coins", params)
  },

  coinsimple: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/coinsimple", params)
  },

  keepcoin: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/keepcoin", params)
  },

  txpowsearch: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txpowsearch", params)
  },

  txpowinfo: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txpowinfo", params)
  },

  scripts: function() {
    return get_minima_endpoint(Minima_API.rpchost, "/scripts", "")
  },

  newscript: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/newscript", params)
  },

  extrascript: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/extrascript", params)
  },

  cleanscript: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/cleanscript", params)
  },

  runscript: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/runscript", params)
  },

  tokens: function() {
    return get_minima_endpoint(Minima_API.rpchost, "/tokens", "")
  },

  tokencreate: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/tokencreate", params)
  },

  tokenvalidate: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/tokenvalidate", params)
  },

  sign: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/sign", params)
  },

  verify: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/verify", params)
  },

  chainsha: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/chainsha", params)
  },

  random: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/random", params)
  },

  hash: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/hash", params)
  },

  maxima: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/maxima", params)
  },

  sshtunnel: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/sshtunnel", params)
  },

  minidapps: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/minidapps", params)
  },

  txnlist: function (params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/txnlist", params)
  },

  txncreate: function(params="") {
    return get_minima_endpoint(Minima_API.rpchost, "/txncreate", params)
  },

  txndelete: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txndelete", params)
  },

  txnexport: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnexport", params)
  },

  txnimport: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnimport", params)
  },

  txninput: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txninput", params)
  },

  txnoutput: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnoutput", params)
  },

  txnreminput: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnreminput", params)
  },

  txnremoutput: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnremoutput", params)
  },

  txnstate: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnstate", params)
  },

  txnscript: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnscript", params)
  },

  txnsign: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnsign", params)
  },

  txnauto: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnauto", params)
  },

  txnsignauto: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnsignauto", params)
  },

  txnvalidate: function(params) {
    return get_minima_endpoint(Minima_API.rpchost, "/txnvalidate", params)
  },

  quit: function() {
    get_minima_endpoint(Minima_API.rpchost, "/quit", "")
  }
}

const get_minima_endpoint = async (host, endpoint, params="") => {
    const url =  host + endpoint + "+" + params.replace(/;/g, "+");
    try{
      const response = await axios.get(url, {timeout: cfg.HTTP_TIMEOUT}, {maxContentLength: 3000},  {responseType: 'plain'})
      
      // handle success
      if(response && response.status == 200) {
          console.log(params, response.data);
          if(response.data.status == true) {
            return response.data.response;
          }
      }
    } catch { error => {
        // handle error
        console.log(error);
      }
    }
}

exports.Minima_API = Minima_API

