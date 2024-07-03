/**
 * MDS JS lib for MiniDAPPs..
 *
 * @spartacusrex
 */

/**
 * The MAIN Minima Callback function
 */
var MDS_MAIN_CALLBACK = null;

/**
 * Main MINIMA Object for all interaction
 */
var MDS = {
  //Main File host
  filehost: "",

  //RPC Host for Minima
  mainhost: "",

  //The MiniDAPP UID
  minidappuid: "",

  //Is logging RPC enabled
  logging: false,

  //When debuggin you can hard set the Host and port
  DEBUG_HOST: null,
  DEBUG_PORT: -1,

  //An allowed TEST Minidapp ID for SQL - can be overridden
  DEBUG_MINIDAPPID: "0x00",

  /**
   * Minima Startup - with the callback function used for all Minima messages
   */
  init: function (callback) {
    //Log a little..
    MDS.log("Initialising MDS..");

    //Is logging enabled.. via the URL
    if (MDS.form.getParams("MDS_LOGGING") != null) {
      MDS.logging = true;
    }

    //Get the host and port..
    var host = window.location.hostname;
    var port = Math.floor(window.location.port);

    //Get ther MiniDAPP UID
    MDS.minidappuid = MDS.form.getParams("uid");

    //HARD SET if debug mode - running from a file
    if (MDS.DEBUG_HOST != null) {
      MDS.log("DEBUG Settings Found..");

      host = MDS.DEBUG_HOST;
      port = MDS.DEBUG_PORT;
    }

    // env overrides
    if (window.DEBUG) {
      host = window.DEBUG_HOST;
      port = Math.floor(window.DEBUG_PORT);
      MDS.minidappuid = window.DEBUG_UID;
    }

    if (MDS.minidappuid == null) {
      MDS.minidappuid = MDS.DEBUG_MINIDAPPID;
    }

    //Is one specified..
    if (MDS.minidappuid == "0x00") {
      MDS.log("No MiniDAPP UID specified.. using test value");
    }

    MDS.filehost = "https://" + host + ":" + port + "/";
    MDS.mainhost = "https://" + host + ":" + port + "/mdscommand_/";
    MDS.log("MDS HOST  : " + MDS.filehost);

    //Store this for poll messages
    MDS_MAIN_CALLBACK = callback;

    //Start the Long Poll listener
    PollListener();

    //And Post a message
    MDSPostMessage({ event: "inited" });
  },

  /**
   * Log some data with a timestamp in a consistent manner to the console
   */
  log: function (output) {
    console.log("Minima @ " + new Date().toLocaleString() + " : " + output);
  },

  /**
   * Notify the User - on Phone it pops up in status bar. On desktop appears in Logs
   */
  notify: function (output) {
    //Send via POST
    httpPostAsync("notify", output);
  },

  /**
   * Cancel this MiniDAPPs notification
   */
  notifycancel: function () {
    //Send via POST
    httpPostAsync("notifycancel", "*");
  },

  /**
   * Runs a function on the Minima Command Line - same format as MInima
   */
  cmd: function (command, callback) {
    //Send via POST
    httpPostAsync("cmd", command, callback);
  },

  /**
   * Runs a SQL command on this MiniDAPPs SQL Database
   */
  sql: function (command, callback) {
    //Send via POST
    httpPostAsync("sql", command, callback);
  },

  /**
   * Get a link to a different Dapp. READ dapps can only get READ DAPPS. WRITE can get all dapps.
   */
  dapplink: function (dappname, callback) {
    //Send via POST
    httpPostAsync("dapplink", dappname, function (result) {
      var linkdata = {};
      linkdata.status = result.status;

      //Create the link..
      if (result.status) {
        linkdata.uid = result.response.uid;
        linkdata.sessionid = result.response.sessionid;
        linkdata.base =
          MDS.filehost +
          linkdata.uid +
          "/index.html?uid=" +
          result.response.sessionid;
      } else {
        //Not found..
        linkdata.error = result.error;
      }

      callback(linkdata);
    });
  },

  /**
   * Network Commands
   */
  net: {
    /**
     * Make a GET request
     */
    GET: function (url, callback) {
      //Send via POST
      httpPostAsync("net", url, callback);
    },

    /**
     * Make a POST request
     */
    POST: function (url, data, callback) {
      //Create the sinlg eline version..
      var postline = url + "&" + data;

      //Send via POST
      httpPostAsync("netpost", postline, callback);
    },
  },

  /**
   *  Simple GET and SET key value pairs that are saved persistently
   */
  keypair: {
    /**
     * GET a value
     */
    get: function (key, callback) {
      //Create the single line
      var commsline = "get&" + key;

      //Send via POST
      httpPostAsync("keypair", commsline, callback);
    },

    /**
     * SET a value
     */
    set: function (key, value, callback) {
      //Create the single line
      var commsline = "set&" + key + "&" + value;

      //Send via POST
      httpPostAsync("keypair", commsline, callback);
    },
  },

  /**
   * COMMS - send a message to ALL minidapps or JUST your own service.js
   */
  comms: {
    /**
     * PUBLIC message broadcast to ALL (callback is optional)
     */
    broadcast: function (msg, callback) {
      //Create the single line
      var commsline = "public&" + msg;

      //Send via POST
      httpPostAsync("comms", commsline, callback);
    },

    /**
     * PRIVATE message send just to this MiniDAPP (callback is optional)
     */
    solo: function (msg, callback) {
      //Create the single line
      var commsline = "private&" + msg;

      //Send via POST
      httpPostAsync("comms", commsline, callback);
    },
  },

  /**
   * File access
   */
  file: {
    /**
     * List file in a folder .. start at /
     */
    list: function (folder, callback) {
      //Create the single line
      var commsline = "list&" + folder;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Save text - can be text, a JSON in string format or hex encoded data
     */
    save: function (filename, text, callback) {
      //Create the single line
      var commsline = "save&" + filename + "&" + text;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Save Binary Data - supply as a HEX string
     */
    savebinary: function (filename, hexdata, callback) {
      //Create the single line
      var commsline = "savebinary&" + filename + "&" + hexdata;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Load text - can be text, a JSON in string format or hex encoded data
     */
    load: function (filename, callback) {
      //Create the single line
      var commsline = "load&" + filename;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Load Binary data - returns the HEX data
     */
    loadbinary: function (filename, callback) {
      //Create the single line
      var commsline = "loadbinary&" + filename;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Delete a file
     */
    delete: function (filename, callback) {
      //Create the single line
      var commsline = "delete&" + filename;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Get the full path - if you want to run a command on the file / import a txn / unsigned txn etc
     */
    getpath: function (filename, callback) {
      //Create the single line
      var commsline = "getpath&" + filename;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Make a directory
     */
    makedir: function (filename, callback) {
      //Create the single line
      var commsline = "makedir&" + filename;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Copy a file
     */
    copy: function (filename, newfilename, callback) {
      //Create the single line
      var commsline = "copy&" + filename + "&" + newfilename;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Move a file
     */
    move: function (filename, newfilename, callback) {
      //Create the single line
      var commsline = "move&" + filename + "&" + newfilename;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Download a File from the InterWeb - Will be put in Downloads folder
     */
    download: function (url, callback) {
      //Create the single line
      var commsline = "download&" + url;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Upload a file in chunks to the /fileupload folder
     */
    upload: function (file, callback) {
      //Start the file recursion..
      _recurseUploadMDS(file, 0, callback);
    },

    /**
     * Copy a file to your web folder
     */
    copytoweb: function (file, webfile, callback) {
      //Create the single line
      var commsline = "copytoweb&" + file + "&" + webfile;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },

    /**
     * Delete a file or folder from web folder
     */
    deletefromweb: function (file, callback) {
      //Create the single line
      var commsline = "deletefromweb&" + file;

      //Send via POST
      httpPostAsync("file", commsline, callback);
    },
  },

  /**
   * Function for GET parameters..
   */
  form: {
    //Return the GET parameter by scraping the location..
    getParams: function (parameterName) {
      var result = null,
        tmp = [];
      var items = window.location.search.substr(1).split("&");
      for (var index = 0; index < items.length; index++) {
        tmp = items[index].split("=");
        //console.log("TMP:"+tmp);
        if (tmp[0] === parameterName) result = decodeURIComponent(tmp[1]);
      }
      return result;
    },
  },

  /**
   * UTILITY functions.. very useful
   */
  util: {
    //Convert HEX to Base 64 - removes the 0x if necessary
    hexToBase64(hexstring) {
      //Check if starts with 0x
      var thex = hexstring;
      if (hexstring.startsWith("0x")) {
        thex = hexstring.substring(2);
      }

      return btoa(
        thex
          .match(/\w{2}/g)
          .map(function (a) {
            return String.fromCharCode(parseInt(a, 16));
          })
          .join("")
      );
    },

    //Convert Base64 to HEX
    base64ToHex(str) {
      const raw = atob(str);
      let result = "";
      for (let i = 0; i < raw.length; i++) {
        const hex = raw.charCodeAt(i).toString(16);
        result += hex.length === 2 ? hex : "0" + hex;
      }
      return result.toUpperCase();
    },

    //Convert Base64 to a Uint8Array - useful for Blobs
    base64ToArrayBuffer(base64) {
      var binary_string = window.atob(base64);
      var len = binary_string.length;
      var bytes = new Uint8Array(len);
      for (var i = 0; i < len; i++) {
        bytes[i] = binary_string.charCodeAt(i);
      }
      return bytes.buffer;
    },

    //Return a state variable given the coin
    getStateVariable(coin, port) {
      //Get the state vars
      var statvars = coin.state;
      var len = statvars.length;
      for (var i = 0; i < len; i++) {
        var state = statvars[i];
        if (state.port == port) {
          return state.data;
        }
      }

      return undefined;
    },
  },
};

/**
 * Post a message to the Minima Event Listeners
 */
function MDSPostMessage(json) {
  //And dispatch
  if (MDS_MAIN_CALLBACK) {
    MDS_MAIN_CALLBACK(json);
  }
}

var PollCounter = 0;
var PollSeries = 0;
function PollListener() {
  //The POLL host
  var pollhost = MDS.mainhost + "poll?" + "uid=" + MDS.minidappuid;
  var polldata = "series=" + PollSeries + "&counter=" + PollCounter;

  httpPostAsyncPoll(pollhost, polldata, function (msg) {
    //Are we on the right Series..
    if (PollSeries != msg.series) {
      //Reset to the right series..
      PollSeries = msg.series;
      PollCounter = msg.counter;
    } else {
      //Is there a message ?
      if (msg.status == true) {
        //Get the current counter..
        PollCounter = msg.response.counter + 1;

        //And Post the message..
        MDSPostMessage(msg.response.message);
      }
    }

    //And around we go again..
    PollListener();
  });
}

function postMDSFail(command, params, status) {
  //Some error..
  if (MDS.logging) {
    MDS.log("** An error occurred during an MDS command!");
  }

  //Create the message
  var errormsg = {};
  errormsg.event = "MDSFAIL";
  errormsg.data = {};
  errormsg.data.command = command;
  errormsg.data.params = params;
  errormsg.data.error = status;

  //Post it to the stack
  MDSPostMessage(errormsg);
}

/**
 * Utility function for GET request
 *
 * @param theUrl
 * @param callback
 * @param params
 * @returns
 */
function httpPostAsync(theUrl, params, callback) {
  //Add the MiniDAPP UID..
  var finalurl = MDS.mainhost + theUrl + "?uid=" + MDS.minidappuid;

  //Do we log it..
  if (MDS.logging) {
    MDS.log("POST_RPC:" + finalurl + " PARAMS:" + params);
  }

  var xmlHttp = new XMLHttpRequest();
  xmlHttp.onreadystatechange = function () {
    var status = xmlHttp.status;
    if (xmlHttp.readyState == XMLHttpRequest.DONE) {
      if (status === 0 || (status >= 200 && status < 400)) {
        //Do we log it..
        if (MDS.logging) {
          MDS.log("RESPONSE:" + xmlHttp.responseText);
        }

        //Send it to the callback function..
        if (callback) {
          callback(JSON.parse(xmlHttp.responseText));
        }
      } else {
        //Some error..
        postMDSFail(finalurl, params, xmlHttp.status);
      }
    }
  };
  xmlHttp.open("POST", finalurl, true); // true for asynchronous
  xmlHttp.overrideMimeType("text/plain; charset=UTF-8");
  xmlHttp.send(encodeURIComponent(params));
  //xmlHttp.onerror = function () {
  //  console.log("** An error occurred during the transaction");
  //};
}

/**
 * POLLING Call
 */
function httpPostAsyncPoll(theUrl, params, callback) {
  //Do we log it..
  if (MDS.logging) {
    MDS.log("POST_POLL_RPC:" + theUrl + " PARAMS:" + params);
  }

  var xmlHttp = new XMLHttpRequest();
  xmlHttp.onreadystatechange = function () {
    var status = xmlHttp.status;
    if (xmlHttp.readyState == XMLHttpRequest.DONE) {
      if (status === 0 || (status >= 200 && status < 400)) {
        //Do we log it..
        if (MDS.logging) {
          MDS.log("RESPONSE:" + xmlHttp.responseText);
        }

        //Send it to the callback function..
        if (callback) {
          callback(JSON.parse(xmlHttp.responseText));
        }
      } else {
        //Some error..
        postMDSFail(theUrl, params, xmlHttp.status);
      }
    }
  };
  xmlHttp.addEventListener("error", function (ev) {
    MDS.log("Error Polling - reconnect in 10s");
    setTimeout(function () {
      PollListener();
    }, 10000);
  });
  xmlHttp.open("POST", theUrl, true); // true for asynchronous
  xmlHttp.overrideMimeType("text/plain; charset=UTF-8");
  xmlHttp.send(encodeURIComponent(params));
}

/**
 * Internal recursive function for file upload
 */
function _recurseUploadMDS(thefullfile, chunk, callback) {
  //Get some details
  var filename = thefullfile.name;
  var filesize = thefullfile.size;

  //1MB MAX Chunk size..
  var chunk_size = 1024 * 1024;
  var allchunks = Math.ceil(filesize / chunk_size);

  //Have we finished..
  if (chunk > allchunks - 1) {
    return;
  }

  var startbyte = chunk_size * chunk;
  var endbyte = startbyte + chunk_size;
  if (endbyte > filesize) {
    endbyte = filesize;
  }

  //Get a piece of the file
  var filepiece = thefullfile.slice(startbyte, endbyte);

  //Create a form..
  var formdata = new FormData();
  formdata.append("uid", MDS.minidappuid);

  //Filedata handled a little differently
  formdata.append("filename", filename);
  formdata.append("filesize", filesize);
  formdata.append("allchunks", allchunks);
  formdata.append("chunknum", chunk);
  formdata.append("fileupload", filepiece);

  var request = new XMLHttpRequest();
  request.open("POST", "/fileuploadchunk.html");
  request.onreadystatechange = function () {
    var status = request.status;
    if (request.readyState == XMLHttpRequest.DONE) {
      if (status === 0 || (status >= 200 && status < 400)) {
        //Send it to the callback function..
        if (callback) {
          var resp = {};
          resp.status = true;
          resp.filename = filename;
          resp.size = filesize;
          resp.allchunks = allchunks;
          resp.chunk = chunk + 1;
          resp.start = startbyte;
          resp.end = endbyte;

          callback(resp);
        }

        //And now continue uploading..
        if (callback) {
          _recurseUploadMDS(thefullfile, chunk + 1, callback);
        } else {
          _recurseUploadMDS(thefullfile, chunk + 1);
        }
      } else {
        if (callback) {
          var resp = {};
          resp.status = false;
          resp.error = request.responseText;
          resp.filename = filename;
          resp.size = filesize;
          resp.allchunks = allchunks;
          resp.chunk = chunk;
          resp.start = startbyte;
          resp.end = endbyte;

          callback(resp);
        }

        //Some error..
        MDS.log("MDS FILEUPLOAD CHUNK ERROR: " + request.responseText);
      }
    }
  };

  //And finally send the POST request
  request.send(formdata);
}
