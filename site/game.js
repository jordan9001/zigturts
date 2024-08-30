"use strict";

// globals
var canvas = null;
var ctx = null;

document.addEventListener('DOMContentLoaded', () => {
  // get the canvas
  canvas = document.getElementById("canvas");
  ctx = canvas.getContext('2d');

  // setup handler for submitting
  let btn = document.getElementById("progbtn");
  let txt = document.getElementById("progbox");

  // create the websocket
  let ws = new WebSocket("wss://"+ location.host + "/ws/" + location.search.slice(1));
  ws.binaryType = 'arraybuffer';
  ws.onerror = (err_evt) => {
    console.log("WS Error", err_evt);
  }
  ws.onclose = (close_evt) => {
    console.log("WS Close", close_evt);
  }
  ws.onopen = (open_evt) => {
    console.log("Connected");

    // get the floor and get the existing program
    let prog_req = Uint8Array.from([2]);
    ws.send(prog_req);
    let floor_req = Uint8Array.from([1]);
    ws.send(floor_req);
  }
  ws.onmessage = (msg_evt) => {
    // parse msg
    let data = msg_evt.data;
    let dv = new DataView(data);
    // get message type
    let mt = dv.getUint8(0, true);
    console.log("Got message type " + mt);
    console.log("dbg: ", dv);
  }

  btn.addEventListener("click", (evt) => {
      // change value and disable button until it goes through
      //TODO
      let prog = txt.value;

      // filter for their benefit
      // lowercase everything and remove everything except a-z
      prog = prog.toLowerCase();
      prog = prog.replace(/[^a-z ]/g, '');
      txt.value = prog;

      prog = "\x03" + prog;

      const buf = Uint8Array.from(prog.split('').map(c => c.charCodeAt(0)));
      try {
        ws.send(buf);

      } catch (err) {
        // not ready to send
        //TODO
        console.log(err);
        console.log("please wait a sec...");
      }
  });
});
