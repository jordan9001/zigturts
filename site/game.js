"use strict";

function Setup() {
  // get the canvas and resize it
  //TODO

  // setup handler for submitting
  //TODO

  // create the websocket
  let ws = new WebSocket("ws://"+ location.host +"/ws");
  ws.binaryType = 'arraybuffer';
  ws.onerror = (err_evt) => {
    console.log("Error", err_evt);
    alert("Connection Lost");
  }
  ws.onclose = (close_evt) => {
    console.log("Close", close_evt);
    alert("Connection Lost");
  }
  ws.onopen = (open_evt) => {
    console.log("Connected");

    // get the floor and get the existing program
    //TODO
  }
  ws.onmessage = (msg_evt) => {
    // parse msg
    let data = msg_evt.data;
    let dv = new DataView(data);
    // get message type
    let mt = dv.getUint8(0, true);
    console.log("Got message type " + mt);
  }
}

Setup();
