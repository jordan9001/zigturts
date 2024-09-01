"use strict";

// globals
var canvas = null;
var ctx = null;
var img = null;

function getcolor(v) {
  switch (v) {
    case 0:
      return [0x14, 0x15, 0x16];
    case 1:
      return [0x41, 0x10, 0x14];
    case 2:
      return [0x48, 0x2d, 0xb];
    case 3:
      return [0x43, 0x28, 0x66];
    case 4:
      return [0x61, 0x2e, 0x56];
    case 5:
      return [0x97, 0x4a, 0x28];
    case 6:
      return [0x57, 0x65, 0x92];
    case 7:
      return [0x9a, 0x70, 0x5b];
    case 8:
      return [0xcb, 0x41, 0x75];
    case 9:
      return [0x8e, 0x57, 0xb3];
    case 10:
      return [0xd0, 0x6b, 0x71];
    case 11:
      return [0x73, 0xd8, 0x64];
    case 12:
      return [0x88, 0xb2, 0xa2];
    case 13:
      return [0xd6, 0xa3, 0x68];
    case 14:
      return [0xe8, 0xeb, 0x8b];
    case 15:
      return [0xd5, 0xc7, 0xd1];
    default:
      console.log("Uh?");
      return [0xff, 0x00, 0x00];
  }
}

document.addEventListener('DOMContentLoaded', () => {
  // get the canvas
  canvas = document.getElementById("canvas");
  ctx = canvas.getContext('2d');
  let dpr = window.devicePixelRatio || 1;
  ctx.scale(dpr, dpr);

  // setup handler for submitting
  let btn = document.getElementById("progbtn");
  let txt = document.getElementById("progbox");

  let img = null;

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
    let mt = dv.getUint8(0);
    let c = 1;
    console.log("Got message type " + mt, dv);

    switch (mt) {
      case 3:
        // program
        let decoder = new TextDecoder();
        let prog = decoder.decode(dv.buffer.slice(c));
        txt.value = prog;
        break;
      case 4:
        // floor
        let w = dv.getUint16(c, true);
        c += 2;
        let h = dv.getUint16(c, true);
        c += 2;

        canvas.width = w;
        canvas.height = h;

        img = ctx.getImageData(0, 0, w, h);
        let pi = 0;
        while (c < dv.byteLength) {
          let b = dv.getUint8(c);

          let [r1, g1, b1] = getcolor(b & 0xf);
          let [r2, g2, b2] = getcolor((b>>4) & 0xf);

          img.data[(pi * 4) + 0] = r1;
          img.data[(pi * 4) + 1] = g1;
          img.data[(pi * 4) + 2] = b1;
          img.data[(pi * 4) + 3] = 0xff; // alpha

          pi += 1;

          img.data[(pi * 4) + 0] = r2;
          img.data[(pi * 4) + 1] = g2;
          img.data[(pi * 4) + 2] = b2;
          img.data[(pi * 4) + 3] = 0xff; // alpha

          pi += 1;
          c += 1;
        }
        ctx.putImageData(img, 0, 0);

        break;
      case 5:
        // update
        // TODO grab smaller piece of canvas?
        if (img == null) {
          break;
        }
        while (c < dv.byteLength) {
          let x = dv.getUint16(c, true);
          c += 2;
          let y = dv.getUint16(c, true);
          c += 2;
          let [r, g, b] = getcolor(dv.getUint8(c));
          c += 1;

          let idx = x + (y * canvas.width);
          img.data[(idx * 4) + 0] = r;
          img.data[(idx * 4) + 1] = g;
          img.data[(idx * 4) + 2] = b;
        }
        ctx.putImageData(img, 0, 0);
        break;
      default:
        console.log("Unknown message type");
    }
  }

  btn.addEventListener("click", (evt) => {
      // change value and disable button until it goes through
      //TODO
      let prog = txt.value;

      // filter for their benefit
      // lowercase everything and remove everything except a-z
      prog = prog.toLowerCase();
      prog = prog.replace(/[^a-z]/g, '');
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
