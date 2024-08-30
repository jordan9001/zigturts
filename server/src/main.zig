//! The server for the turtles game
//! This server evaluates the 'programs' and updates the clients on turtle locations and color changes
//! We only handle websocket comms, caddy serves the static site

const std = @import("std");
const zap = @import("zap");

const WebSockets = zap.WebSockets;
const WebsocketHandler = WebSockets.Handler(UserContext);
const RndGen = std.rand.DefaultPrng;

const MAXPROG: u16 = 0x480;

// type aliases
const idint = u64;
const color_t = u4;

var gpa = std.heap.GeneralPurposeAllocator(.{
    .thread_safe = true,
}){};
const alloc = gpa.allocator();


/// A turtle has a program, and other state
const Turtle = struct {
    //TODO could use rwlock instead?
    mux: std.Thread.Mutex = .{},

    prog: [MAXPROG]u8 = .{0} ** MAXPROG,
    progsz: u16 = 0,

    cursor: u16 = 0,
    rot: f32 = 0.0,
    wait: u16 = 0,
    x: u16 = 0,
    y: u16 = 0,
    color: color_t = 0,
};

/// A User has a unique id, a turtle, and a websocket context
const UserContext = struct {
    /// unique id used to log in
    id: idint,

    turtle: Turtle,

    /// user's websocket handle is initialized and destroyed by zap threads, and written to by our loop
    /// so we need a mux
    /// this is not the same as the turtle's mux
    mux: std.Thread.Mutex = .{},
    wsh: ?WebSockets.WsHandle,
    wss: WebsocketHandler.WebSocketSettings,
};

const Floor = struct {
    //TODO have actual segmentation and lazy streaming?
    //TODO have compression fn
    //TODO have a mutext on individual segments or whole floor? evaluator is the only writer
    // evaluator is the only writing thread
    // and chunk updates are the only other reader
    h: u16,
    w: u16,
    img: []color_t,
    mux: std.Thread.Mutex = .{},
};

/// A GameContext has all the preallocated UserContexts and floor bitmaps
const GameContext = struct {
    floor: Floor,
    /// the users list is created at startup and does not change
    users: []UserContext,

    const Self = @This();

    fn init(usernum: u16, w: u16, h: u16) !Self {
        const users_s = try alloc.alloc(UserContext, usernum);

        if (w % 2 == 1) {
            @panic("Width must be multiple of two!");
        }

        const img_s = try alloc.alloc(color_t, w * h);
        @memset(img_s, 0);

        for (users_s) |*user| {
            //TODO actually read these in, don't remake them every time
            user.id = std.crypto.random.int(idint);
            std.debug.print("User: {}\n", .{user.id});
            {
                user.mux.lock();
                defer user.mux.unlock();

                user.wsh = null;
                user.turtle = Turtle{};
                user.wss = WebsocketHandler.WebSocketSettings{
                    .on_message = ws_on_msg,
                    .on_open = ws_on_open,
                    .on_close = ws_on_close,
                    .context = user,
                };
            }
        }

        return .{
            .users = users_s,
            .floor = Floor{
                .h = h,
                .w = w,
                .img = img_s,
            },
        };
    }
};

var g_gamectx: GameContext = undefined;

fn ws_on_open(userctx: ?*UserContext, handle: WebSockets.WsHandle) void {
    const user = userctx orelse return;

    // save the handle
    {
        user.mux.lock();
        defer user.mux.unlock();

        // what if there is an existing connection? Can we trash that one?
        if (user.wsh) |oldhandle| {
            WebsocketHandler.close(oldhandle);
            // will this call on_close? I don't want that
            // hmmm, try and see I guess?
            std.debug.print("Got a new connection with an existing connection already here... closed old one, see if new one sticks around?\n    old {?} new {?}", .{oldhandle, handle});
        }

        user.wsh = handle;
    }

    std.debug.print("Opened new wshandle for user {} : {?}\n", .{user.id, handle});
}

fn ws_on_close(userctx: ?*UserContext, uuid: isize) void {
    const user = userctx orelse return;
    _ = uuid;

    // players are persistent, so there shouldn't be anything to free
    // but we need to remove the invalid WebSockets.WsHandle
    {
        user.mux.lock();
        defer user.mux.unlock();

        if (user.wsh) |oldhandle| {
            std.debug.print("Closing wshandle for user {}: {?}\n", .{user.id, oldhandle});
        }

        user.wsh = null;
    }
}

const MsgType = enum(u8) {
    getfloor = 1,
    getprog = 2,
    pushprog = 3,
    pushfloor = 4,
    pushupd = 5,
    _,
};

fn send_floor(handle: WebSockets.WsHandle) void {
    const floor = &g_gamectx.floor;

    {
        floor.mux.lock();
        defer floor.mux.unlock();

        const floor_out: []u8 = alloc.alloc(u8, (floor.img.len / 2) + 2 + 2 + 1) catch unreachable;
        defer alloc.free(floor_out);

        floor_out[0] = @intFromEnum(MsgType.pushfloor);
        floor_out[1] = @intCast(floor.w & 0xff);
        floor_out[2] = @intCast((floor.w >> 8) & 0xff);
        floor_out[3] = @intCast(floor.h & 0xff);
        floor_out[4] = @intCast((floor.h >> 8) & 0xff);

        //@memcpy(floor_out[5..], floor.img);
        for (0..(floor.img.len / 2)) |bi| {
            // copy over two at a time
            floor_out[bi + 5] = floor.img[bi*2];
            floor_out[bi + 5] |= @as(u8, floor.img[(bi*2) + 1]) << 4;
        }

        WebsocketHandler.write(handle, floor_out, false) catch |err| {
            std.debug.print("Unable to write out floor: {}\n", .{err});    
        };
    }

}

fn send_prog(handle: WebSockets.WsHandle, turt: *Turtle) void {
    // lock the prog
    turt.mux.lock();
    defer turt.mux.unlock();

    // write out the existing program
    const prog_out: []u8 = alloc.alloc(u8, turt.progsz + 1) catch unreachable;
    defer alloc.free(prog_out);

    std.debug.print("progsz {} outsz {}?\n", .{turt.progsz, prog_out.len});

    prog_out[0] = @intFromEnum(MsgType.pushprog);
    if (prog_out.len > 1) {
        @memcpy(prog_out[1..prog_out.len], turt.prog[0..turt.progsz]);
    }

    WebsocketHandler.write(handle, prog_out, false) catch |err| {
        std.debug.print("Unable to write out existing program: {}\n", .{err});    
    };
}

fn ws_on_msg(
    userctx: ?*UserContext,
    handle: WebSockets.WsHandle,
    msg: []const u8,
    is_text: bool,
) void {
    _ = is_text;
    const user = userctx orelse return;

    // messages start with a message type identifier
    if (msg.len < 1) {
        std.debug.print("User {} gave empty message\n", .{user.id});
        return;
    }

    std.debug.print("User {} sent a msg {}\n", .{user.id, msg[0]});

    const msgtype: MsgType = @enumFromInt(msg[0]);
    switch (msgtype) {
        .getfloor => {
            send_floor(handle);
        },
        .getprog => {
            send_prog(handle, &user.turtle);
        },
        .pushprog => {
            // got a prog for this user's turtle
            std.debug.print("User {} send a program of len {}\n", .{user.id, msg.len});

            if (msg.len > MAXPROG) {
                return;
            }

            {
                user.turtle.mux.lock();
                defer user.turtle.mux.unlock();

                user.turtle.progsz = @intCast(msg.len);
                @memcpy(user.turtle.prog[0..msg.len], msg);
                user.turtle.cursor = 0;
                user.turtle.rot = 0.0;
                user.turtle.wait = 0;
                // don't reset position
            }
        },
        .pushfloor, .pushupd => return,
        _ => return,
    }

    //TODO what if we fill up the output buffer and get an error?
    // we would have to use on_ready
    // and just register the request here for our output worker to respond to
}

/// handle requests
/// shouldn't be used, caddy should serve our static files
/// just redirects to the main site
fn on_request(r: zap.Request) void {
    r.redirectTo("/", null) catch unreachable;
}

/// Upgrade to websocket
fn on_upgrade(r: zap.Request, target_protocol: []const u8) void {
    // make sure we're talking the right protocol
    if (!std.mem.eql(u8, target_protocol, "websocket")) {
        std.log.warn("received illegal protocol: {s}", .{target_protocol});
        r.setStatus(.bad_request);
        r.sendBody("Unsupported protocol") catch unreachable;
        return;
    }

    std.debug.print("Got upgrade for {?s}\n", .{r.path});

    if (r.path) |path| {
        const strid = path[("/ws/".len)..];
        const id: idint = std.fmt.parseInt(idint, strid, 10) catch {
            std.debug.print("Bad id {s}\n", .{strid});
            return;
        };

        // check key for user in db
        // users list and ids are static at this point, no need for a lock
        for (g_gamectx.users) |*u| {
            if (id == u.id) {
                std.debug.print("Found user!", .{});
                
                // do upgrade!
                WebsocketHandler.upgrade(r.h, &u.wss) catch |err| {
                    std.debug.print("Error in websocketUpgrade(): {any}", .{err});
                    return;
                };

                break;
            }
        } else {
            std.debug.print("Did not find matching user for {}\n", .{id});
            return;
        }

    } else {
        return;
    }

}


pub fn main() !void {
    std.debug.print("Starting.\n", .{});

    var do_help = false;
    var prog: []const u8 = "turtserver";
    var usernum: u16 = 0;
    var floorw: u16 = 0;
    var floorh: u16 = 0;

    var argi = std.process.args();
    var i: u16 = 0;
    while (argi.next()) |a| : (i+=1) {
        switch (i) {
            0 => {prog = a;},
            1 => {usernum = std.fmt.parseInt(u16, a, 0) catch 0;},
            2 => {floorw  = std.fmt.parseInt(u16, a, 0) catch 0;},
            3 => {floorh  = std.fmt.parseInt(u16, a, 0) catch 0;},
            else => {do_help = true; break;},
        }
    }

    if (usernum == 0 or floorw == 0 or floorh == 0) {
        do_help = true;
    }

    if (do_help) {
        std.debug.print("Usage: {s} <# of users> <floor width> <floor height>\n", .{prog});
        std.process.exit(255);
    }

    // build the gamestate, given a number of users and a size of floor

    g_gamectx = try GameContext.init(usernum, floorw, floorh);

    // setup listener
    var listener = zap.HttpListener.init(
        .{
            .port = 3000,
            .on_request = on_request,
            .on_upgrade = on_upgrade,
            .max_clients = 1000,
            .max_body_size = 1 * 1024,
            .public_folder = "examples/websockets/frontend",
            .log = true,
        },
    );
    try listener.listen();

    std.debug.print("Listening on 0.0.0.0:3000\n", .{});

    // start our own thread for evaluating the instructions
    // evaluator thread does:
    // - runs instructions
    // - writes out updates
    // - pauses if no clients are connected
    //TODO

    // start worker threads
    zap.start(.{
        .threads = 1,
        .workers = 1,
    });
}

