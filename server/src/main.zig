//! The server for the turtles game
//! This server evaluates the 'programs' and updates the clients on turtle locations and color changes
//! We only handle websocket comms, caddy serves the static site

const std = @import("std");
const zap = @import("zap");

const WebSockets = zap.WebSockets;
const WebsocketHandler = WebSockets.Handler(UserContext);
const RndGen = std.rand.DefaultPrng;

const MAXPROG: u16 = 0x480;
const MAXLOOP: u16 = 30;

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
    spd: f32 = 0.0,
    rotspd: f32 = 0.0,
    rot: f32 = 0.0,
    x: f32 = 0.0,
    y: f32 = 0.0,
    color: color_t = 0,
    thick: u8 = 1,
    wait: u16 = 0,
    loop_pos: [MAXLOOP]u16 = .{0} ** MAXLOOP,
    loop_count: [MAXLOOP]u8 = .{0} ** MAXLOOP,
    loop_head: u16 = 0,
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

    fn pos2idx(self: *Floor, x: i32, y: i32) usize {
        const pos = self.wrappos(x, y);
        return @intCast(pos[0] + (pos[1] * self.w));
    }

    fn wrappos(self: *Floor, ix: i32, iy: i32) [2]i32 {
        var x = ix;
        var y = iy;
        while (x < 0) {
            x += self.w;
        }

        while (x >= self.w) {
            x -= self.w;
        }

        while (y < 0) {
            y += self.h;
        }

        while (y >= self.h) {
            y -= self.h;
        }

        return [2]i32{ x, y };
    }
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
        @memset(img_s, 0xf);

        for (users_s) |*user| {
            //TODO actually read these in, don't remake them every time
            user.id = std.crypto.random.int(idint);
            std.debug.print("User: {}\n", .{user.id});
            {
                user.mux.lock();
                defer user.mux.unlock();

                user.wsh = null;
                user.turtle = Turtle{
                    .x = std.crypto.random.float(f32) * @as(f32, @floatFromInt(w)),
                    .y = std.crypto.random.float(f32) * @as(f32, @floatFromInt(h)),
                    .color = 0x0,
                };
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
            std.debug.print("Got a new connection with an existing connection already here... closed old one, see if new one sticks around?\n    old {?} new {?}", .{ oldhandle, handle });
        }

        user.wsh = handle;
    }

    std.debug.print("Opened new wshandle for user {} : {?}\n", .{ user.id, handle });
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
            std.debug.print("Closing wshandle for user {}: {?}\n", .{ user.id, oldhandle });
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
            floor_out[bi + 5] = floor.img[bi * 2];
            floor_out[bi + 5] |= @as(u8, floor.img[(bi * 2) + 1]) << 4;
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

    std.debug.print("progsz {} outsz {}?\n", .{ turt.progsz, prog_out.len });

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

    std.debug.print("User {} sent a msg {}\n", .{ user.id, msg[0] });

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
            std.debug.print("User {} sent a program of len {}\n", .{ user.id, msg.len });

            if (msg.len > MAXPROG) {
                return;
            }

            // validate
            for (msg) |c| {
                if (c > 'z' or c < 'a') {
                    std.debug.print("User {} sent bad characters, ignoring\n", .{user.id});
                    return;
                }
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

const idle_sleep_time = 18e8; // 1.5 sec
const no_prog_sleep_time = 12e8; // 1.5 sec
const tick_time_ns = 2e8; // tick every .2 seconds
const tick_time_sec: f32 = tick_time_ns / 1e9;

const speed_mult: f32 = 1;
const turn_step: f32 = std.math.degreesToRadians(90.0 / 24.0);
const turn_step_rate: f32 = turn_step * tick_time_sec;
const set_ang_step: f32 = std.math.degreesToRadians(360.0 / 24.0);

const PixelUpdate = packed struct {
    x: u16,
    y: u16,
    color: u8,
};

const TurtOp = enum(u8) {
    set_fwd = 0,
    add_fwd = 1,
    set_turn_r = 2,
    set_turn_l = 3,
    add_turn_r = 4,
    add_turn_l = 5,
    now_turn_r = 6,
    now_turn_l = 7,
    set_ang = 8,
    set_color = 9,
    next_color = 10,
    read_color = 11,
    set_thick = 12,
    start_loop = 13,
    do_loop = 14,
    wait_x1 = 15,
    wait_x26 = 16,
    _, // TODO fill up all 26
};

/// evaluator thread does:
/// - runs instructions
/// - writes out updates
/// - pauses if no clients are connected
fn evaluator() void {
    loop: while (true) {
        // loop
        var active_users = false;
        // go through each user
        // if we have no users active or no programs, then sleep larger chunks
        for (g_gamectx.users) |*user| {
            {
                user.mux.lock();
                defer user.mux.unlock();

                if (user.wsh != null) {
                    active_users = true;
                    break;
                }
            }
        }

        if (!active_users) {
            std.time.sleep(idle_sleep_time);
            continue :loop;
        }

        // max update of pixels is max thickness with each
        const maxupdate = g_gamectx.users.len * 26 * 26;
        var numupdate: usize = 0;
        const bytesize = 1 + (maxupdate * @sizeOf(PixelUpdate));
        const updatebuf: []u8 = alloc.alloc(u8, bytesize) catch unreachable;
        defer alloc.free(updatebuf);
        updatebuf[0] = @intFromEnum(MsgType.pushupd);

        const updateitems: [*]PixelUpdate = @ptrCast(@alignCast(&updatebuf[1]));

        var active_progs = false;
        for (g_gamectx.users) |*user| {
            const turt: *Turtle = &user.turtle;

            if (turt.progsz == 0) {
                continue;
            } else {
                active_progs = true;
            }

            turt.mux.lock();
            defer turt.mux.unlock();

            if (turt.wait > 0) {
                turt.wait -= 1;
            } else {
                // evaluate instructions
                inst_loop: while (true) {
                    // force wait at end of prog or looping
                    if ((turt.cursor + 2) > turt.progsz) {
                        turt.cursor = 0;
                        break :inst_loop;
                    }

                    turt.cursor += 2;

                    const op: TurtOp = @enumFromInt(turt.prog[turt.cursor] - 'a');
                    const imm = (turt.prog[turt.cursor + 1] - 'a');

                    switch (op) {
                        .set_fwd => {
                            turt.spd = speed_mult * @as(f32, @floatFromInt(imm));
                        },
                        .add_fwd => {
                            turt.spd += @as(f32, @floatFromInt(imm));
                        },
                        .set_turn_r => {
                            turt.rotspd = turn_step_rate * @as(f32, @floatFromInt(imm));
                        },
                        .set_turn_l => {
                            turt.rotspd = -turn_step_rate * @as(f32, @floatFromInt(imm));
                        },
                        .add_turn_r => {
                            turt.rotspd += turn_step_rate * @as(f32, @floatFromInt(imm));
                        },
                        .add_turn_l => {
                            turt.rotspd -= turn_step_rate * @as(f32, @floatFromInt(imm));
                        },
                        .now_turn_r => {
                            turt.rot += turn_step * @as(f32, @floatFromInt(imm));
                        },
                        .now_turn_l => {
                            turt.rot -= turn_step * @as(f32, @floatFromInt(imm));
                        },
                        .set_ang => {
                            turt.rot = set_ang_step * @as(f32, @floatFromInt(imm));
                        },
                        .set_color => {
                            turt.color = @as(color_t, @intCast(imm & 0xf));
                        },
                        .next_color => {
                            turt.color = @intCast((@as(u8, turt.color) + imm) & 0xf);
                        },
                        .read_color => {
                            //TODO use imm?

                            g_gamectx.floor.mux.lock();
                            defer g_gamectx.floor.mux.unlock();

                            const undercolor = g_gamectx.floor.img[
                                g_gamectx.floor.pos2idx(@intFromFloat(turt.x), @intFromFloat(turt.y))
                            ];

                            turt.color = undercolor;
                        },
                        .set_thick => {
                            turt.thick = imm;
                        },
                        .start_loop => {
                            if (turt.loop_head >= MAXLOOP) {
                                // too many loops
                                continue :inst_loop;
                            }

                            turt.loop_pos[turt.loop_head] = turt.cursor;
                            turt.loop_count[turt.loop_head] = imm + 1;
                        },
                        .do_loop => {
                            if (turt.loop_head <= imm) {
                                // no loop target there
                                std.debug.print("Unknown loop target, {} {}\n", .{ turt.loop_head, imm });
                                continue :inst_loop;
                            }

                            const targ = turt.loop_head - (imm + 1);
                            if (turt.loop_count[targ] != 0) {
                                turt.loop_count[targ] -= 1;
                                turt.cursor = turt.loop_pos[targ];
                            }

                            // force wait a tick if using this inst
                            break :inst_loop;
                        },
                        .wait_x1 => {
                            turt.wait = imm;
                            break :inst_loop;
                        },
                        .wait_x26 => {
                            turt.wait = imm * 26;
                            break :inst_loop;
                        },
                        _ => {
                            // unused (for now)
                            // force a wait
                            turt.wait = imm;
                            break :inst_loop;
                        },
                    }
                }
            }

            // draw first
            {
                g_gamectx.floor.mux.lock();
                defer g_gamectx.floor.mux.unlock();

                const thk: i32 = turt.thick;
                var dy = -thk;
                draw_loop: while (dy <= thk) : (dy += 1) {
                    const ty = @as(i32, @intFromFloat(turt.y)) - @as(i32, @intCast(dy));
                    var dx = -thk;
                    while (dx <= thk) : (dx += 1) {
                        const tx = @as(i32, @intFromFloat(turt.x)) - @as(i32, @intCast(dx));
                        const indx = g_gamectx.floor.pos2idx(tx, ty);
                        const prev_color: color_t = g_gamectx.floor.img[indx];
                        if (prev_color != turt.color) {
                            g_gamectx.floor.img[indx] = turt.color;

                            if (numupdate >= maxupdate) {
                                std.debug.print("Too many updates!\n", .{});
                                break :draw_loop;
                            }

                            // add to update
                            //TODO don't do if pixel already updated this tick?
                            updateitems[numupdate] = .{
                                .x = @intCast(tx),
                                .y = @intCast(ty),
                                .color = @as(u8, turt.color),
                            };
                            numupdate += 1;
                        }
                    }
                }
            }

            // then apply movement
            turt.rot += turt.rotspd;
            if (turt.rot > std.math.tau) {
                turt.rot -= std.math.tau;
            }
            if (turt.rot < -0.0) {
                turt.rot += std.math.tau;
            }

            const fw: f32 = @floatFromInt(g_gamectx.floor.w);
            var nxtx: f32 = turt.x + (@cos(turt.rot) * turt.spd);
            while (nxtx >= fw) {
                nxtx -= fw;
            }
            while (nxtx < 0.0) {
                nxtx += fw;
            }
            turt.x = nxtx;

            const fh: f32 = @floatFromInt(g_gamectx.floor.h);
            var nxty: f32 = turt.y + (@sin(turt.rot) * turt.spd);
            while (nxty >= fh) {
                nxty -= fh;
            }
            while (nxty < 0.0) {
                nxty += fw;
            }
            turt.y = nxty;
        }

        if (!active_progs) {
            std.time.sleep(no_prog_sleep_time);
            continue :loop;
        }

        // sent out updates
        for (g_gamectx.users) |*user| {
            user.mux.lock();
            defer user.mux.unlock();

            if (user.wsh) |handle| {
                WebsocketHandler.write(handle, updatebuf, false) catch |err| {
                    std.debug.print("Unable to write out updates: {any} {?}\n", .{ err, handle });
                };
            }
        }

        // tick
        std.time.sleep(tick_time_ns);
        // loop
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
    while (argi.next()) |a| : (i += 1) {
        switch (i) {
            0 => {
                prog = a;
            },
            1 => {
                usernum = std.fmt.parseInt(u16, a, 0) catch 0;
            },
            2 => {
                floorw = std.fmt.parseInt(u16, a, 0) catch 0;
            },
            3 => {
                floorh = std.fmt.parseInt(u16, a, 0) catch 0;
            },
            else => {
                do_help = true;
                break;
            },
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
    const evaluator_thrd = std.Thread.spawn(
        std.Thread.SpawnConfig{},
        evaluator,
        .{},
    ) catch unreachable;

    // start worker threads
    zap.start(.{
        .threads = 1,
        .workers = 1,
    });

    std.Thread.join(evaluator_thrd);
}
