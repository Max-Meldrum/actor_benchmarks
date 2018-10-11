extern crate actix;
extern crate futures;


use actix::prelude::*;
use std::io;
use std::time::Duration;
use std::env;


// Define messages
struct Ping {
    addr: Recipient<Pong>
}

impl Message for Ping {
    type Result = ();
}


struct Pong;

impl Message for Pong {
    type Result = ();
}

struct Start;

impl Message for Start {
    type Result = ();
}

// Define Actors

struct Pinger {
    ponger: Recipient<Ping>,
    count: usize
}

impl Actor for Pinger {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Context<Self>) {
    }

    fn stopped(&mut self, ctx: &mut Context<Self>) {
    }
}


impl Handler<Pong> for Pinger {
    type Result = ();

    fn handle(&mut self, msg: Pong, ctx: &mut Context<Self>) -> () {
        if self.count > 0 {
            self.count -= 1;
            self.ponger.do_send(Ping{addr: ctx.address().recipient()});
        } else {
            System::current().stop();
        }
    }
}


impl Handler<Start> for Pinger {
    type Result = ();

    fn handle(&mut self, msg: Start, ctx: &mut Context<Self>) -> () {
        self.ponger.do_send(Ping{addr: ctx.address().recipient()});
    }
}

struct Ponger;

impl Actor for Ponger {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Context<Self>) {
    }

    fn stopped(&mut self, ctx: &mut Context<Self>) {
    }
}

impl Handler<Ping> for Ponger {
    type Result = ();

    fn handle(&mut self, msg: Ping, ctx: &mut Context<Self>) {
        msg.addr.do_send(Pong{});
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let count_str = &args[1].to_string();
        let count: usize = count_str.parse().unwrap();
        let sys = System::new("ping_pong");
        let ponger = Ponger{}.start();
        let pinger = Pinger{count: count, ponger: ponger.recipient()}.start();
        pinger.do_send(Start{});
        sys.run();
    } else {
        println!("{}", "No Count amount was given, exiting");
    }


}
