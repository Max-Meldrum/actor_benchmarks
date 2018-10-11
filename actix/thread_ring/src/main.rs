extern crate actix;
extern crate futures;


use actix::prelude::*;
use std::io;
use std::time::Duration;
use std::any::Any;
use actix::dev::{MessageResponse, ResponseChannel};
use futures::Future;
use std::env;

enum Messages {
    PingMsg(usize),
    NextActorMsg(Addr<RingActor>)
}

impl Message for Messages {
    type Result = ();
}



// Define Actors

struct RingActor {
    id: usize,
    next_actor: Option<Addr<RingActor>>
}

impl RingActor {
    fn new(id: usize) -> RingActor {
        RingActor {
            id: id,
            next_actor: None
        }
    }
}

impl Actor for RingActor {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Context<Self>) {
    }

    fn stopped(&mut self, ctx: &mut Context<Self>) {
    }
}


impl Handler<Messages> for RingActor {
    type Result = ();

    fn handle(&mut self, msg: Messages, ctx: &mut Context<Self>) -> Self::Result {
        match msg {
            Messages::PingMsg(count) => {
                match &self.next_actor {
                    Some(next) => {
                        if count > 0 {
                            next.do_send(Messages::PingMsg(count-1));
                            ()
                        } else {
                            System::current().stop();
                        }
                    },
                    None => {}
                }
            },
            Messages::NextActorMsg(addr)  => {
                self.next_actor = Some(addr)
            }
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let ring_size_str = &args[1].to_string();
        let ring_size: usize = ring_size_str.parse().unwrap();

        let msg_count_str = &args[2].to_string();
        let msg_count: usize = msg_count_str.parse().unwrap();

        let sys = System::new("thread_ring");

        let mut actor_vec: Vec<Addr<RingActor>> = Vec::new();
        for x in 0..ring_size {
            let actor = RingActor::new(x).start();
            actor_vec.push(actor);
        }

        for (i, actor_ref) in actor_vec.iter().enumerate().collect::<Vec<_>>(){
            let next_actor: &Addr<RingActor> = &actor_vec[(i+1) % ring_size];
            actor_ref.do_send(Messages::NextActorMsg(next_actor.clone()));
        }

        let starter: &Addr<RingActor> = &actor_vec[0];
        starter.do_send(Messages::PingMsg(msg_count));
        sys.run();
    } else {
        println!("{}", "No Ring Size or Count amount was given, exiting");
    }
}
