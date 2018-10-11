extern crate actix;
extern crate futures;


use actix::prelude::*;
use std::io;
use std::time::Duration;
use std::any::Any;
use actix::dev::{MessageResponse, ResponseChannel};
use futures::Future;
use std::env;
use std::time::Instant;

enum Messages {
    IncrementMsg,
    RetrieveMsg(Recipient<Messages>),
    ResultMsg(usize)
}


impl Message for Messages {
    type Result = ();
}


// Define Actors

struct CountingActor{
    count: usize
}

impl CountingActor {
    fn new(init_val: usize) -> CountingActor {
        CountingActor {
            count: init_val
        }
    }
}

impl Actor for CountingActor {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Context<Self>) {
    }

    fn stopped(&mut self, ctx: &mut Context<Self>) {
    }
}


impl Handler<Messages> for CountingActor {
    type Result = ();

    fn handle(&mut self, msg: Messages, ctx: &mut Context<Self>) -> Self::Result {
        match msg {
            Messages::IncrementMsg => {
                self.count += 1;
            },
            Messages::RetrieveMsg(addr)  => {
                addr.do_send(Messages::ResultMsg(self.count));
            },
            Messages::ResultMsg(_) => {

            }
        }
    }
}

struct ProducerActor {
    counter_actor: Recipient<Messages>,
    increments: usize
}

impl ProducerActor {
    fn new(target: Recipient<Messages>, num: usize) -> ProducerActor {
        ProducerActor {
            counter_actor: target,
            increments: num
        }
    }
}

impl Actor for ProducerActor {
    type Context = Context<Self>;

    fn started(&mut self, ctx: &mut Context<Self>) {
        for _ in 0..self.increments {
            self.counter_actor.do_send(Messages::IncrementMsg{});
        }
        let _self = ctx.address().recipient();
        self.counter_actor.do_send(Messages::RetrieveMsg(_self));
    }

    fn stopped(&mut self, ctx: &mut Context<Self>) {
    }
}

impl Handler<Messages> for ProducerActor{
    type Result = ();

    fn handle(&mut self, msg: Messages, ctx: &mut Context<Self>) -> Self::Result {
        match msg {
            Messages::IncrementMsg => {
            },
            Messages::RetrieveMsg(addr)  => {
            },
            Messages::ResultMsg(count)=> {
                assert!(count == self.increments);
                System::current().stop()
            }
        }
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let start = Instant::now();
        let count_str = &args[1].to_string();
        let count: usize = count_str.parse().unwrap();
        let sys = System::new("counting_actor");
        let counter = CountingActor::new(0).start();
        let producer = ProducerActor::new(counter.recipient(), count).start();
        sys.run();
        let elapsed = start.elapsed();
        println!("Elapsed: {} ms",
                 (elapsed.as_secs() * 1_000) + (elapsed.subsec_nanos() / 1_000_000) as u64);
    } else {
        println!("{}", "No Count amount was given, exiting");
    }

}
