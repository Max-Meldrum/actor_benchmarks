extern crate kompact;

use kompact::*;

use ComponentContext;
use Provide;
use default_components::DeadletterBox;

use ActorRef;
use ActorPath;
use ControlEvent;
use ControlPort;
use KompicsConfig;
use KompicsSystem;
use Transport;
use prelude::*;

use std::{thread};
use std::time::Duration;
use std::process;
use std::env;

#[derive(Debug, Clone)]
struct Ping;

#[derive(Debug, Clone)]
struct Pong;


#[derive(Debug, Clone)]
struct Start;

impl Serialisable for Ping {
    fn serid(&self) -> u64 {
        55 // random
    }
    fn size_hint(&self) -> Option<usize> {
        Some(0)
    }
    fn serialise(&self, _buf: &mut BufMut) -> Result<(), SerError> {
        Ok(())
    }
    fn local(self: Box<Self>) -> Result<Box<Any + Send>, Box<Serialisable>> {
        Ok(self)
    }
}

impl Serialisable for Pong {
    fn serid(&self) -> u64 {
        55 // random
    }
    fn size_hint(&self) -> Option<usize> {
        Some(4)
    }
    fn serialise(&self, _buf: &mut BufMut) -> Result<(), SerError> {
        Ok(())
    }
    fn local(self: Box<Self>) -> Result<Box<Any + Send>, Box<Serialisable>> {
        Ok(self)
    }
}

impl Serialisable for Start {
    fn serid(&self) -> u64 {
        55 // random
    }
    fn size_hint(&self) -> Option<usize> {
        Some(0)
    }
    fn serialise(&self, _buf: &mut BufMut) -> Result<(), SerError> {
        Ok(())
    }
    fn local(self: Box<Self>) -> Result<Box<Any + Send>, Box<Serialisable>> {
        Ok(self)
    }
}


// Pinger Actor

#[derive(ComponentDefinition)]
struct Pinger {
    ctx: ComponentContext<Pinger>,
    ponger: ActorRef,
    count: usize
}

impl Pinger {
    fn new(ponger: ActorRef, count: usize) -> Pinger {
        Pinger {
            ctx: ComponentContext::new(),
            ponger: ponger,
            count: count
        }
    }
}

impl Actor for Pinger {
    fn receive_local(&mut self, sender: ActorRef, msg: Box<Any>) -> () {
        // TODO: improve this..
        if let Some(_i) = msg.downcast_ref::<Start>() {
            self.ponger.tell(Box::new(Ping{}), self)
        }

        if let Some(_r) = msg.downcast_ref::<Pong>() {
            if self.count > 0 {
                self.count -= 1;
                sender.tell(Box::new(Ping{}), self)
            } else {
                std::process::exit(0)
            }
        }
    }
    fn receive_message(&mut self, _sender: ActorPath, _ser_id: u64, _buf: &mut Buf) -> () {}
}


impl Provide<ControlPort> for Pinger {
    fn handle(&mut self, event: ControlEvent) -> () {
        match event {
            ControlEvent::Start => {
            }
            _ => (),
        }
    }
}

#[derive(ComponentDefinition)]
struct Ponger {
    ctx: ComponentContext<Ponger>
}

impl Ponger {
    fn new() -> Ponger {
        Ponger {
            ctx: ComponentContext::new()
        }
    }
}

impl Actor for Ponger {
    fn receive_local(&mut self, sender: ActorRef, msg: Box<Any>) -> () {
        if let Some(_ping) = msg.downcast_ref::<Ping>() {
            sender.tell(Box::new(Pong{}), self)
        }
    }
    fn receive_message(&mut self, _sender: ActorPath, _ser_id: u64, _buf: &mut Buf) -> () {}
}

impl Provide<ControlPort> for Ponger {
    fn handle(&mut self, event: ControlEvent) -> () {
        match event {
            ControlEvent::Start => {
            }
            _ => (),
        }
    }
}

fn main() {

    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let count_str = &args[1].to_string();
        let count: usize = count_str.parse().unwrap();

        let mut cfg = KompicsConfig::new();
        cfg.system_components(DeadletterBox::new, NetworkDispatcher::default);
        let system = KompicsSystem::new(cfg);

        let ponger = system.create_and_register(Ponger::new);
        system.start(&ponger);

        let pinger = system.create_and_register(move || Pinger::new(ponger.actor_ref().clone(), count));
        system.start(&pinger);

        pinger.actor_ref().tell(Box::new(Start{}), &pinger);
        thread::sleep(Duration::from_millis(5000000));

    } else {
        println!("{}", "No Count amount was given, exiting");
    }



}

