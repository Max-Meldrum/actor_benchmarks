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
struct IncrementMsg;

#[derive(Debug, Clone)]
struct RetrieveMsg;


#[derive(Debug, Clone)]
struct ResultMsg {
    result: usize
}

impl Serialisable for IncrementMsg {
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

impl Serialisable for ResultMsg {
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

impl Serialisable for RetrieveMsg {
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


// Counting Actor

#[derive(ComponentDefinition)]
struct CountingActor {
    ctx: ComponentContext<CountingActor>,
    count: usize
}

impl CountingActor {
    fn new() -> CountingActor {
        CountingActor {
            ctx: ComponentContext::new(),
            count: 0
        }
    }
}

impl Actor for CountingActor {
    fn receive_local(&mut self, sender: ActorRef, msg: Box<Any>) -> () {
        // TODO: improve this..
        if let Some(_i) = msg.downcast_ref::<IncrementMsg>() {
            self.count += 1;
        }

        if let Some(_r) = msg.downcast_ref::<RetrieveMsg>() {
            sender.tell(Box::new(ResultMsg{result: self.count}), self)
        }
    }
    fn receive_message(&mut self, _sender: ActorPath, _ser_id: u64, _buf: &mut Buf) -> () {}
}


impl Provide<ControlPort> for CountingActor {
    fn handle(&mut self, event: ControlEvent) -> () {
        match event {
            ControlEvent::Start => {
            }
            _ => (),
        }
    }
}



// Producer Actor


#[derive(ComponentDefinition)]
struct ProducerActor {
    ctx: ComponentContext<ProducerActor>,
    counter_actor: ActorRef,
    increments: usize
}

impl ProducerActor {
    fn new(counter_actor: ActorRef, increments: usize) -> ProducerActor {
        ProducerActor {
            ctx: ComponentContext::new(),
            counter_actor,
            increments
        }
    }
}

impl Actor for ProducerActor {
    fn receive_local(&mut self, _sender: ActorRef, msg: Box<Any>) -> () {
        if let Some(ref i) = msg.downcast_ref::<ResultMsg>() {
            assert!(i.result == self.increments);
            process::exit(0); // Is there a System::shutdown for Kompact?
        }
    }
    fn receive_message(&mut self, _sender: ActorPath, _ser_id: u64, _buf: &mut Buf) -> () {}
}


impl Provide<ControlPort> for ProducerActor {
    fn handle(&mut self, event: ControlEvent) -> () {
        match event {
            ControlEvent::Start => {
                for _ in 0..self.increments {
                    self.counter_actor.tell(Box::new(IncrementMsg), self)
                }
                self.counter_actor.tell(Box::new(RetrieveMsg), self)
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

        let thread_str = &args[2].to_string();
        let thread_size: usize = thread_str.parse().unwrap();

        let mut cfg = KompicsConfig::new();
        cfg.threads(thread_size);
        cfg.system_components(DeadletterBox::new, NetworkDispatcher::default);
        let system = KompicsSystem::new(cfg);

        let counter = system.create_and_register(CountingActor::new);
        system.start(&counter);
        let producer = system.create_and_register(move || ProducerActor::new(counter.actor_ref().clone(), count));
        system.start(&producer);

        thread::sleep(Duration::from_millis(5000000));

    } else {
        println!("{}", "No Count amount was given, exiting");
    }



}

