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
use std::env;
use std::time::Instant;


#[derive(Debug, Clone)]
struct PingMsg {
    pings_left: usize
}

impl PingMsg {
    fn has_next(&self) -> bool {
        self.pings_left > 0
    }

    fn next(&self) -> PingMsg {
        PingMsg{pings_left: self.pings_left - 1}
    }
}

#[derive(Debug, Clone)]
struct NextActorMsg {
    actor: ActorRef
}


impl Serialisable for PingMsg {
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

impl Serialisable for NextActorMsg {
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

// Ring Actor

#[derive(ComponentDefinition)]
struct RingActor {
    ctx: ComponentContext<RingActor>,
    id: usize,
    total_actors: usize,
    next_actor: Option<ActorRef>,
    instant: Instant
}

impl RingActor {
    fn new(id: usize, total: usize, instant: Instant) -> RingActor {
        RingActor {
            ctx: ComponentContext::new(),
            id: id,
            total_actors: total,
            next_actor: None,
            instant
        }
    }

}

impl Provide<ControlPort> for RingActor {
    fn handle(&mut self, event: ControlEvent) -> () {
        match event {
            ControlEvent::Start => {
            }
            _ => (),
        }
    }
}

impl Actor for RingActor {
    fn receive_local(&mut self, sender: ActorRef, msg: Box<Any>) -> () {
        if let Some(ref na) = msg.downcast_ref::<NextActorMsg>() {
            self.next_actor = Some(na.actor.clone());
            // Clone :/
        }

        if let Some(ref ping) = msg.downcast_ref::<PingMsg>() {
            match &self.next_actor {
                Some(next) => {
                    if ping.has_next() {
                        next.tell(Box::new(ping.next()), self)
                    } else {
                        let elapsed = self.instant.elapsed();
                        println!("Elapsed: {} ms",
                                 (elapsed.as_secs() * 1_000) + (elapsed.subsec_nanos() / 1_000_000) as u64);
                        std::process::exit(0);
                    }
                },
                None => {}
            }
        }
    }
    fn receive_message(&mut self, _sender: ActorPath, _ser_id: u64, _buf: &mut Buf) -> () {}
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() > 2 {
        let start = Instant::now();
        let ring_size_str = &args[1].to_string();
        let ring_size: usize = ring_size_str.parse().unwrap();

        let msg_count_str = &args[2].to_string();
        let msg_count: usize = msg_count_str.parse().unwrap();

        let thread_str = &args[3].to_string();
        let thread_size: usize = thread_str.parse().unwrap();

        let mut cfg = KompicsConfig::new();
        cfg.threads(thread_size);
        cfg.system_components(DeadletterBox::new, NetworkDispatcher::default);
        let system = KompicsSystem::new(cfg);


        let mut actor_vec: Vec<ActorRef> = Vec::new();
        for x in 0..ring_size {
            let actor = system.create_and_register(move || RingActor::new(x, ring_size, start));
            system.start(&actor);
            let actor_ref = actor.actor_ref();
            actor_vec.push(actor_ref)
        }

        for (i, actor_ref) in actor_vec.iter().enumerate().collect::<Vec<_>>(){
            let next_actor: &ActorRef = &actor_vec[(i+1) % ring_size];
            actor_ref.tell(Box::new(NextActorMsg{actor: next_actor.clone()}), actor_ref);
        }

        let starter: &ActorRef = &actor_vec[0];
        starter.tell(Box::new(PingMsg{pings_left: msg_count}), starter);

        thread::sleep(Duration::from_millis(5000000));
    } else {
        println!("{}", "No Ring Size or Count amount was given, exiting");
    }
}

