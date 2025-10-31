mod bindings {
    use crate::Handler;

    wit_bindgen::generate!({
        path: "..",
        world: "handler",
        generate_all,
    });
    export!(Handler);
}

use bindings::exports::docs::greet::greet::Guest;

struct Handler;

impl Guest for Handler {
    fn greet(name: String) -> String {
        format!("hello, {name}!")
    }
}
