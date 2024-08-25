#[rustler::nif]
pub fn truly_random() -> i64 {
    4 // Chosen by fair dice roll. Guaranteed to be random.
}

rustler::init!("glans");
