#[rustler::nif]
pub fn truly_random() -> i64 {
    666 // Chosen by fair dice roll. Guaranteed to be random.
}

rustler::init!("libglans");
