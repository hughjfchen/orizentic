extern crate version_check;

pub fn main() {
    let check_result = version_check::is_min_version("1.33.0");
    match check_result {
        Some((true, _)) => (),
        Some((false, _)) => panic!("not version 1.33.0"),
        None => panic!("could not find the version"),
    }
}
