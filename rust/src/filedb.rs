extern crate serde_json;

use core;

use std::fs::File;
use std::path::Path;
use std::io::{Read, Error, Write};

pub fn save_claims_to_file(claimsets: &Vec<&core::ClaimSet>, path: &String) -> Result<(), Error> {
    let path = Path::new(path);
    let mut file = File::create(&path)?;

    let claimsets_js: Vec<core::ClaimSetJS> = claimsets
        .into_iter()
        .map(|claims| core::ClaimSetJS::from_claimset(claims))
        .collect();
    let claimset_str = serde_json::to_string(&claimsets_js)?;
    file.write_fmt(format_args!("{}", claimset_str))?;

    Ok(())
}

pub fn load_claims_from_file(path: &String) -> Result<Vec<core::ClaimSet>, Error> {
    let path = Path::new(path);
    let mut file = File::open(&path)?;
    let mut text = String::new();

    file.read_to_string(&mut text)?;

    let claimsets_js: Vec<core::ClaimSetJS> = serde_json::from_str(&text)?;
    let claimsets = claimsets_js
        .into_iter()
        .map(|cl_js| core::ClaimSetJS::to_claimset(&cl_js))
        .collect();

    Ok(claimsets)
}
