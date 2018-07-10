
use core;

use std::fs::File;
use std::path::Path;
use std::io::{ BufReader, BufRead, Error, Lines, Write };

pub fn save_claims_to_file(claimsets: &Vec<&core::ClaimSet>, path: &String) -> Result<(), Error> {
    let path = Path::new(path);
    let mut file = File::create(&path)?;

    for claimset in claimsets.iter() {
        let claimset_str = core::to_json(claimset)?;
        file.write_fmt(format_args!("{}\n", claimset_str))?;
    };
    Ok(())
}

pub fn load_claims_from_file(path: &String) -> Result<Vec<core::ClaimSet>, Error> {
    let path = Path::new(path);
    let f = File::open(&path)?;
    let mut file = BufReader::new(&f);
    let mut res = Vec::new();
    for line in file.lines() {
        match line {
            Ok(line_) => {
                let val = core::from_json(&line_)?;
                res.push(val);
            },
            Err(err) => return Err(err),
        }
    };
    Ok(res)
}

