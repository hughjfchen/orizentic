extern crate clap;
extern crate itertools;
extern crate orizentic;

use clap::{Arg, App, SubCommand, ArgMatches};
use std::env;
use itertools::Itertools;

use orizentic::*;


// ORIZENTIC_DB
// ORIZENTIC_SECRET
//
// list
// create
// revoke
// encode
pub fn main() {
    let db_path = env::var_os("ORIZENTIC_DB")
        .map(|str| str.into_string().expect("ORIZENTIC_DB contains invalid Unicode sequences"));
    let secret = env::var_os("ORIZENTIC_SECRET")
        .map(|str| Secret(str.into_string()
                          .map(|s| s.into_bytes())
                          .expect("ORIZENTIC_SECRET contains invalid Unicode sequences")));

    println!("ORIZENTIC_DB:     {:?}", db_path);
    println!("ORIZENTIC_SECRET: {:?}", secret);

    let matches = App::new("orizentic cli")
        .subcommand(SubCommand::with_name("list"))
        .subcommand(SubCommand::with_name("create")
                    .arg(Arg::with_name("issuer").long("issuer").takes_value(true).required(true))
                    .arg(Arg::with_name("ttl").long("ttl").takes_value(true).required(true))
                    .arg(Arg::with_name("resource").long("resource").takes_value(true).required(true))
                    .arg(Arg::with_name("username").long("username").takes_value(true).required(true))
                    .arg(Arg::with_name("perms").long("perms").takes_value(true).required(true)))
        .subcommand(SubCommand::with_name("revoke")
                    .arg(Arg::with_name("id").long("id").takes_value(true).required(true)))
        .subcommand(SubCommand::with_name("encode")
                    .arg(Arg::with_name("id").long("id").takes_value(true).required(true)))
        .get_matches();

    match matches.subcommand() {
        ("list",   _) => { list_tokens(db_path) },
        ("create", Some(args)) => { create_token(db_path, secret, args) },
        ("revoke", Some(args)) => { revoke_token(db_path, secret, args) },
        ("encode", Some(args)) => { encode_token(db_path, secret, args) },
        (cmd, _) => { println!("unknown subcommand: {}", cmd); },
    }
}


fn list_tokens(db_path: Option<String>) {
    let db_path_ = db_path.expect("ORIZENTIC_DB is required for this operation");
    let claimsets = orizentic::filedb::load_claims_from_file(&db_path_);
    match claimsets {
        Ok(claimsets_) => {
            for claimset in claimsets_ {
                println!("[{}]", claimset.id);
                println!("Audience:         {}", claimset.audience.0);
                match claimset.expiration {
                    Some(expiration) => println!("Expiration:       {}", expiration.format("%Y-%m-%d %H:%M:%S")),
                    None => println!("Expiration:       None"),
                }
                println!("Issuer:           {}", claimset.issuer.0);
                println!("Issued At:        {}",
                    claimset.issued_at.format("%Y-%m-%d %H:%M:%S"));
                println!("Resource Name:    {}", claimset.resource.0);

                let perm_val: String = claimset.permissions.0.clone().into_iter().intersperse(String::from(", ")).collect();
                println!("Permissions:      {}", perm_val);
                println!("")
            }
        },
        Err(err) => {
            println!("claimset failed to load: {}", err);
            std::process::exit(1);
        },
    }
}


fn create_token(db_path: Option<String>, secret: Option<Secret>, args: &ArgMatches) {
    let db_path_ = db_path.expect("ORIZENTIC_DB is required for this operation");
    let secret_ = secret.expect("ORIZENTIC_SECRET is required for this operation");
    println!("create_token");
    println!("issuer: {}", args.value_of("issuer").unwrap());
    println!("ttl: {}", args.value_of("ttl").unwrap());
    println!("resource: {}", args.value_of("resource").unwrap());
    println!("username: {}", args.value_of("username").unwrap());
    println!("perms: {}", args.value_of("perms").unwrap());
}


fn revoke_token(db_path: Option<String>, secret: Option<Secret>, args: &ArgMatches) {
    println!("revoke_token");
}


fn encode_token(db_path: Option<String>, secret: Option<Secret>, args: &ArgMatches) {
    println!("encode_token");
}

