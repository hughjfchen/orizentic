extern crate chrono;
extern crate orizentic;

use chrono::prelude::*;
use orizentic::*;

#[test]
fn can_create_a_new_token() {
    let ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string()), Vec::new());
    let claims = ctx.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    let claims2 = ctx.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );

    assert_eq!(claims.audience, Username(String::from("Savanni")));
    match claims.expiration {
        Some(ttl) => assert_eq!(ttl - claims.issued_at, chrono::Duration::seconds(3600)),
        None => panic!("ttl should not be None"),
    }
    assert_eq!(claims.issuer, Issuer(String::from("test")));
    assert_eq!(claims.resource, ResourceName(String::from("resource-1")));
    assert_eq!(claims.permissions, Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]));

    assert_ne!(claims2.id, claims.id);
    assert_eq!(claims2.resource, ResourceName(String::from("resource-2")));
}

#[test]
fn can_revoke_a_token() {
}

