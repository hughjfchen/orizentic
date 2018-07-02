extern crate chrono;
extern crate orizentic;

use chrono::prelude::*;
use orizentic::*;

#[test]
fn can_create_a_new_token() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string()), Vec::new());
    let claims = ctx.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
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
    {
        let tok_list = ctx.list_claims();
        assert_eq!(tok_list.len(), 1);
        assert!(tok_list.contains(&claims.id));
    }

    let claims2 = ctx.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );

    assert_ne!(claims2.id, claims.id);
    assert_eq!(claims2.resource, ResourceName(String::from("resource-2")));

    let tok_list = ctx.list_claims();
    assert_eq!(tok_list.len(), 2);
    assert!(tok_list.contains(&claims.id));
    assert!(tok_list.contains(&claims2.id));
}

#[test]
fn can_revoke_a_token() {
    /*
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string()), Vec::new());
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

    ctx.revoke_claims(&claims);
    let tokList = ctx.list_claims();
    assert_eq!(tokList.len(), 1);
    assert!(!tokList.contains(&&claims));
    assert!(tokList.contains(&&claims2));
    */
}

#[test]
fn rejects_tokens_with_an_invalid_secret() {
}

#[test]
fn rejects_tokens_that_are_absent_from_the_database() {
}

#[test]
fn validates_present_tokens_with_a_valid_secret() {
}

#[test]
fn rejects_expired_tokens() {
}

#[test]
fn accepts_tokens_that_have_no_expiration() {
}

#[test]
fn authorizes_a_token_with_the_correct_resource_and_permissions() {
}

#[test]
fn rejects_a_token_with_the_incorrect_permissions() {
}

#[test]
fn rejects_a_token_with_the_incorrect_resource_name() {
}

