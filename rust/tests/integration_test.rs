extern crate chrono;
extern crate orizentic;

use std::time;
use std::thread;
use orizentic::*;

#[test]
fn can_create_a_new_token() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string().into_bytes()), Vec::new());
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
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string().into_bytes()), Vec::new());
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
    let tok_list = ctx.list_claims();
    assert_eq!(tok_list.len(), 1);
    assert!(!tok_list.contains(&claims.id));
    assert!(tok_list.contains(&claims2.id));
}

#[test]
fn rejects_tokens_with_an_invalid_secret() {
    let mut ctx1 = OrizenticCtx::new_ctx(Secret("ctx1".to_string().into_bytes()), Vec::new());
    let ctx2 = OrizenticCtx::new_ctx(Secret("ctx2".to_string().into_bytes()), Vec::new());
    let claims = ctx1.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    let token_str = ctx1.encode_claims(&claims).unwrap();
    assert!(ctx2.decode_and_validate_text(&token_str).is_none());
}

#[test]
fn rejects_tokens_that_are_absent_from_the_database() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = ctx.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    let token_str = ctx.encode_claims(&claims).unwrap();

    ctx.revoke_claims(&claims);
    assert!(ctx.decode_and_validate_text(&token_str).is_none());
}

#[test]
fn validates_present_tokens_with_a_valid_secret() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = ctx.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    let token_str = ctx.encode_claims(&claims).unwrap();
    println!("[validates_present_tokens] {}", token_str);
    assert!(ctx.decode_and_validate_text(&token_str).is_some());
}

#[test]
fn rejects_expired_tokens() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = ctx.create_claims(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(1))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    thread::sleep(time::Duration::from_secs(2));
    let token_str = ctx.encode_claims(&claims).unwrap();
    assert!(ctx.decode_and_validate_text(&token_str).is_none());
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

