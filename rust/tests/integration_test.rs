extern crate chrono;
extern crate orizentic;

use std::time;
use std::thread;
use orizentic::*;

#[test]
fn can_create_a_new_claimset() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    assert_eq!(claims.audience, Username(String::from("Savanni")));
    match claims.expiration {
        Some(ttl) => assert_eq!(ttl - claims.issued_at, chrono::Duration::seconds(3600)),
        None => panic!("ttl should not be None"),
    }
    assert_eq!(claims.issuer, Issuer(String::from("test")));
    assert_eq!(claims.resource, ResourceName(String::from("resource-1")));
    assert_eq!(claims.permissions, Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]));
    {
        let tok_list = ctx.list_claimsets();
        assert_eq!(tok_list.len(), 1);
        assert!(tok_list.contains(&claims.id));
    }

    let claims2 = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims2.clone());

    assert_ne!(claims2.id, claims.id);
    assert_eq!(claims2.resource, ResourceName(String::from("resource-2")));

    let tok_list = ctx.list_claimsets();
    assert_eq!(tok_list.len(), 2);
    assert!(tok_list.contains(&claims.id));
    assert!(tok_list.contains(&claims2.id));
}

#[test]
fn can_retrieve_claim_by_id() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    let claims2 = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    ctx.add_claimset(claims2.clone());

    assert_eq!(ctx.find_claimset(&claims.id), Some(&claims));
    assert_eq!(ctx.find_claimset(&claims2.id), Some(&claims2));

    ctx.revoke_claimset(&claims);
    assert_eq!(ctx.find_claimset(&claims.id), None);
    assert_eq!(ctx.find_claimset(&claims2.id), Some(&claims2));
}

#[test]
fn can_revoke_claim_by_id() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    let claims2 = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );

    ctx.add_claimset(claims.clone());
    ctx.add_claimset(claims2.clone());

    assert_eq!(ctx.find_claimset(&claims.id), Some(&claims));
    assert_eq!(ctx.find_claimset(&claims2.id), Some(&claims2));

    ctx.revoke_by_uuid(&claims.id);
    assert_eq!(ctx.find_claimset(&claims.id), None);
    assert_eq!(ctx.find_claimset(&claims2.id), Some(&claims2));
}


#[test]
fn can_revoke_a_token() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("abcdefg".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    let claims2 = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    ctx.add_claimset(claims2.clone());

    ctx.revoke_claimset(&claims);
    let tok_list = ctx.list_claimsets();
    assert_eq!(tok_list.len(), 1);
    assert!(!tok_list.contains(&claims.id));
    assert!(tok_list.contains(&claims2.id));
}

#[test]
fn rejects_tokens_with_an_invalid_secret() {
    let mut ctx1 = OrizenticCtx::new_ctx(Secret("ctx1".to_string().into_bytes()), Vec::new());
    let ctx2 = OrizenticCtx::new_ctx(Secret("ctx2".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx1.add_claimset(claims.clone());
    let encoded_token = ctx1.encode_claimset(&claims).ok().unwrap();
    assert!(ctx2.decode_and_validate_text(&encoded_token.text).is_err());
}

#[test]
fn rejects_tokens_that_are_absent_from_the_database() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    let encoded_token = ctx.encode_claimset(&claims).ok().unwrap();

    ctx.revoke_claimset(&claims);
    assert!(ctx.decode_and_validate_text(&encoded_token.text).is_err());
}

#[test]
fn validates_present_tokens_with_a_valid_secret() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(3600))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    let encoded_token = ctx.encode_claimset(&claims).ok().unwrap();
    assert!(ctx.decode_and_validate_text(&encoded_token.text).is_ok());
}

#[test]
fn rejects_expired_tokens() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        Some(TTL(chrono::Duration::seconds(1))),
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    thread::sleep(time::Duration::from_secs(2));
    let encoded_token = ctx.encode_claimset(&claims).ok().unwrap();
    assert!(ctx.decode_and_validate_text(&encoded_token.text).is_err());
}

#[test]
fn accepts_tokens_that_have_no_expiration() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        None,
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    let encoded_token = ctx.encode_claimset(&claims).ok().unwrap();
    assert!(ctx.decode_and_validate_text(&encoded_token.text).is_ok());
}

#[test]
fn authorizes_a_token_with_the_correct_resource_and_permissions() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        None,
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    let encoded_token = ctx.encode_claimset(&claims).ok().unwrap();
    let token = ctx.decode_and_validate_text(&encoded_token.text).ok().unwrap();
    let res = check_authorizations(
        |rn: &ResourceName, perms: &Permissions| *rn == ResourceName(String::from("resource-1")) && perms.0.contains(&String::from("grant")),
        &token,
    );
    assert!(res);
}

#[test]
fn rejects_a_token_with_the_incorrect_permissions() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        None,
        ResourceName(String::from("resource-1")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write")]),
    );
    ctx.add_claimset(claims.clone());
    let encoded_token = ctx.encode_claimset(&claims).ok().unwrap();
    let token = ctx.decode_and_validate_text(&encoded_token.text).ok().unwrap();
    let res = check_authorizations(
        |rn: &ResourceName, perms: &Permissions| *rn == ResourceName(String::from("resource-1")) && perms.0.contains(&String::from("grant")),
        &token,
    );
    assert!(!res);
}

#[test]
fn rejects_a_token_with_the_incorrect_resource_name() {
    let mut ctx = OrizenticCtx::new_ctx(Secret("ctx".to_string().into_bytes()), Vec::new());
    let claims = create_claimset(
        Issuer(String::from("test")),
        None,
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );
    ctx.add_claimset(claims.clone());
    let encoded_token = ctx.encode_claimset(&claims).ok().unwrap();
    let token = ctx.decode_and_validate_text(&encoded_token.text).ok().unwrap();
    let res = check_authorizations(
        |rn: &ResourceName, perms: &Permissions| *rn == ResourceName(String::from("resource-1")) && perms.0.contains(&String::from("grant")),
        &token,
    );
    assert!(!res);
}

#[test]
fn claims_serialize_to_json() {
    let claims = create_claimset(
        Issuer(String::from("test")),
        None,
        ResourceName(String::from("resource-2")),
        Username(String::from("Savanni")),
        Permissions(vec![String::from("read"), String::from("write"), String::from("grant")]),
    );

    let expected_jti = format!("\"jti\":\"{}\"", claims.id);

    let claim_str = to_json(&claims)
        .expect("to_json threw an error");
        //.expect(assert!(false, format!("[claims_serilazie_to_json] {}", err)));
    assert!(claim_str.contains(&expected_jti));

    let claims_ = from_json(claim_str)
        .expect("from_json threw an error");
    assert_eq!(claims, claims_);
}

