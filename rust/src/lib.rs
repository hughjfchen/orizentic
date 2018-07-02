extern crate yaml_rust;
extern crate chrono;
//extern crate jsonwebtoken as jwt;
extern crate uuid;

use chrono::prelude::*;
use uuid::Uuid;
use std::collections::HashMap;

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct ResourceName(pub String);

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Permissions(pub Vec<String>);

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Issuer(pub String);

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct TTL(pub chrono::Duration);

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Username(pub String);

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct Secret(pub String);

#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Clone)]
pub struct ClaimSet {
    pub id: String,
    pub audience: Username,
    pub expiration: Option<DateTime<Utc>>,
    pub issuer: Issuer,
    pub issued_at: DateTime<Utc>,
    pub resource: ResourceName,
    pub permissions: Permissions,
}

pub struct OrizenticCtx(Secret, HashMap<String, ClaimSet>);

impl OrizenticCtx {
    pub fn new_ctx(secret: Secret, claims_lst: Vec<ClaimSet>) -> OrizenticCtx {
        OrizenticCtx(secret, HashMap::new())
    }

    //pub fn validate_token
    //pub fn check_authorizations

    pub fn create_claims(&mut self,
                         issuer: Issuer,
                         ttl: Option<TTL>,
                         resource_name: ResourceName,
                         user_name: Username,
                         perms: Permissions) -> ClaimSet {
        let issued_at: DateTime<Utc> = Utc::now();
        let expiration = match ttl {
            Some(TTL(ttl_)) => issued_at.checked_add_signed(ttl_),
            None => None,
        };
        let claimset = ClaimSet{
            id: String::from(Uuid::new_v4().hyphenated().to_string()),
            audience: user_name,
            expiration,
            issuer,
            issued_at,
            resource: resource_name,
            permissions: perms,
        };
        let clr = claimset.clone();
        self.1.insert(claimset.id.clone(), claimset);
        clr
    }

    pub fn revoke_claims(&mut self, claim: &ClaimSet) { }

    pub fn revoke_by_uuid(&self, claim_id: &String) { }

    pub fn replace_claims(&self, claims_lst: Vec<ClaimSet>) { }

    pub fn list_claims(&self) -> Vec<String> {
        self.1.keys().map(| id | id.clone()).collect()
    }

    pub fn find_claims(&self, claims_id: String) -> Option<&ClaimSet> {
        None
    }

    pub fn encode_claims(&self, claims: &ClaimSet) -> String {
        "".to_string()
    }
}

