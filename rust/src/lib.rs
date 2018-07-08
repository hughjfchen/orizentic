#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate yaml_rust;
extern crate chrono;
extern crate jsonwebtoken as jwt;
extern crate uuid;

use chrono::prelude::*;
use uuid::Uuid;
use std::collections::HashMap;

pub enum Error {
    JWTError(jwt::errors::Error),
    UnknownToken(),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ResourceName(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct Permissions(pub Vec<String>);

#[derive(Debug, PartialEq, Clone)]
pub struct Issuer(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct TTL(pub chrono::Duration);

#[derive(Debug, PartialEq, Clone)]
pub struct Username(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct Secret(pub Vec<u8>);

#[derive(Debug, PartialEq, Clone)]
pub struct ClaimSet {
    pub id: String,
    pub audience: Username,
    pub expiration: Option<DateTime<Utc>>,
    pub issuer: Issuer,
    pub issued_at: DateTime<Utc>,
    pub resource: ResourceName,
    pub permissions: Permissions,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ClaimSetJS {
    jti: String,
    aud: String,
    exp: Option<i64>,
    iss: String,
    iat: i64,
    sub: String,
    perms: Vec<String>,
}

impl ClaimSetJS {
    fn from_claimset(claims: &ClaimSet) -> ClaimSetJS {
        ClaimSetJS{
            jti: claims.id.clone(),
            aud: claims.audience.0.clone(),
            exp: claims.expiration.map(|t| t.timestamp()),
            iss: claims.issuer.0.clone(),
            iat: claims.issued_at.timestamp(),
            sub: claims.resource.0.clone(),
            perms: claims.permissions.0.clone(),
        }
    }

    fn to_claimset(&self) -> ClaimSet {
        ClaimSet{
            id: self.jti.clone(),
            audience: Username(self.aud.clone()),
            expiration: self.exp.map(|t| Utc.timestamp(t, 0)),
            issuer: Issuer(self.iss.clone()),
            issued_at: Utc.timestamp(self.iat, 0),
            resource: ResourceName(self.sub.clone()),
            permissions: Permissions(self.perms.clone()),
        }
    }
}

pub struct OrizenticCtx(Secret, HashMap<String, ClaimSet>);

#[derive(Debug)]
pub struct UnverifiedToken {
    pub text: String,
    pub claims: ClaimSet,
}

#[derive(Debug)]
pub struct VerifiedToken {
    pub text: String,
    pub claims: ClaimSet,
}

impl OrizenticCtx {
    pub fn new_ctx(secret: Secret, claims_lst: Vec<ClaimSet>) -> OrizenticCtx {
        OrizenticCtx(secret, HashMap::new())
    }

    pub fn decode_text(&self, text: &String) -> Result<UnverifiedToken, Error> {
        let validation = jwt::Validation{
            leeway: 0,
            validate_exp: false,
            validate_iat: false,
            validate_nbf: false,
            aud: None,
            iss: None,
            sub: None,
            ..jwt::Validation::default()
        };
        let res = jwt::decode::<ClaimSetJS>(text, &(self.0).0, &validation);
        match res {
            Ok(res_) => Ok(UnverifiedToken{text: text.clone(), claims: res_.claims.to_claimset()}),
            Err(err) => Err(Error::JWTError(err)),
        }
    }

    pub fn validate_token(&self, token: &UnverifiedToken) -> Result<VerifiedToken, Error> {
        let validator = match token.claims.expiration {
            Some(_) => jwt::Validation::default(),
            None => jwt::Validation{ validate_exp: false, ..jwt::Validation::default() },
        };
        let res = jwt::decode::<ClaimSetJS>(&token.text, &(self.0).0, &validator);
        match res {
            Ok(res_) => {
                let claims = res_.claims;
                let in_db = self.1.get(&claims.jti);
                if in_db.is_some() {
                    Ok(VerifiedToken{text: token.text.clone(), claims: claims.to_claimset()})
                } else {
                    Err(Error::UnknownToken())
                }
            },
            Err(err) => Err(Error::JWTError(err)),
        }
    }

    pub fn decode_and_validate_text(&self, text: &String) -> Result<VerifiedToken, Error> {
        match self.decode_text(text) {
            Ok(unverified) => self.validate_token(&unverified),
            Err(err) => Err(err),
        }
    }

    pub fn add_claimset(&mut self, claimset: ClaimSet) {
        self.1.insert(claimset.id.clone(), claimset);
    }

    pub fn revoke_claimset(&mut self, claim: &ClaimSet) {
        self.1.remove(&claim.id);
    }

    pub fn revoke_by_uuid(&mut self, claim_id: &String) {
        self.1.remove(claim_id);
    }

    pub fn replace_claimsets(&mut self, claims_lst: Vec<ClaimSet>) { }

    pub fn list_claimsets(&self) -> Vec<String> {
        self.1.keys().map(| id | id.clone()).collect()
    }

    pub fn find_claimset(&self, claims_id: &String) -> Option<&ClaimSet> {
        self.1.get(claims_id)
    }

    pub fn encode_claimset(&self, claims: &ClaimSet) -> Result<VerifiedToken, Error> {
        let in_db = self.1.get(&claims.id);
        if in_db.is_some() {
            let text = jwt::encode(&jwt::Header::default(), &ClaimSetJS::from_claimset(&claims), &(self.0).0);
            match text {
                Ok(text_) => 
                    Ok(VerifiedToken{
                        text: text_,
                        claims: claims.clone(),
                    }),
                Err(err) => Err(Error::JWTError(err)),
            }
        } else {
            Err(Error::UnknownToken())
        }
    }
}

pub fn check_authorizations<F: FnOnce(&ResourceName, &Permissions) -> bool>(f: F, token: &VerifiedToken) -> bool {
    f(&token.claims.resource, &token.claims.permissions)
}

pub fn create_claimset(issuer: Issuer,
                       ttl: Option<TTL>,
                       resource_name: ResourceName,
                       user_name: Username,
                       perms: Permissions) -> ClaimSet {
    let issued_at: DateTime<Utc> = Utc::now();
    let expiration = match ttl {
        Some(TTL(ttl_)) => issued_at.checked_add_signed(ttl_),
        None => None,
    };
    ClaimSet{
        id: String::from(Uuid::new_v4().hyphenated().to_string()),
        audience: user_name,
        expiration,
        issuer,
        issued_at,
        resource: resource_name,
        permissions: perms,
    }
}

