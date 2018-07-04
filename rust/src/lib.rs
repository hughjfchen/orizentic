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

    pub fn decode_text(&self, text: &String) -> Option<UnverifiedToken> {
        let validation = jwt::Validation{
            leeway: 0,
            validate_exp: false,
            validate_iat: false,
            validate_nbf: false,
            aud: None,
            iss: None,
            sub: None,
            algorithms: Vec::new(),
        };
        let res = jwt::decode::<ClaimSetJS>(text, &(self.0).0, &jwt::Validation::new(jwt::Algorithm::HS256));
        match res {
            Ok(res_) => {
                let claims = res_.claims;
                let in_db = self.1.get(&claims.jti);
                if in_db.is_some() {
                    Some(UnverifiedToken{text: text.clone(), claims: claims.to_claimset()})
                } else {
                    None
                }
            },
            Err(err) => None,
        }
    }

    pub fn validate_token(&self, token: &UnverifiedToken) -> Option<VerifiedToken> {
        self.decode_and_validate_text(&token.text)
    }

    pub fn decode_and_validate_text(&self, text: &String) -> Option<VerifiedToken> {
        let res = jwt::decode::<ClaimSetJS>(text, &(self.0).0, &jwt::Validation::new(jwt::Algorithm::HS256));
        match res {
            Ok(res_) => {
                let claims = res_.claims;
                let in_db = self.1.get(&claims.jti);
                if in_db.is_some() {
                    Some(VerifiedToken{text: text.clone(), claims: claims.to_claimset()})
                } else {
                    None
                }
            },
            Err(err) => None,
        }
    }
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

    pub fn revoke_claims(&mut self, claim: &ClaimSet) {
        self.1.remove(&claim.id);
    }

    pub fn revoke_by_uuid(&self, claim_id: &String) { }

    pub fn replace_claims(&self, claims_lst: Vec<ClaimSet>) { }

    pub fn list_claims(&self) -> Vec<String> {
        self.1.keys().map(| id | id.clone()).collect()
    }

    pub fn find_claims(&self, claims_id: String) -> Option<&ClaimSet> {
        None
    }

    pub fn encode_claims(&self, claims: &ClaimSet) -> Result<String, jwt::errors::Error> {
        jwt::encode(&jwt::Header::default(), &ClaimSetJS::from_claimset(&claims), &(self.0).0)
    }
}

