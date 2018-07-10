extern crate serde;
extern crate serde_json;
extern crate yaml_rust;
extern crate chrono;
extern crate jsonwebtoken as jwt;
extern crate uuid;

use core::chrono::prelude::*;
use core::uuid::Uuid;
use std::collections::HashMap;

/// Orizentic Errors
pub enum Error {
    /// An underlying JWT decoding error. May be replaced with Orizentic semantic errors to better
    /// encapsulate the JWT library.
    JWTError(jwt::errors::Error),
    /// Token decoded and verified but was not present in the database.
    UnknownToken(),
}

/// ResourceName is application-defined and names a resource to which access should be controlled
#[derive(Debug, PartialEq, Clone)]
pub struct ResourceName(pub String);

/// Permissions are application-defined descriptions of what can be done with the named resource
#[derive(Debug, PartialEq, Clone)]
pub struct Permissions(pub Vec<String>);

/// Issuers are typically informative, but should generally describe who or what created the token
#[derive(Debug, PartialEq, Clone)]
pub struct Issuer(pub String);

/// Time to live is the number of seconds until a token expires. This is used for creating tokens
/// but tokens store their actual expiration time.
#[derive(Debug, PartialEq, Clone)]
pub struct TTL(pub chrono::Duration);

/// Username, or Audience in JWT terms, should describe who or what is supposed to be using this
/// token
#[derive(Debug, PartialEq, Clone)]
pub struct Username(pub String);

#[derive(Debug, PartialEq, Clone)]
pub struct Secret(pub Vec<u8>);

/// A ClaimSet represents one set of permissions and claims. It is a standardized way of specifying
/// the owner, issuer, expiration time, relevant resources, and specific permissions on that
/// resource. By itself, this is only an informative data structure and so should never be trusted
/// when passed over the wire. See `VerifiedToken` and `UnverifiedToken`.
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

/// ClaimSetJS is an intermediary data structure between JWT serialization and a more usable
/// ClaimSet.
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
struct ClaimSetJS {
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

/// The Orizentic Context encapsulates a set of claims and an associated secret. This provides the
/// overall convenience of easily creating and validating tokens. Generated claimsets are stored
/// here on the theory that, even with validation, only those claims actually stored in the
/// database should be considered valid.
pub struct OrizenticCtx(Secret, HashMap<String, ClaimSet>);

/// An UnverifiedToken is a combination of the JWT serialization and the decoded `ClaimSet`. As this
/// is unverified, this should only be used for information purposes, such as determining what a
/// user can do with a token even when the decoding key is absent.
#[derive(Debug)]
pub struct UnverifiedToken {
    pub text: String,
    pub claims: ClaimSet,
}

/// An VerifiedToken is a combination of the JWT serialization and the decoded `ClaimSet`. This will
/// only be created by the `validate_function`, and thus will represent a token which has been
/// validated via signature, expiration time, and presence in the database.
#[derive(Debug)]
pub struct VerifiedToken {
    pub text: String,
    pub claims: ClaimSet,
}

impl OrizenticCtx {
    /// Create a new Orizentic Context with an initial set of claims.
    pub fn new_ctx(secret: Secret, claims_lst: Vec<ClaimSet>) -> OrizenticCtx {
        OrizenticCtx(secret, HashMap::new())
    }

    /// Validate a token by checking its signature, that it is not expired, and that it is still
    /// present in the database. Return an error if any check fails, but return a `VerifiedToken`
    /// if it all succeeds.
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

    /// Given a text string, as from a web application's `Authorization` header, decode the string
    /// and then validate the token.
    pub fn decode_and_validate_text(&self, text: &String) -> Result<VerifiedToken, Error> {
        // it is necessary to first decode the token because we need the validator to know whether
        // to attempt to validate the expiration. Without that check, the validator will fail any
        // expiration set to None.
        match decode_text(text) {
            Ok(unverified) => self.validate_token(&unverified),
            Err(err) => Err(err),
        }
    }

    /// Add a claimset to the database.
    pub fn add_claimset(&mut self, claimset: ClaimSet) {
        self.1.insert(claimset.id.clone(), claimset);
    }

    /// Remove a claims set from the database so that all additional validation checks fail.
    pub fn revoke_claimset(&mut self, claim: &ClaimSet) {
        self.1.remove(&claim.id);
    }

    /// Revoke a ClaimsSet given its ID, which is set in the `jti` claim of a JWT or the `id` field
    /// of a `ClaimSet`.
    pub fn revoke_by_uuid(&mut self, claim_id: &String) {
        self.1.remove(claim_id);
    }

    /// *NOT IMPLEMENTED*
    pub fn replace_claimsets(&mut self, claims_lst: Vec<ClaimSet>) { }

    /// List all of the `ClaimSet` IDs in the database.
    pub fn list_claimsets(&self) -> Vec<&ClaimSet> {
        self.1.values().map(| item | item).collect()
    }

    /// Find a `ClaimSet` by ID.
    pub fn find_claimset(&self, claims_id: &String) -> Option<&ClaimSet> {
        self.1.get(claims_id)
    }

    /// Encode and sign a claimset, returning the result as a `VerifiedToken`.
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

/// Create a new `ClaimSet`. This will return a claimset with the expiration time calculated from
/// the TTL if the TTL is provided. No expiration will be set if no TTL is provided.
pub fn create_claimset(issuer: Issuer,
                       ttl: Option<TTL>,
                       resource_name: ResourceName,
                       user_name: Username,
                       perms: Permissions) -> ClaimSet {
    let issued_at: DateTime<Utc> = Utc::now().with_nanosecond(0).unwrap();
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

/// Decode a JWT text string without verification
pub fn decode_text(text: &String) -> Result<UnverifiedToken, Error> {
    let res = jwt::dangerous_unsafe_decode::<ClaimSetJS>(text);
    match res {
        Ok(res_) => Ok(UnverifiedToken{text: text.clone(), claims: res_.claims.to_claimset()}),
        Err(err) => Err(Error::JWTError(err)),
    }
}

/// Given a `VerifiedToken`, pass the resource name and permissions to a user-defined function. The
/// function should return true if the caller should be granted access to the resource and false,
/// otherwise. That result will be passed back to the caller.
pub fn check_authorizations<F: FnOnce(&ResourceName, &Permissions) -> bool>(f: F, token: &VerifiedToken) -> bool {
    f(&token.claims.resource, &token.claims.permissions)
}


pub fn to_json(claims: &ClaimSet) -> Result<String, serde_json::Error> {
    serde_json::to_string(&(ClaimSetJS::from_claimset(claims)))
}

pub fn from_json(text: &String) -> Result<ClaimSet, serde_json::Error> {
    serde_json::from_str(&text)
        .map(|x| ClaimSetJS::to_claimset(&x))
}

