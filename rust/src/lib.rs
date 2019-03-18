//! The Orizentic token management library
//!
//! This library provides a high level interface for authentication token management.  It wraps
//! around the [JWT](https://jwt.io/) standard using the
//! [`jsonwebtoken`](https://github.com/Keats/jsonwebtoken) library for serialization and
//! validation. 
//!
//! Functionality revolves around the relationship between a [ClaimSet](struct.ClaimSet.html), a
//! [VerifiedToken](struct.VerifiedToken.html), and an
//! [UnverifiedToken](struct.UnverifiedToken.html). A [ClaimSet](struct.ClaimSet.html) is
//! considered informative and stores all of the information about the permissions and resources
//! that the token bearer should have access to.  [VerifiedToken](struct.VerifiedToken.html) and
//! [UnverifiedToken](struct.UnverifiedToken.html) are the result of the process of decoding a
//! string JWT, and inherently specify whether the decoding process verified the signature,
//! expiration time, and presence in the database.
//!
//! This library does not currently contain database save and load features, but those are a likely
//! upcoming feature.
//!
//! No setup is necessary when using this library to decode JWT strings. Refer to the standalone
//! [decode_text](fn.decode_text.html) function.

#[macro_use]
extern crate serde_derive;

pub use core::*;

mod core;
pub mod filedb;

