package orizentic

import (
	"errors"
	"fmt"
	"github.com/dgrijalva/jwt-go"
	"reflect"
)

var _ = fmt.Println

type UnverifiedToken struct {
	token_  jwt.Token
	claims_ ClaimSet
}

func (self UnverifiedToken) token() jwt.Token { return self.token_ }
func (self UnverifiedToken) claims() ClaimSet { return self.claims_ }

type VerifiedToken struct {
	token_  jwt.Token
	claims_ ClaimSet
}

func (self VerifiedToken) token() jwt.Token { return self.token_ }
func (self VerifiedToken) claims() ClaimSet { return self.claims_ }

func DecodeAndValidate(token string, secret Secret) (VerifiedToken, error) {
	null := VerifiedToken{jwt.Token{}, ClaimSet{}}

	res, err := jwt.Parse(token, func(*jwt.Token) (interface{}, error) {
		return []byte(secret), nil
	})

	if err != nil {
		return null, err
	}

	if _, ok := res.Method.(*jwt.SigningMethodHMAC); !ok {
		return null, errors.New("incorrect signing method")
	}

	claims, err := ClaimsetFromJWT(res.Claims)
	return VerifiedToken{*res, claims}, nil
}

func Decode(token string) (UnverifiedToken, error) {
	null := UnverifiedToken{jwt.Token{}, ClaimSet{}}

	res, err := jwt.Parse(token, func(token *jwt.Token) (interface{}, error) {
		return nil, nil
	})

	switch err_ := err.(type) {
	case *jwt.ValidationError:
		if err_.Errors&jwt.ValidationErrorSignatureInvalid == jwt.ValidationErrorSignatureInvalid {
			claims, err := ClaimsetFromJWT(res.Claims)
			return UnverifiedToken{*res, claims}, err
		}
	}

	return null, err
}

type ClaimSet struct {
	id          string
	audience    Username
	expiration  int64
	issuer      Issuer
	issuedAt    int64
	resource    ResourceName
	permissions []string
}

func ClaimsetFromJWT(jwtClaims jwt.Claims) (ClaimSet, error) {
	var claims ClaimSet

	switch jwtClaims_ := jwtClaims.(type) {
	case (jwt.StandardClaims):
		return ClaimSet{}, errors.New("StandardClaims are not handled")
	case (jwt.MapClaims):
		claims.id = jwtClaims_["jti"].(string)
		claims.audience = Username(jwtClaims_["aud"].(string))
		if val, ok := jwtClaims_["exp"].(float64); ok {
			claims.expiration = int64(val)
		}
		claims.issuer = Issuer(jwtClaims_["iss"].(string))

		if val, ok := jwtClaims_["iat"].(float64); ok {
			claims.issuedAt = int64(val)
		}
		claims.resource = ResourceName(jwtClaims_["sub"].(string))

		switch perms := jwtClaims_["perms"].(type) {
		case []interface{}:
			claims.permissions = make([]string, len(perms))
			for i, v := range perms {
				claims.permissions[i] = v.(string)
			}
		case []string:
			claims.permissions = make([]string, len(perms))
			for i, v := range perms {
				claims.permissions[i] = v
			}
		}
		return claims, nil
	}
	return ClaimSet{}, errors.New(fmt.Sprintf("Unknown jwt claimset type: %v", reflect.TypeOf(jwtClaims)))
}

func (self ClaimSet) ToJWT() jwt.MapClaims {
	claims := jwt.MapClaims{
		"aud":   string(self.audience),
		"exp":   self.expiration,
		"jti":   self.id,
		"iat":   self.issuedAt,
		"iss":   string(self.issuer),
		"sub":   string(self.resource),
		"perms": self.permissions,
	}

	return claims
}

func (self ClaimSet) Id() string {
	return self.id
}

func (self ClaimSet) Audience() Username {
	return self.audience
}

func (self ClaimSet) Expiration() int64 {
	return self.expiration
}

func (self ClaimSet) Issuer() Issuer {
	return self.issuer
}

func (self ClaimSet) IssuedAt() int64 {
	return self.issuedAt
}

func (self ClaimSet) ResourceName() ResourceName {
	return self.resource
}

func (self ClaimSet) Permissions() PermissionList {
	return self.permissions
}

/* This is technically unnecessary as standard equality works, however it is helpful for debugging when something inevitably goes wrong. */
//func (self MapClaims) Equals(other MapClaims) bool {
//for k, v := range self {
//val, ok := other[k]
//if !ok {
//return false
//}
//switch val_ := val.(type) {
//case []string:
//v_ := v.([]string)
//if len(v_) != len(val_) {
//return false
//}
//for i, _ := range v_ {
//if v_[i] != val_[i] {
//return false
//}
//}
//default:
//if v != val {
//return false
//}
//}
//}
//return true
//}
