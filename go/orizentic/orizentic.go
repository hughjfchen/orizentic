package orizentic

import (
	"errors"
	"fmt"
	"time"

	"github.com/dgrijalva/jwt-go"
	"github.com/google/uuid"
)

var _ = fmt.Println

type OrizenticCtx struct {
	secret Secret
	store  map[string]ClaimSet
}

func NewContext(s Secret, claims map[string]ClaimSet) OrizenticCtx {
	return OrizenticCtx{s, claims}
}

func (self *OrizenticCtx) ValidateToken(token UnverifiedToken) (VerifiedToken, error) {
	return self.DecodeAndValidateToken(token.token().Raw)
}

func (self *OrizenticCtx) DecodeAndValidateToken(token string) (VerifiedToken, error) {
	nullClaims := ClaimSet{}
	null := VerifiedToken{jwt.Token{}, nullClaims}

	t, err := DecodeAndValidate(token, self.secret)

	if err != nil {
		err_ := err.(*jwt.ValidationError)
		if err_.Errors&jwt.ValidationErrorSignatureInvalid == jwt.ValidationErrorSignatureInvalid {
			return null, InvalidToken(token)
		}
		if err_.Errors&jwt.ValidationErrorExpired == jwt.ValidationErrorExpired {
			return null, ExpiredToken(token)
		}
		return null, err
	}

	_, ok := self.store[t.claims().id]
	if !ok {
		return null, UnknownToken(t.claims().id)
	}
	return t, nil
}

func (self *OrizenticCtx) CreateClaims(issuer Issuer, ttl *TTL, resource ResourceName, user Username, perms PermissionList) (ClaimSet, error) {
	now := time.Now()
	id := uuid.New()

	exp := int64(0)
	if ttl != nil {
		ttl_ := time.Duration(*ttl)
		exp = now.Unix() + int64(ttl_.Seconds())
	}

	/*
		claims := jwt.MapClaims{
			"aud":   string(user),
			"exp":   exp,
			"jti":   id.String(),
			"iat":   now.Unix(),
			"iss":   string(issuer),
			"sub":   string(resource),
			"perms": []string(perms),
		}
	*/
	claims := ClaimSet{
		id:          id.String(),
		audience:    user,
		expiration:  exp,
		issuer:      issuer,
		resource:    resource,
		permissions: perms,
	}
	self.store[id.String()] = claims
	return claims, nil
}

func (self *OrizenticCtx) RevokeClaims(claims ClaimSet) error {
	delete(self.store, claims.id)
	return nil
}

func (self *OrizenticCtx) RevokeByUUID(id string) error {
	delete(self.store, id)
	return nil
}

func ReplaceClaims() error {
	return errors.New("Not implemented")
}

func (self *OrizenticCtx) ListClaims() ([]ClaimSet, error) {
	lst := make([]ClaimSet, len(self.store))
	i := 0
	for _, v := range self.store {
		lst[i] = v
		i++
	}
	return lst, nil
}

func FindClaims() error {
	return errors.New("Not implemented")
}

func (self *OrizenticCtx) EncodeClaims(claims ClaimSet) (string, error) {
	claims_ := claims.ToJWT()
	t := jwt.NewWithClaims(jwt.SigningMethodHS256, claims_)
	return t.SignedString([]byte(self.secret))
}

func CheckAuthorizations(token VerifiedToken, f func(ResourceName, PermissionList) bool) bool {
	resourceName := token.claims().resource
	permissions := token.claims().permissions
	return f(resourceName, permissions)
}
