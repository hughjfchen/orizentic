package orizentic_test

import (
	"fmt"
	//"github.com/dgrijalva/jwt-go"
	//"reflect"
	"time"

	. "github.com/luminescent-dreams/orizentic/go/orizentic"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var _ = fmt.Printf

var oneHourTTL = TTL(time.Duration(1 * time.Hour))

var _ = Describe("Orizentic Unit Tests", func() {
	It("can create a new claimset", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))

		claims1, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())
		claims2, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-2"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		tokList, err := ctx.ListClaims()
		Expect(err).To(BeNil())

		Expect(len(tokList)).To(Equal(2))
		Expect(tokList).To(ContainElement(claims1))
		Expect(tokList).To(ContainElement(claims2))
	})

	It("can revoke a claimset", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))

		claims1, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())
		claims2, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-2"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())
		err = ctx.RevokeClaims(claims1)
		Expect(err).To(BeNil())

		tokList, err := ctx.ListClaims()

		Expect(len(tokList)).To(Equal(1))
		Expect(tokList).ToNot(ContainElement(claims1))
		Expect(tokList).To(ContainElement(claims2))
	})

	It("rejects tokens with an invalid secret", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		ctx2 := NewContext(Secret([]byte("ctx2")), make(map[string]ClaimSet))
		claims1, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims1)
		Expect(err).To(BeNil())

		_, err = ctx2.DecodeAndValidateToken(bytes)
		_, ok := err.(InvalidToken)
		Expect(ok).To(BeTrue())
	})

	It("rejects tokens that are absent from the database", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		claims1, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims1)
		Expect(err).To(BeNil())

		ctx.RevokeClaims(claims1)

		_, err = ctx.DecodeAndValidateToken(bytes)
		_, ok := err.(UnknownToken)
		Expect(ok).To(BeTrue())
	})

	It("validates present tokens with a valid secret", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		claims1, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims1)
		Expect(err).To(BeNil())

		_, err = ctx.DecodeAndValidateToken(bytes)
		Expect(err).To(BeNil())
	})

	It("decodes tokens without validating the secret", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		claims, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims)
		Expect(err).To(BeNil())

		_, err = Decode(bytes)
		Expect(err).To(BeNil())
	})

	It("rejects expired tokens", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		oneSecondTTL := TTL(time.Duration(1 * time.Second))
		claims, err := ctx.CreateClaims(Issuer("test"),
			&oneSecondTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		time.Sleep(time.Duration(2 * time.Second))

		bytes, err := ctx.EncodeClaims(claims)
		Expect(err).To(BeNil())

		_, err = ctx.DecodeAndValidateToken(bytes)
		_, errOk := err.(ExpiredToken)
		Expect(errOk).To(BeTrue())
	})

	It("accepts tokens that have no expiration", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		claims, err := ctx.CreateClaims(Issuer("test"),
			nil,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims)
		Expect(err).To(BeNil())

		_, err = ctx.DecodeAndValidateToken(bytes)
		Expect(err).To(BeNil())
	})

	It("authorizes a token with the correct resource and permissions", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		claims, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims)
		Expect(err).To(BeNil())

		token, err := ctx.DecodeAndValidateToken(bytes)
		Expect(err).To(BeNil())

		authorized := CheckAuthorizations(
			token,
			func(rn ResourceName, perms PermissionList) bool {
				if rn != ResourceName("resource-1") {
					return false
				}
				for _, v := range perms {
					if v == "read" {
						return true
					}
				}
				return false
			})

		Expect(authorized).To(BeTrue())
	})

	It("rejects a token with the incorrect permissions", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		claims, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims)
		Expect(err).To(BeNil())

		token, err := ctx.DecodeAndValidateToken(bytes)
		Expect(err).To(BeNil())

		authorized := CheckAuthorizations(
			token,
			func(rn ResourceName, perms PermissionList) bool {
				if rn != ResourceName("resource-1") {
					return false
				}
				for _, v := range perms {
					if v == "delete" {
						return true
					}
				}
				return false
			})

		Expect(authorized).To(BeFalse())
	})

	It("rejects a token with the incorrect source name", func() {
		ctx := NewContext(Secret([]byte("ctx")), make(map[string]ClaimSet))
		claims, err := ctx.CreateClaims(Issuer("test"),
			&oneHourTTL,
			ResourceName("resource-1"),
			Username("Savanni"),
			PermissionList([]string{"read", "write", "grant"}))
		Expect(err).To(BeNil())

		bytes, err := ctx.EncodeClaims(claims)
		Expect(err).To(BeNil())

		token, err := ctx.DecodeAndValidateToken(bytes)
		Expect(err).To(BeNil())

		authorized := CheckAuthorizations(
			token,
			func(rn ResourceName, perms PermissionList) bool {
				if rn != ResourceName("resource") {
					return false
				}
				for _, v := range perms {
					if v == "read" {
						return true
					}
				}
				return false
			})

		Expect(authorized).To(BeFalse())
	})
})
