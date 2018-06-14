package orizentic_test

import (
	"github.com/dgrijalva/jwt-go"
	. "github.com/luminescent-dreams/orizentic/go/orizentic"

	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"
)

var claim = []byte("{\"sub\":\"t-minus\",\"jti\":\"3befd2e9-1442-4f83-bebb-31c07c40071c\",\"iss\":\"Savanni\",\"iat\":1499575581,\"aud\":\"Aria\",\"perms\":[\"\"]}")

var db = []byte("[{\"sub\":\"t-minus\",\"jti\":\"3befd2e9-1442-4f83-bebb-31c07c40071c\",\"iss\":\"Savanni\",\"iat\":1499575581,\"aud\":\"Aria\",\"perms\":[\"\"]},{\"sub\":\"t-minus\",\"jti\":\"b81c7fd7-77be-48d4-884b-5f3798a0db8e\",\"iss\":\"Savanni\",\"iat\":1499575577,\"aud\":\"Leah\",\"perms\":[\"\"]},{\"sub\":\"t-minus\",\"jti\":\"582bd111-9421-43b4-aa43-8e96e228c34b\",\"iss\":\"Savanni\",\"iat\":1499575572,\"aud\":\"Cait\",\"perms\":[\"\"]},{\"sub\":\"t-minus\",\"jti\":\"b71c75ad-1f70-4206-8335-7ce45be63522\",\"iss\":\"Savanni\",\"iat\":1499575569,\"aud\":\"Amelia\",\"perms\":[\"\"]},{\"sub\":\"t-minus\",\"jti\":\"e202fa23-cba7-492f-bfe6-67b1c7ad93ae\",\"iss\":\"Savanni\",\"iat\":1499575564,\"aud\":\"Daria\",\"perms\":[\"\"]},{\"sub\":\"t-minus\",\"jti\":\"e5628561-2ec6-4e2f-b655-5175bf2f4d3c\",\"iss\":\"Savanni\",\"iat\":1499575556,\"aud\":\"Savanni\",\"perms\":[\"\"]}]")

var _ = Describe("Parser", func() {
	It("properly compares two equivalent claims", func() {
		expectedClaims, err := ClaimsetFromJWT(jwt.MapClaims{
			"aud":   "Aria",
			"jti":   "3befd2e9-1442-4f83-bebb-31c07c40071c",
			"iat":   1499575581,
			"iss":   "Savanni",
			"sub":   "t-minus",
			"perms": []string{""},
		})
		Expect(err).To(BeNil())

		exactMatch, err := ClaimsetFromJWT(jwt.MapClaims{
			"aud":   "Aria",
			"jti":   "3befd2e9-1442-4f83-bebb-31c07c40071c",
			"iat":   1499575581,
			"iss":   "Savanni",
			"sub":   "t-minus",
			"perms": []string{""},
		})
		Expect(err).To(BeNil())

		reorderedMatch, err := ClaimsetFromJWT(jwt.MapClaims{
			"jti":   "3befd2e9-1442-4f83-bebb-31c07c40071c",
			"aud":   "Aria",
			"iat":   1499575581,
			"iss":   "Savanni",
			"sub":   "t-minus",
			"perms": []string{""},
		})
		Expect(err).To(BeNil())

		Expect(exactMatch).To(Equal(expectedClaims))
		Expect(reorderedMatch).To(Equal(expectedClaims))
	})

	It("should parse a normal claim", func() {
		expectedClaims, err := ClaimsetFromJWT(jwt.MapClaims{
			"aud":   "Aria",
			"jti":   "3befd2e9-1442-4f83-bebb-31c07c40071c",
			"iat":   float64(1499575581),
			"iss":   "Savanni",
			"sub":   "t-minus",
			"perms": []string{""},
		})
		Expect(err).To(BeNil())

		unexpectedClaims, err := ClaimsetFromJWT(jwt.MapClaims{
			"aud":   "Aria",
			"jti":   "3befd2e9-1442-4f83-bebb-31c07c40071c",
			"iat":   float64(1499575581),
			"iss":   "Savanni",
			"sub":   "t-minus",
			"perms": []string{"read"},
		})
		Expect(err).To(BeNil())

		claimSet, err := ParseClaim(claim)
		Expect(err).To(BeNil())

		Expect(claimSet).To(Equal(expectedClaims))
		Expect(claimSet).ToNot(Equal(unexpectedClaims))
	})

	It("should parse a database file", func() {
		claims, err := ParseClaims(db)

		Expect(err).To(BeNil())

		claimset, ok := claims["3befd2e9-1442-4f83-bebb-31c07c40071c"]
		Expect(ok).To(BeTrue())
		Expect(claimset.Audience()).To(Equal(Username("Aria")))

		claimset, ok = claims["b81c7fd7-77be-48d4-884b-5f3798a0db8e"]
		Expect(ok).To(BeTrue())
		Expect(claimset.Audience()).To(Equal(Username("Leah")))

		claimset, ok = claims["e202fa23-cba7-492f-bfe6-67b1c7ad93ae"]
		Expect(ok).To(BeTrue())
		Expect(claimset.Audience()).To(Equal(Username("Daria")))
	})
})
