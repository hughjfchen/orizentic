package orizentic

import (
	"github.com/luminescent-dreams/gaeson"
)

func FormatClaims(claimsdb []ClaimSet) []byte {
	js := make([]gaeson.JSValue, len(claimsdb))
	for i, claimset := range claimsdb {
		js[i] = FormatClaimset(claimset)
	}

	return gaeson.ToJSONText(gaeson.JSArray(js))
}

func FormatClaimset(claims ClaimSet) gaeson.JSValue {
	clMap := make(map[string]gaeson.JSValue)
	clMap["jti"] = gaeson.JSString(claims.id)
	clMap["aud"] = gaeson.JSString(string(claims.audience))
	clMap["exp"] = gaeson.JSNumber(claims.expiration)
	clMap["iss"] = gaeson.JSString(string(claims.issuer))
	clMap["iat"] = gaeson.JSNumber(claims.issuedAt)
	clMap["sub"] = gaeson.JSString(string(claims.resource))

	permsArr := make([]gaeson.JSValue, len(claims.permissions))
	for i, perm := range claims.permissions {
		permsArr[i] = gaeson.JSString(perm)
	}
	clMap["perms"] = gaeson.JSArray(permsArr)

	return gaeson.JSObject(clMap)
}

func ParseClaims(bytes []byte) (map[string]ClaimSet, error) {
	js, err := gaeson.FromJSONText(bytes)
	if err != nil {
		return map[string]ClaimSet{}, err
	}

	claimsDB := make(map[string]ClaimSet)
	switch js_ := js.(type) {
	case gaeson.JSArray:
		for _, jselem := range js_ {
			switch jselem_ := jselem.(type) {
			case gaeson.JSObject:
				claims, err := decodeClaims(jselem_)
				if err != nil {
					return map[string]ClaimSet{}, err
				}
				claimsDB[claims.id] = claims
			}
		}
	}
	return claimsDB, nil
}

func ParseClaim(bytes []byte) (ClaimSet, error) {
	js, err := gaeson.FromJSONText(bytes)
	if err != nil {
		return ClaimSet{}, err
	}

	switch js_ := js.(type) {
	case gaeson.JSObject:
		return decodeClaims(js_)
	}
	return ClaimSet{}, err
}

func decodeClaims(js gaeson.JSObject) (ClaimSet, error) {
	claims := ClaimSet{}

	/* TODO: make sure that I catch when the json doesn't decode correctly */
	if val, ok := js["aud"]; ok {
		claims.audience = Username(string(val.(gaeson.JSString)))
	}
	if val, ok := js["exp"]; ok {
		claims.expiration = int64(val.(gaeson.JSNumber))
	}
	if val, ok := js["jti"]; ok {
		claims.id = string(val.(gaeson.JSString))
	}
	if val, ok := js["iat"]; ok {
		claims.issuedAt = int64(val.(gaeson.JSNumber))
	}
	if val, ok := js["iss"]; ok {
		claims.issuer = Issuer(string(val.(gaeson.JSString)))
	}
	if val, ok := js["sub"]; ok {
		claims.resource = ResourceName(string(val.(gaeson.JSString)))
	}
	if val, ok := js["perms"]; ok {
		/* TODO: bad formats, say a string instead of an array, can cause
		* panics here, and it might be nice to be able to simply print an error
		* and continue */
		lst := val.(gaeson.JSArray)
		claims.permissions = make([]string, len(lst))
		for i, v := range lst {
			claims.permissions[i] = string(v.(gaeson.JSString))
		}
	}

	return claims, nil
}
