package orizentic

import (
	"time"
)

type ResourceName string
type PermissionList []string
type Issuer string
type TTL time.Duration
type Username string
type Secret []byte

type InvalidToken string

func (self InvalidToken) Error() string {
	return "Token does not validate with this key"
}

type UnknownToken string

func (self UnknownToken) Error() string {
	return "Token is unknown"
}

type ExpiredToken string

func (self ExpiredToken) Error() string {
	return "Token has expired"
}

type Set map[interface{}]bool

func mkSet(lst []interface{}) Set {
	m := make(Set)
	for _, elem := range lst {
		m[elem] = true
	}
	return m
}

func (self Set) difference(rside Set) Set {
	diff := make(Set)
	for k, _ := range self {
		_, ok := rside[k]
		if !ok {
			diff[k] = true
		}
	}
	return diff
}
