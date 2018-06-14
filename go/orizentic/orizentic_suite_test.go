package orizentic_test

import (
	. "github.com/onsi/ginkgo"
	. "github.com/onsi/gomega"

	"testing"
)

func TestOrizentic(t *testing.T) {
	RegisterFailHandler(Fail)
	RunSpecs(t, "Orizentic Suite")
}
