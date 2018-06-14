package main

import (
	"fmt"
	"gopkg.in/urfave/cli.v1"
	"io/ioutil"
	"os"
	"strings"

	"github.com/luminescent-dreams/orizentic/go/orizentic"
)

func main() {
	var configPath string
	var secret string

	app := cli.NewApp()
	app.Name = "orizentic"
	app.Flags = []cli.Flag{
		cli.StringFlag{
			Name:        "config",
			EnvVar:      "ORIZENTIC_CONFIG",
			Destination: &configPath,
		},
		cli.StringFlag{
			Name:        "secret",
			EnvVar:      "ORIZENTIC_SECRET",
			Destination: &secret,
		},
	}
	app.Commands = []cli.Command{
		{
			Name:   "list",
			Action: func(c *cli.Context) error { return listTokens(configPath) },
		},
		{
			Name: "create",
			Flags: []cli.Flag{
				cli.StringFlag{Name: "issuer"},
				cli.UintFlag{Name: "ttl"},
				cli.StringFlag{Name: "resource"},
				cli.StringFlag{Name: "username"},
				cli.StringFlag{Name: "perms"},
			},
			Action: func(c *cli.Context) error { return createToken(configPath, secret, c) },
		},
		{
			Name: "revoke",
			Flags: []cli.Flag{
				cli.StringFlag{Name: "id"},
			},
			Action: func(c *cli.Context) error { return revokeToken(configPath, secret, c) },
		},
		{
			Name: "encode",
			Flags: []cli.Flag{
				cli.StringFlag{Name: "id"},
			},
			Action: func(c *cli.Context) error { return encodeToken(configPath, secret, c.String("id")) },
		},
	}

	err := app.Run(os.Args)
	if err != nil {
		fmt.Println(err)
	}
}

func listTokens(configPath string) error {
	fmt.Println("list tokens from ", configPath)
	tokens, err := loadClaims(configPath)
	if err != nil {
		return err
	}

	for _, v := range tokens {
		fmt.Printf("[%v]\n", v.Id())
		fmt.Printf("Audience:       %v\n", v.Audience())
		fmt.Printf("Expiration:     %v\n", v.Expiration())
		fmt.Printf("Issuer:         %v\n", v.Issuer())
		fmt.Printf("Issued At:      %v\n", v.IssuedAt())
		fmt.Printf("Resource Name:  %v\n", v.ResourceName())
		fmt.Printf("Permissions:	%v\n", v.Permissions())

		fmt.Printf("\n")
	}
	return nil
}

func createToken(configPath string, secret string, c *cli.Context) error {
	tokens, err := loadClaims(configPath)
	if err != nil {
		return err
	}

	ctx := orizentic.NewContext(orizentic.Secret(secret), tokens)
	issuer := orizentic.Issuer(c.String("issuer"))
	ttl := orizentic.TTL(c.Uint("ttl"))
	resource := orizentic.ResourceName(c.String("resource"))
	username := orizentic.Username(c.String("username"))
	perms := orizentic.PermissionList(strings.Split(c.String("perms"), ","))

	claimset, err := ctx.CreateClaims(issuer, &ttl, resource, username, perms)
	if err != nil {
		return err
	}

	fmt.Println(claimset)

	claims, err := ctx.ListClaims()
	if err != nil {
		return nil
	}

	return saveClaims(configPath, claims)
}

func revokeToken(configPath string, secret string, c *cli.Context) error {
	tokens, err := loadClaims(configPath)
	if err != nil {
		return err
	}

	ctx := orizentic.NewContext(orizentic.Secret(secret), tokens)
	ctx.RevokeByUUID(c.String("id"))

	claims, err := ctx.ListClaims()
	if err != nil {
		return nil
	}
	return saveClaims(configPath, claims)
}

func encodeToken(configPath string, secret string, id string) error {
	tokens, err := loadClaims(configPath)
	if err != nil {
		return err
	}

	c := orizentic.NewContext(orizentic.Secret(secret), tokens)
	claim := tokens[id]
	str, err := c.EncodeClaims(claim)
	if err != nil {
		return err
	}
	fmt.Printf("%s\n", str)
	return nil
}

func loadClaims(path string) (map[string]orizentic.ClaimSet, error) {
	m := map[string]orizentic.ClaimSet{}
	bytes, err := ioutil.ReadFile(path)
	if err != nil {
		return m, err
	}
	return orizentic.ParseClaims(bytes)
}

func saveClaims(path string, claims []orizentic.ClaimSet) error {
	bytes := orizentic.FormatClaims(claims)
	return ioutil.WriteFile(path, bytes, 0)
}
