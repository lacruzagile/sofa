# SOFA

This is a prototype front-end of Smart § Spec. It is implemented as a
single-page application in the [PureScript] programming language using
the [Halogen] UI library.

## Quick Start

If you just want SOFA up and running as quickly as possible you can
use the prebuilt Docker image then start it with [Docker Compose]. Run

``` sh-session
$ docker image pull \
    nexus.int.clxnetworks.net:8089/sinch/sinch-projects/enterprise-and-messaging/beehive/teams/customer-provisioning/smart-ordering/sofa:latest
$ docker-compose up --no-build
```

in a terminal to fetch the most recent build. Then open
<http://localhost:1234/> in a web browser.

## Development

### Code formatting

Formatting of the PureScript code is done using [purty]. It can be run
by

``` sh-session
$ yarn format
```

### Running locally

You can run the application in a development mode relatively simply.
Note, when running this way the application will communicate with the
backend services running in the staging environment.

First make sure you have [Yarn] installed. If you have [Nix] installed
with Flake support, then you can get a shell with the required
applications by running `nix develop`.

Also make sure to run `yarn install` to download all additional
dependencies.

1. If your editor doesn't automatically build the project (e.g.,
   though `purs ide`) then you can start a watcher by running

    ``` sh-session
    $ yarn watch
    ```

   in a terminal.

1. Add the SOFA authentication client credentials to `.proxyrc`. These
   can be found in [Vault](https://vault.int.staging.sinch.com/ui/vault/secrets/secret/show/org_engineering_self_serve_and_common_services/bpa/sofa-oauth-client-creds).

1. Start serving the application by running

    ``` sh-session
    $ yarn start
    ```

   in another terminal. When the code is edited it should
   automatically be reopened in the browser.

### Documentation

Code documentation is generated by the CI system and published on
[GitLab Pages](https://dev.pages.sinch.com/business-process-automation/sofa/).

You can generate the documentation locally by running

``` sh-session
$ yarn spago docs
…
```

and opening `generated-docs/html/index.html` in a browser.

### Using a custom product catalog

If you want to experiment using SOFA with a custom product catalog
then the simplest way is to edit `~/.proxyrc` so that the line

    "/examples": {

becomes

    "/examples/legalentities.json": {

Then place your product catalog JSON file in

    dist/examples/product-catalog.json

## Salesforce deployment

The `salesforce` directory contains the necessary files to produce a
Salesforce deployment.

Note, when deploying to a new Salesforce environment, it is necessary
to create an SSL certificate. To do that you can follow the
[Salesforce authentication key instructions]. The `server.crt` file
should be sent to the person on the Salesforce side and the
`server.key` file should be added as a GitLab CI variable using the
"File" type.

[CUE]: https://cuelang.org/
[Docker Compose]: https://docs.docker.com/compose/
[GNU Bash]: https://www.gnu.org/software/bash/
[GNU Make]: https://www.gnu.org/software/make/
[GNU sed]: https://www.gnu.org/software/sed/
[Halogen]: https://purescript-halogen.github.io/purescript-halogen/
[Nix]: https://nixos.org/
[PureScript]: https://www.purescript.org/
[Salesforce authentication key instructions]: https://developer.salesforce.com/docs/atlas.en-us.sfdx_dev.meta/sfdx_dev/sfdx_dev_auth_key_and_cert.htm
[Spago]: https://github.com/purescript/spago
[Yarn]: https://yarnpkg.com/
[jq]: https://stedolan.github.io/jq/
[purty]: https://gitlab.com/joneshf/purty
