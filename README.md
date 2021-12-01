# SOFA

This is a prototype front-end of Smart § Spec. It is implemented as a
single-page application in the [PureScript] programming language using
the [Halogen] UI library.

## Quick Start

If you just want SOFA up and running as quickly as possible you can
use the prebuilt Docker image then start it with [Docker Compose]. Run

``` sh-session
$ docker image pull \
    nexus.int.clxnetworks.net:8089/dev/business-process-automation/sofa:latest
$ docker-compose up --no-build
```

in a terminal to fetch the most recent build. Then open
<http://localhost:8080/> in a web browser.

## Development

### Code formatting

Formatting is done using [purty](https://gitlab.com/joneshf/purty),
for example,

``` sh-session
$ find src -name '*.purs' -exec purty format --write '{}' ';'
```

### Running locally

You can run the application in a development mode relatively simply.
Note, when running this way the application will communicate with the
backend services running in the staging environment.

First make sure you have [Yarn], [PureScript], and [Spago] installed.
If you have [Nix] installed with Flake support, then you can get a
shell with the required applications by running `nix develop`.

Also make sure to run `yarn install` to download all additional
dependencies.

1. If your editor doesn't automatically build the project (e.g.,
   though `purs ide`) then you can start a watcher by running

    ``` sh-session
    $ yarn watch
    ```

   in a terminal.

2. Start serving the application by running

    ``` sh-session
    $ yarn start
    ```

   in another terminal. When the code is edited it should
   automatically be reopened in the browser.

[CUE]: https://cuelang.org/
[Docker Compose]: https://docs.docker.com/compose/
[GNU Bash]: https://www.gnu.org/software/bash/
[GNU Make]: https://www.gnu.org/software/make/
[GNU sed]: https://www.gnu.org/software/sed/
[Halogen]: https://purescript-halogen.github.io/purescript-halogen/
[Nix]: https://nixos.org/
[PureScript]: https://www.purescript.org/
[Spago]: https://github.com/purescript/spago
[Yarn]: https://yarnpkg.com/
[jq]: https://stedolan.github.io/jq/
