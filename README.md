# SOFA

This is a prototype front-end of Smart § Spec. It is implemented as a
single-page application in the [PureScript] programming language using
the [Halogen] UI library.

## Quick Start

If you just want SOFA up and running as quickly as possible you can
use the Docker image. Run

``` sh-session
$ docker run -p 8080:80 \
    -e ORDERING_SERVICE_URL=https://ordering.eu1tst.bpa.staging.sinch.com \
    -e TOKEN_SERVICE_URL=https://public.token-service.common-auth.staging.sinch.com \
    nexus.int.clxnetworks.net:8089/ea/smart-spec/sofa:latest
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
First make sure you have [Yarn], [PureScript], and [Spago] installed.
If you have [Nix] installed, then you can get a shell with the
required applications by running `nix-shell`.

Also make sure to run `yarn install` to download all additional
dependencies.

1. Build the example JSON files by going to the root directory and
   running

   ``` sh-session
   $ make example example-order-form
   ```

   This command requires [GNU Bash], [GNU Make], [GNU sed], [CUE],
   [jq], and various standard Unix tools such as `mkdir`, `rm`, etc.
   If you use the [Nix] package manager than you can run `nix-shell`
   to get a development shell with all necessary tools in well
   supported versions.

2. If your editor doesn't automatically build the project (e.g.,
   though `purs ide`) then you can start a watcher by running

    ``` sh-session
    $ yarn watch
    ```

   in a terminal.

3. Start serving the application by running

    ``` sh-ession
    $ yarn start
    ```

   in another terminal. When the code is edited it should
   automatically be reopened in the browser.

[CUE]: https://cuelang.org/
[GNU Bash]: https://www.gnu.org/software/bash/
[GNU Make]: https://www.gnu.org/software/make/
[GNU sed]: https://www.gnu.org/software/sed/
[Halogen]: https://purescript-halogen.github.io/purescript-halogen/
[Nix]: https://nixos.org/
[PureScript]: https://www.purescript.org/
[Spago]: https://github.com/purescript/spago
[Yarn]: https://yarnpkg.com/
[jq]: https://stedolan.github.io/jq/
