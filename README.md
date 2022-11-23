# Alster

> A proof-of-concept Language Server for the [Agda][Agda] programming language.

So far it doesn't do much, but it can parse your Agda files and highlight (some)
syntax errors as you type.

## Building

Currently, the only tested compiler is _GHC 9.4.2_.  Use `cabal` to build the project:

```sh
$ cabal build exe:alster
```

Alternatively, use [`nix`][Nix] to obtain a reproducible development environment:

```sh
# Binary will be symlinked to ./result/bin/Alster
$ nix build

$ nix shell
(nix-shell) # Inside of a Nix shell, all dev dependencies are available:
(nix-shell) which ghc
/nix/store/44lbf7kcza4a5k1w2hspga2621zjg62k-ghc-9.4.2-with-packages/bin/ghc
```

## Usage

To set up _Alster_ for use with [_Neovim_][Neovim], use [this fork][lspconfig] of `lspconfig`,
or copy the file `lua/lspconfig/server_configurations/alster.lua` there to your runtime path.
Afterwards, the _Alster_ is configured like any other language server:

```lua
require('lspconfig').alster.setup {
    cmd = { '/custom/path/to/alster', 'arg1', 'arg2', ... },
    settings = {
        Alster = { ... },
    },
}
```

If want to use _Alster_ with a different editor, consult your editor's LSP documentation.
For _Emacs_, [`lsp-mode`][lsp-mode] is probably a good starting point.
If you're using _VS Code_ and feel brave, [good luck][vscode-guide].

## Contributing

Don't wait for me to implement everything!
Feel free to open issues if you need help getting the server to run,
or if you want to discuss its design.
Alternatively, head over to the [Agda Zulip][Agda-Zulip] and discuss with us there.

## License

This project is licensed under the [MIT License][MIT].
See [`LICENSE`](LICENSE).


[Agda]: https://wiki.portal.chalmers.se/agda/pmwiki.php
[Agda-Zulip]: https://agda.zulipchat.com
[Nix]: https://nixos.org/
[Neovim]: https://neovim.io/
[lspconfig]: https://github.com/phijor/nvim-lspconfig/tree/feature-agda-language-server
[lsp-mode]: https://emacs-lsp.github.io/lsp-mode/page/adding-new-language/
[vscode-guide]: https://code.visualstudio.com/api/language-extensions/language-server-extension-guide
[MIT]: https://choosealicense.com/licenses/mit/
