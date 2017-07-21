# Install haskell and your first program Guess Number

## Install Stack

[https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)



## Editor Intgration 

### Atom



\[Emacs\]\(https://commercialhaskell.github.io/intero/\)



\#\# VScode with haskell-ide-engine

Haskell Syntax Highlight

https://github.com/haskell/haskell-ide-engine



Install on MacOS

brew install icu4c && brew link icu4c --force



\`\`\`bash

stack install text-icu --extra-lib-dirs=/usr/local/opt/icu4c/lib --extra-include-dirs=/usr/local/opt/icu4c/include

\`\`\`



\`\`\`bash

git clone https://github.com/haskell/haskell-ide-engine

stack install

\`\`\`



stack install hoogle

hoogle generate

