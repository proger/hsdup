## hsdup

Finds duplicate files in a directory (recursively).

### Usage

```
% cabal sandbox init
% cabal install
% ./.cabal-sandbox/bin/hsdup ~/Pictures | tee junk    # prints files that are safe to delete 
                                                # (the criteria is a path length. works for me)
% cat junk | tr '\n' '\0' | xargs -0t -n1 echo rm     # oops
```
