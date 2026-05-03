# mtl2b

```
cd existing_repo
git remote add origin https://gitlab.irit.fr/methodesformelles/mtl2b/mtl2b.git
git branch -M main
git push -uf origin main

```

## Compile
```
  dune build
```

## Execute
with options
  -q: no trace
  -b: generates B machine
  -evb: generates Event-B machine
  -ta: generates pdf automaton
  -xta: generates Uppaal automaton
  -f: MTL formula

```
  _build/install/default/bin/mtl2x.exe -q -b -evb -ta -xta -f '[][<=2]([][<=3]p)'
```
