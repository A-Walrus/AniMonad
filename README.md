# AniMonad
_AniMonad_ is a programmatic animation library in Haskell.

Inspired By:
- [Motion Canvas](https://motioncanvas.io/)
- [Manim](https://www.manim.community/)

## Demo
```haskell
    base = [at (V2 (x * 80) 0) (Rect 60 60 white 10) | x <- [-5 .. 5]]
    anim =
      base
        |> simul [key (ix 4 . color) blue, key (ix 7 . color) red] 0.5
        <> inner (ix 0 . color) (ky yellow 0.5 <> ky limegreen 0.5)
        <> key (ixs [4, 7] . y) (-80) 1
        <> inner (partsOf (ixs [4, 7] . x)) (\[a, b] -> ky [b, a] 1)
        <> key (ixs [4, 7] . y) 0 1
        <> mapEnd (sortOn (view x))
        <> key (traverse . color) white 0.5
        <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]  
```

https://github.com/A-Walrus/AniMonad/assets/58790821/a6d2a643-a9f8-4764-bdb8-3ea5461da802

