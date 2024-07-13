# AniMonad
_AniMonad_ is a programmatic animation library in Haskell.

Inspired By:
- [Motion Canvas](https://motioncanvas.io/)
- [Manim](https://www.manim.community/)

## Demos
### Simple
https://github.com/user-attachments/assets/4526f0b3-3dcd-46da-8ef0-b53c488d35c1
```haskell
anim =
  Circle 100 white
    |> key color blue 2
    <> key radius 200 0.5
    <> inner radius (signal ((* 100) . (+ 1) <$> sample (2 * pi) cos))
    <> delay 1
```

### Less simple
https://github.com/A-Walrus/AniMonad/assets/58790821/a6d2a643-a9f8-4764-bdb8-3ea5461da802
```haskell
base = [at (V2 (x * 80) 0) (Rect 60 60 white 10) | x <- [-5 .. 5]]
anim =
  base
    |> simul [key (ix 4 . color) blue, key (ix 7 . color) red] 0.5
    <> inner (ix 0 . color) (ky yellow 0.5 <> ky limegreen 0.5)
    <> key (ixs [4, 7] . y) (-80) 1
    <> inner (partsOf (ixs [4, 7] . x)) (fn (\[a, b] -> ky [b, a] 1))
    <> key (ixs [4, 7] . y) 0 1
    <> mapEnd (sortOn (view x))
    <> key (traverse . color) white 0.5
    <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]
```

### Bubble Sort
https://github.com/user-attachments/assets/1d45ba7a-d711-419c-bf1f-9dcb616b314e
```haskell
background = Rect 1024 1024 bgColor 0
values = [15, 62, 30, 69, 58, 44, 81] :: [Int]
len = length values
try_swap a b t =
  key (ixs [a, b] . y) 50 t
    <> innerFn
      (partsOf (ixs [a, b]))
      ( \[n1, n2] ->
          if (n1 ^. _2 . str) > (n2 ^. _2 . str)
            then
              innerFn (partsOf (traverse . x)) (\[a1, a2] -> ky [a2, a1] t)
            else
              mempty
      )
    <> key (ixs [a, b] . y) 0 t
    <> mapEnd (sortOn (view x))
times = 0.5 : map (max 0.2 . (* 0.9)) times
sort = fold (zipWith sortPass [(len - 2), (len - 3) .. (-1)] times)
sortPass i t = foldMap (\b -> try_swap b (b + 1) t) [0 .. i] <> key (ix (i + 1) . _1 . color) disabledColor t
base = zipWith (\i v -> at (V2 ((i - (fromIntegral (length values - 1) / 2)) * 140) 0) (Rect 120 120 itemColor 25, Text v 40 white)) [0 ..] values
anim =
  (,) background
    <$> base
      |> sort
      <> delay 0.2
      <> key (traverse . _1 . color) itemColor 0.6
      <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 .. 10]]
      <> delay 1
```
