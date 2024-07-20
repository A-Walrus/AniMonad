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

### Layout & Casting
https://github.com/user-attachments/assets/78064a41-cf35-4805-ba98-831244ac19b6
```haskell
r = Rect 100 100 white 10
c = Circle 75 blue
base = [SomeElem r, SomeElem r, SomeElem c, SomeElem r]
anim =
  row 20
    <$> base
      |> key (ix 0 . (as @Rect) . width) 200 1
      <> inner (ix 2 . (as @Circle) . radius) (ky 25 1 <> ky 100 1)
      <> inner (ix 1 . (as @Rect)) (key color tomato 1 <> simul [key height 200, key width 50] 1)
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
base = row 20 [(Rect 120 120 itemColor 24, Text val 40 white) | val <- values]
anim =
  (,) background
    <$> base
      |> sort
      <> delay 0.2
      <> key (traverse . _1 . color) itemColor 0.6
      <> inners (traverse . y) [delay (i * 0.05) <> ky (-90) 0.2 <> ky 0 0.2 | i <- [0 ..]]
      <> delay 1
```
