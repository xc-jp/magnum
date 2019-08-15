# magnum

Two functions, `readNum` and `showNum`, to work with numbers with a magnitude postfix:
```
> showNum (Just 3) 1234567
"1.234M"

> (readNum "1.234M" :: Int)
Just 1234000
```

Is legally required to only depend on base.
