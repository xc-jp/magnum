# magnum

Two functions, `readNum` and `showNum`, to work with numbers with a magnitude postfix:
```
> showNum 12345678
"12M"

> readNum "12M"
12000000
```

Is legally required to only depend on base.

To maybe do at some point if I need it:

  - `showNum` decimal points ("12.3M")
  - `readNum` decimal points ("12.3M")
