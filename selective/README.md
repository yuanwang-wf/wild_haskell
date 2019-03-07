# selective


https://github.com/snowleopard/selective
https://www.staff.ncl.ac.uk/andrey.mokhov/selective-functors.pdf
https://twitter.com/andreymokhov/status/1102525008436432896


## the $ trick used in SelectM

```haskell
selectM :: Monad f => f (Either a b) -> f (a -> b) -> f b
selectM x y = x >>= \ e -> case e of Left a -> ($ a) <$> y
                                     Right b -> pure b
```

At first, I am confused about what is going on with `Left a -> ($a) <$> y`. `<$>` (a.k.a `fmap`) usually take a pure function, and a value wrapped in certain functor. But in this case, y is `f (a -> b)`, a is pure value. In haskell, the difference between function and value is blurry, so we can pass `f (a -> b)` to `fmap` as long as the first argument type is `a -> b -> c `, what is `($a)`?  `($a)` is the point free version of `\ f -> f $ a`. Take away is `$` is not just for change evaluation order, it also can compose function. In fact, you go to http://pointfree.io/ to check the pointfree version of `\ f -> f a`, you will get `($ a)`