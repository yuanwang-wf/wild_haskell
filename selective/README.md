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

At first, I am confused about what is going on with `Left a -> ($a) <$> y`. `<$>` (a.k.a `fmap`) usually take a pure function, and a value wrapped in certain functor. But in this case, y is `f (a -> b)`, a is pure value. In Haskell, the difference between function and value is blurry, so we can pass `f (a -> b)` to `fmap` as long as the first argument type is `a -> b -> c `, what is `($a)`?  `($a)` is the point free version of `\ f -> f $ a`. Take away is `$` is not just for change evaluation order, it also can compose function. In fact, you go to http://pointfree.io/ to check the pointfree version of `\ f -> f a`, you will get `($ a)`


```haskell
apDefault :: Selective f => f (a -> b) -> f a -> f b
apDefault f x = select (Left <$> f) (flip ($) <$> x)
```

`($)` type is `(a -> b) -> a -> b`, `flip ($)` type is `a -> (a -> b) -> b`. `(flip ($) <$> x)` type is `f ((a -> b) -> b)`, `Left <$> f` type is `f (Left (a -> b))`. `select (f (Left (a -> b))) (f ((a -> b) -> b))` , since `select :: f (Either a b) -> f (a -> b) -> f b`, `select (f Left (a -> b)) f ((a -> b) -> b) -> f b`


Let's define `whenS` using `Selective`

```haskell
whenS :: Selective f => f Bool -> f () -> f ()
whenS x y = selector <*? effect
    where
        selector = bool (Right ()) (Left ()) <$> x
        effect   = const                     <$> y
```

`bool :: a -> a -> Bool -> a`, so there `bool (Right ()) (Left ()) :: Bool -> Either () ()` and `x :: f Bool`, so `bool (Right ()) (Left ()) <$> x :: f (Either () ())`.

`selector :: f (Either () ())`

`const :: a -> b -> a`, `y :: f ()` , `const <$> y :: f (b -> ())`

`effect :: f (() -> ())`

```haskell
branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch e first second = selector <*? first
    where
        selector = liftA2 (\ e g -> either Left (Right. g) e ) e second
```


```haskell
branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch e first second = selector <*? first
    where
        selector = liftA2 (\ e g -> fmap g e ) e second
```

```haskell
branch :: Selective f => f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
branch e first second = selector <*? first
    where
        selector = liftA2 (flip fmap) e second
```