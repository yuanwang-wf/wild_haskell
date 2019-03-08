# Learning Lens


Lenses, Folds, and Traversals
What is a Lens ?
Lenses address some part of a “structure” that always exists, either look that part, or set that part.
“structure” can be a computation result, for example, the hour in time. Functional setter and getter.
##  Data.Lens

```haskell
data Lens s a = Lens { set  :: s -> a -> a
                     , view :: s -> a
                     }
view :: Lens s a -> s -> a
set  :: Lens s a -> s -> a -> s
```

view looks up an attribute a from s
view is the getter, and set is the setter.
Laws
1. set l (view l s) s = s
2. view l (set l s a) = a
3. set l (set l s a) b = set l s b
Law 1 indicates lens has no other effects
since set and view in Lens both start with s ->, so we can fuse the two functions into a single function
`s -> (a -> s, a)`.

So we could define Lens as
```haskell
data Lens s a = Lens (s -> (a -> s), a)
data Store s a = Store (s -> a) s
data Lens s a = Lens (s -> Store a s)
```

Side bar Store Comonad
https://stackoverflow.com/questions/8428554/what-is-the-comonad-typeclass-in-haskell
https://stackoverflow.com/questions/8766246/what-is-the-store-comonad

Lens can form a category

## Semantic Editor Combinator

The Power is in the dot
```haskell
(.)         :: (b -> c) -> (a -> c) -> (a -> c)
(.).(.)     :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.).(.).(.) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
```
[Detail Explantation](https://www.reddit.com/r/haskellquestions/comments/ayi445/help_me_understand_the_function_and_its_type/)

`(.) :: (b -> c) -> (a -> b) -> a -> c`
`f . g = \ t -> f (g t)`

`((.).(.)) (+1) (+) 10 10 = 21`
```
   ((.).(.)) (+1) (+) 10 10
=  (.)((.) (+1)) (+) 10 10
=  ((.) (+1)) (+ 10) 10
= ((+1). (+10)) 10
= (+1) ((+10) 10)
= (+1) (10 + 10)
= 21
```


`(.).(.) :: (b -> c) -> (a -> b) -> a -> c`
`dotF . dotG :: (b -> c) -> (a -> c) -> a -> c`
`dotF :: b -> c`
`dotG :: a -> b`

since `dotF` is just an alias to `(.)`
`dotF :: (u -> v) -> (s -> u) -> s -> v`
`(u -> v) -> (s -> u) -> s -> v === b -> c`
therefore
`b === (u -> v)`
`c === (s -> u) -> s -> v`

`dotG` is also an alias to `(.)`
`dotG :: (y -> z) -> (x -> y) -> x -> z`
`(y -> z) -> (x -> y) -> x -> z === a -> b`
therefore
`a === (y -> z)`
`b === (x -> y) -> x -> z`

since `b` appears on both side

```
a === y -> z
b === (u -> v)
b === (x -> y) -> x -> z
c === (s -> u) -> s -> v
```

we can deduct

```
u === x -> y
v === x -> z
```

therefore `c === (s -> x -> y) -> s -> x -> z`

`(.).(.) === a -> c`
`(.).(.) === (y -> z) -> (s -> x -> y) -> s -> x - z`



we can generalize this to any functor

```haskell
fmap                :: Functor f  => (a -> b) -> f a -> f b
fmap . fmap         :: ( Functor f, Functor g)   => (a -> b) -> f (g a) -> f (g b)
fmap . fmap . fmap  :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
```

it means we compose a function under arbitrary level deep nested context
Semantic Editor Combinator

`type SEC s t a b = (a -> b) -> s -> t`
it like a functor (a -> b) -> f a -> f b
`fmap` is Semantic Editor Combinator
`fmap . fmap` is also a SEC

so we use `s` to generalize `f a`,  `f (g a)`, `f (g (h a))` and `t` to generalize `f b`,  `f (g b)`, `f (g (h b))`.
It may seems counterintuitive at first, we lost the relation between a and s, b and t.
Functor is a semantic Editor Combinator
`fmap :: Functor f => SEC (f a) (f b) a b`

first is a also SEC ?
```haskell
first :: SEC (a, c) (b,c) a b
first f (a, b) = (f a, b)
```
Setters
We can compose Traversable the way as we can compose `(.)` and `fmap`

```haskell
traverse                       :: (Traversable f, Applicative m) => (a -> m b) -> f a -> m (f b)
traverse . traverse            :: (Traversable f, Traversable g, Applicative m) => (a -> m b) -> f (g a) -> m (f (g b))
traverse . traverse . traverse :: (Traversable f, Traversable g, Traversable h, Applicative m) => (a -> m b) -> f (g (h a)) -> m (f (g (h b)))
```

traverse is a generalized version of mapM, and work with any kind of Foldable not just List.

```
class (Functor f, Foldable f) => Traversable f where
   traverse :: Applicative m => (a -> m b) -> f a -> m (f b)
```

```haskell
fmapDefault :: forall t a b. Traverable t => (a -> b) -> t a -> t b
fmapDefault f = runIndentity . traverse (Identity . f)
```

build `fmap` out from traverse
we can change `fmapDefault`

```haskell
over l f = runIdentity . l (Identity . f)
over traverse f = runIdentity . traverse (Identity . f)
                = fmapDefault f
                = fmap f
```
type of `over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t`
type Setter s t a b = (a -> Identity b) -> s -> Identity t
so we could rewrite `over` as
over :: Setter s t a b -> (a -> b) -> s -> t
let’s apply setter

```haskell
mapped :: Functor f => Setter (f a) (f b) a b
mapped f = Identity . fmap (runIdentity . f)
over mapped f = runIdentity . mapped (Identity . f)
              = runIdentity . Identity . fmap (runIdentity . Identity . f)
              - fmap f
```

Examples

```haskell
over mapped (+1) [1,2,3]  ===> [2,3,4]
over (mapped . mapped) (+1) [[1,2], [3]] ===> [[2,3], [4]]
chars :: (Char -> Identity Char) -> Text -> Identity Text
chars f = fmap pack . mapped f . unpack
```

Laws for setters
Functor Laws:
1. `fmap` id = id
2. fmap f . fmap g = fmap (f . g)

Setter Laws for a legal Setter l.
1. over l id = id
2. over l f . over l g = over l (f . g)


Practices
Simplest lens
(1,2,3) ^. _2  ### ===> 2
view _2 (1, 2, 3)


## References
1. http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf
2. http://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html
3. https://www.youtube.com/watch?v=H01dw-BMmlE&feature=youtu.be
4. https://www.youtube.com/watch?v=QZy4Yml3LTY
5. https://www.youtube.com/watch?v=T88TDS7L5DY
6. http://lens.github.io/tutorial.html
7. https://www.reddit.com/r/haskell/comments/9ded97/is_learning_how_to_use_the_lens_library_worth_it/e5hf9ai/
8. https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation
9. https://blog.jle.im/entry/lenses-products-prisms-sums.html




