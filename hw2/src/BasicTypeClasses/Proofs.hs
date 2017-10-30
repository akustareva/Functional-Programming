{-| Block 3
    -------

    Task 1
    ------

    instance Monad m => MonadFish m where ...
    f >=> returnFish == f
    f >=> returnFish = f >=> return
                     = \x -> f x >>= return
                     = \x -> f x
                     = f

    instance Monad m => MonadJoin m where ...
    join . returnJoin == id
    join . returnJoin = (>>= ud) . return
                      = \x -> return x >>= id
                      = \x -> id x
                      = id

    instance MonadFish m => Monad m where ...
    m >>= return == m
    m >>= return = m >>= returnFish
                 = (id >=> returnFish) m
                 = id m
                 = m

    instance MonadFish m => MonadJoin m where ...
    join . returnJoin == id
    join . returnJoin = (id >=> id) . returnFish
                      = \x -> (id >=> id) (returnFish x)
                      = \x -> (\y -> id y >>= id) (returnFish x)
                      = \x -> (\y -> y >>= id) (returnFish x)
                      = \x -> returnFish x >>= id
                      = returnFish >=> id
                      = id

    instance (Functor m, MonadJoin m) => Monad m where ...
    m >>= return == m
    m >>= return = m >>= returnJoin
                 = join $ fmap returnJoin m
                 = (join . fmap returnJoin) m
                 = id m
                 = m

    instance (Functor m, MonadJoin m) => MonadFish m where ...
    f >=> returnFish == f
    f >=> returnFish = f >=> returnJoin
                     = \x -> join $ fmap returnJoin (f x)
                     = \x -> (join . fmap returnJoin) (f x)
                     = \x -> id (f x)
                     = \x -> f x
                     = f

    Task 2
    ------

    instance Functor (Either a) where ...
    Functor 1: fmap id == id
    fmap id x == id x = id
    fmap id (Right x) = Right $ id x = Right x
    fmap id (Left x) = Left x

    instance Functor Identity where ...
    Functor 2: fmap f . fmap g == fmap (f . g)
    a) (fmap f . fmap g) (Identity x) = fmap f (fmap g (Identity x))
                                      = fmap f (Identity $ g x)
                                      = Identity $ f (g x)
    b) fmap (f . g) (Identity x) = Identity $ (f . g) x
                                 = Identity $ f (g x)

    instance Monoid m => Applicative (Const m) where ...
    Applicative 1: pure id <*> v == v
    pure id <*> v = Const mempty <*> Const v'
                  = Const $ mappend mempty v'
                  = Const $ v'
                  = v

    instance Monoid m => Applicative (Const m) where ...
    Applicative 2: pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
    a) pure (.) <*> u <*> v <*> w = Const mempty <*> Const u' <*> v <*> w
                                  = Const $ mappend mappend u' <*> v <*> w
                                  = Const u' <*> v <*> w
                                  = Const u' <*> Const v' <*> w
                                  = Const $ mappend u' v' <*> Const w'
                                  = Const $ mappend (mappend u' v') w'
    b) u <*> (v <*> w) = u <*> Const v' <*> Const w'
                       = Const u' <*> (Const $ mappend v' w')
                       = Const $ mappend u' (mappend v' w')
                       = Const $ mappend (mappend u' v') w'

    instance Monoid m => Applicative (Pair m) where ...
    Applicative 3: pure f <*> pure x == pure (f x)
    pure f <*> pure x = Pair mempty f <*> Pair mempty x
                      = Pair (mappend mempty mempty) $ f x
                      = Pair mempty $ f x
                      = pure (f x)

    instance Monoid m => Applicative (Const m) where ...
    Applicative 4: u <*> pure y == pure ($ y) <*> u
    a) u <*> pure y = Const u' <*> Const mempty
                 = Const $ mappend u' mempty
                 = Const u'
                 = u
    b) pure ($ y) <*> u = Const mempty <*> Const u'
                        = Const $ mappend mempty u'
                        = Const u'
                        = u

    instance Traversable (Const a) where ...
    Traversable 1: t . traverse f == traverse (t . f)
    a) (t . traverse f) (Const x) = t (traverse f (Const x))
                                  = t (pure $ Const x)
                                  = pure $ Const x
    b) traverse (t . f) (Const x) = pure $ Const x

    // TODO:
    instance Traversable Tree where ...
    Traversable 2: traverse Identity == Identity
    a) traverse Identity Leaf = pure Leaf
                              = Leaf
       traverse Identity Node = pure Node
                              = Node
    b) Identity Leaf = Leaf
       Identity Node = Node
|-}