module StateEither where

newtype StateEither s e a = StateEither
    { runStateEither :: s -> (s, Either e a)
    }

instance Functor (StateEither s e) where
    f `fmap` StateEither se = StateEither (\ s -> let (s', eitherA) = se s in (s', f `fmap` eitherA))

instance Applicative (StateEither s e) where
    pure a = StateEither (\ s -> (s, Right a))
    StateEither h <*> StateEither g = StateEither (\ s -> let (s', eitherF) = h s
                                                              (s'', a)      = g s in (s'', eitherF <*> a) )

instance Monad (StateEither s e) where
    StateEither f >>= g = StateEither $ \ s ->
        case f s of
            (s', Left e) -> (s', Left e)
            (s', Right a) -> (runStateEither $ g a) s'

execStateEither :: StateEither s e a -> s -> s
execStateEither stateE = fst . runStateEither stateE

modify' :: (s -> Either e s) -> StateEither s e ()
modify' f = StateEither $ \ s ->
    case f s of
        Left e -> (s, Left e)
        Right s' -> (s', Right ())

foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f accum xs=
        execStateEither  (mapM_ go xs) accum
    where go x = modify' (`f` x)
