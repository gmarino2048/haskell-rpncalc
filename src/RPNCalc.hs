{-# LANGUAGE InstanceSigs #-}

module RPNCalc(Result(..), calculate) where

import Text.Read (readMaybe)

data Result a b = Err a | Ok b deriving (Eq, Show, Read)

instance Functor (Result e) where
    fmap :: (a -> b) -> Result e a -> Result e b
    fmap fx (Ok value) = Ok $ fx value
    fmap fx (Err value) = Err value

instance Applicative (Result e) where
    pure :: a -> Result e a
    pure = Ok

    (<*>) :: Result e (a -> b) -> Result e a -> Result e b
    (<*>) (Ok fx) (Ok value) = Ok $ fx value
    (<*>) (Err value) _ = Err value
    (<*>) _ (Err value) = Err value

instance Monad (Result e) where
    (>>=) :: Result e a -> (a -> Result e b) -> Result e b
    (>>=) (Ok value) fx = fx value
    (>>=) (Err value) fx = Err value

data Operation = Add | Subtract | Multiply | Divide deriving (Eq, Show)
data InputValue = Numeric Double | Operational Operation deriving (Eq, Show)

calculate :: String -> Result String Double
calculate expression = validateInputs (processInputs expression) >>= runCalculation

runCalculation :: [InputValue] -> Result String Double
runCalculation inputs = head <$> foldl applyCalculation (Ok []) inputs where
    applyCalculation :: Result String [Double] -> InputValue -> Result String [Double]
    applyCalculation result input = result >>= (`calculation` input)

calculation :: [Double] -> InputValue -> Result String [Double]
calculation (x:y:ys) (Operational Add) = Ok $ (x + y) : ys
calculation (x:y:ys) (Operational Subtract) = Ok $ (y - x) : ys
calculation (x:y:ys) (Operational Multiply) = Ok $ (x * y) : ys
calculation (x:y:ys) (Operational Divide) = if x == 0.0 then Err "Cannot Divide By Zero" else Ok $ (y / x) : ys
calculation xs (Numeric num) = pure $ num : xs
calculation _ (Operational op) = Err $ "Too few numbers on stack to perform operation: " ++ show op

processInputs :: String -> [Result String InputValue]
processInputs expression = map readInputValues (words expression)

readInputValues :: String -> Result String InputValue
readInputValues inputString = case readMaybe inputString :: Maybe Double of
    Just value -> Ok $ Numeric value
    Nothing -> case readOperationFromSymbol inputString of
        Just operation -> Ok $ Operational operation
        Nothing -> Err $ "Failed to parse usable input from \"" ++ inputString ++ "\""

readOperationFromSymbol :: String -> Maybe Operation
readOperationFromSymbol "+" = Just Add
readOperationFromSymbol "-" = Just Subtract
readOperationFromSymbol "*" = Just Multiply
readOperationFromSymbol "/" = Just Divide
readOperationFromSymbol other = Nothing

validateInputs :: [Result a b] -> Result a [b]
validateInputs = fmap reverse . foldl propagateError (Ok []) where
    propagateError :: Result a [b] -> Result a b -> Result a [b]
    propagateError (Ok list) (Ok value) = Ok $ value:list
    propagateError (Err message) _      = Err message
    propagateError _ (Err message)      = Err message
