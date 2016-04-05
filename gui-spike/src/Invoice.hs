module Invoice where

-- Invariants
-- Invoice is a complete, validated document that has been persisted
-- Anything that needs to be edited goes through separate structures

-- FIXME: types here should be common for both this and editable structures
type Name = String
type Code = String
type Key = Int
type Date = String
type Lines = [Line]
type ItemName = String
type Price = Int

data Invoice = Invoice { pKey :: Key
                       , name :: Name
                       , date :: Date
                       , customerInfo :: CustomerInfo
                       , lines :: Lines }
             deriving (Show)


data CustomerInfo = CustomerInfo { cKey :: Key
                                 , cName :: Name }
                  deriving (Show)


data Line = Line { lKey :: Key
                 , lItem :: ItemName
                 , lPrice :: Price }
          deriving (Show)
