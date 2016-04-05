module EdtInvoice where

-- FIXME: types should be common
type Name = String
type Code = String
type Key = Int
type Date = String
type Lines = [Line]
type ItemName = String
type Price = Int

-- EdtInvoice may be an invoice draft

newtype Command = String
type InvError = String

data EdtInvoice = EdtInvoice { pK :: Maybe Int
                             , header :: Header
                             , lines :: Lines }
                deriving (Show)

data Header = Header { name :: Maybe Name
                     , date :: Maybe Date
                     , customerInfo :: Maybe CustomerInfo}

data CustomerInfo = CustomerInfo { cKey :: Key
                                 , cName :: Name }
                  deriving (Show)

-- Should convert Invoice to EdtInvoice, perform commands, validation (?)
-- and converts back an invoice => how about saving???
editInvoice :: EdtInvoice -> [Command] -> Maybe [InvError] EdtInvoice
