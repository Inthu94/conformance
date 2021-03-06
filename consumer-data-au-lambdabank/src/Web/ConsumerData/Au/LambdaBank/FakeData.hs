{-# LANGUAGE OverloadedStrings #-}
module Web.ConsumerData.Au.LambdaBank.FakeData where

{--
Notes: this is just moving the FakeServer from the types tests across for now.
We will split it apart and make it into a real server with config and a database
and not all in one file. Don't be too upset
that it looks heinous for now!
--}

import Data.Currency            (Alpha(AUD))
import Country.Identifier       (australia)
import Data.Profunctor          (lmap)
import Data.Digit.Decimal
import Data.List.NonEmpty       (NonEmpty((:|)))
import Data.Maybe               (fromMaybe)
import Data.Time (fromGregorian, UTCTime(..))
import Servant.Links            (Link)

import Web.ConsumerData.Au.Api.Types

a12345 :: AccountId
a12345 = AccountId (AsciiString "12345")

testBalances :: AccountBalances
testBalances = AccountBalances
  [ AccountBalance a12345
      (BalanceDeposit (DepositBalanceType (CurrencyAmount (AmountString "400") Nothing) (CurrencyAmount (AmountString "350.75") Nothing)))
  ]

testPerson :: Person
testPerson = Person
  (UTCTime (fromGregorian 2018 11 13) 0)
  "Ben"
  "Kolera"
  ["Leigh"]
  "Mr"
  Nothing
  (Just $ OccupationCode (V6 DecDigit2 DecDigit6 DecDigit1 DecDigit3 DecDigit1 DecDigit3))
testPhoneNumber :: PhoneNumber
testPhoneNumber = PhoneNumber True PhoneNumberPurposeMobile (Just "+61") (Just "04") "88145427" Nothing "+61488145427"

testEmailAddress :: EmailAddress
testEmailAddress = EmailAddress True EmailAddressPurposeWork "ben.kolera@data61.csiro.au"

testAddress :: PhysicalAddress
testAddress = PhysicalAddress
  AddressPurposeRegistered
  (AddressSimple $ SimpleAddress
    (Just "Ben Kolera")
    "Level 3, T.C Beirne Centre"
    (Just "315 Brunswick St")
    Nothing
    (Just "4006")
    "Fortitude Valley"
    (AustralianState AustraliaStateQLD)
    (Just australia))

testPersonDetail :: PersonDetail
testPersonDetail = PersonDetail
  testPerson
  -- TODO: Fix waargonaut bug where nonempty fails on a single element NEL. :)
  (testPhoneNumber :| [])
  [testEmailAddress]
  [testAddress]

testOrganisation :: Organisation
testOrganisation = Organisation
  (UTCTime (fromGregorian 2018 11 13) 0)
  (Just "Ben")
  "Kolera"
  "Programmer"
  "Data 61"
  (Just "Data 61 Legal Name")
  (Just "D61")
  (Just "abn123")
  (Just "acn123")
  (Just True)
  (Just (IndustryCode (V5 x3 x3 x6 x6 x1)))
  (Just OrgTypeCompany)
  (Just australia)
  (Just $ UTCTime (fromGregorian 2015 8 1) 0)

testOrganisationDetail :: OrganisationDetail
testOrganisationDetail = OrganisationDetail
  testOrganisation
  [testAddress]

fakePaginator :: Maybe PageNumber -> (Maybe PageNumber -> Link) -> Paginator
fakePaginator pMay = Paginator p p 0 . (lmap Just)
  where
    p = fromMaybe (PageNumber 1) pMay

testAccount :: Account
testAccount =
  Account a12345 "acc12345" (Just "my savings")
    (MaskedAccountNumber "abcde") (Just PCTermDeposits) "saving"
    (BalanceDeposit (DepositBalanceType (CurrencyAmount (AmountString "201.30") (Just $ CurrencyString AUD)) (CurrencyAmount (AmountString "198.80") Nothing)))

testAccountDetail :: AccountDetail
testAccountDetail = AccountDetail (Just testAccount) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

identified :: a -> Identified a
identified = Identified a12345 "acc12345" (Just "my savings")

testAccounts :: Accounts
testAccounts = Accounts
  [ testAccount
  ]

testAccountTransactions :: AccountTransactions
testAccountTransactions = AccountTransactions a12345 "name" (Just "nick name") (Transactions [testTransaction])

testTransaction :: Transaction
testTransaction = Transaction
  (Just (TransactionId (AsciiString "reference"))) False TransactionStatusPosted "" Nothing Nothing Nothing Nothing "ref"

testTransactionDetail :: TransactionDetail
testTransactionDetail = TransactionDetail Nothing TransactionStatusPosted "" Nothing Nothing Nothing Nothing "" Nothing

testAccountsTransactions :: BulkTransactions
testAccountsTransactions = BulkTransactions [testBulkTransaction]

testBulkTransaction :: BulkTransaction
testBulkTransaction = BulkTransaction a12345 Nothing True BulkTransactionStatusPending "" Nothing Nothing Nothing Nothing ""

testAccountTransactionsDetail :: TransactionsDetail
testAccountTransactionsDetail = TransactionsDetail a12345 "" Nothing (TransactionDetails [testTransactionDetail])

testDirectDebitAuthorisations :: DirectDebitAuthorisations
testDirectDebitAuthorisations = DirectDebitAuthorisations
  [ AccountDirectDebit
      a12345
      (Just (AuthorisedEntity "me" "my bank" Nothing Nothing))
      Nothing
      (Just (AmountString "50.00"))
  ]

testPayee :: Payee
testPayee = Payee (PayeeId "5") "payee-nickname" Nothing Domestic

testPayees :: Payees
testPayees = Payees [testPayee]

testPayeeDetail :: PayeeDetail
testPayeeDetail = PayeeDetail testPayee $ PTDDomestic $ DPPayeeId $ DomesticPayeePayId
  "payee" "hello" OrgNumber

testProduct :: Product
testProduct = Product (AsciiString "product-id-5") Nothing Nothing (DateTimeString (UTCTime (fromGregorian 2018 1 1) 0))
  PCTermDeposits "product name" "description" "fancy" Nothing Nothing True Nothing

testProducts :: Products
testProducts = Products [testProduct]

testProductDetail :: ProductDetail
testProductDetail = ProductDetail (Just testProduct) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
