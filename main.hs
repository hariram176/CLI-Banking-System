import System.IO
import Data.List
import System.Directory (doesFileExist)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Read (readMaybe)

-- DATA TYPE

data Customer = Customer
  { userId   :: String
  , username :: String
  , password :: String
  , phone    :: String
  , balance  :: Int
  } deriving (Show, Read)

fileName :: FilePath
fileName = "customers.txt"


-- HIDE PASSWORD INPUT

getHiddenInput :: IO String
getHiddenInput = do
    hSetEcho stdin False
    input <- getLine
    hSetEcho stdin True
    putStrLn ""
    return input


-- SAFE INTEGER INPUT

getInt :: IO Int
getInt = do
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just n  -> return n
        Nothing -> do
            putStrLn "Please enter a valid number:"
            getInt


-- FILE HANDLING

loadCustomers :: IO [Customer]
loadCustomers = do
    exists <- doesFileExist fileName
    if not exists
        then return []
        else do
            handle <- openFile fileName ReadMode
            content <- hGetContents handle
            let customers = [c | Just c <- map readMaybe (lines content)]
            length customers `seq` hClose handle
            return customers


saveCustomers :: [Customer] -> IO ()
saveCustomers customers =
    writeFile fileName (unlines (map show customers))


updateCustomer :: Customer -> [Customer] -> [Customer]
updateCustomer updated customers =
    map (\c -> if userId c == userId updated then updated else c) customers


-- CORE LOGIC

findUser :: String -> String -> [Customer] -> Maybe Customer
findUser uid pass customers =
    find (\c -> userId c == uid && password c == pass) customers


depositMoney :: Int -> Customer -> Customer
depositMoney amt customer =
    customer { balance = balance customer + amt }


withdrawMoney :: Int -> Customer -> Customer
withdrawMoney amt customer =
    customer { balance = balance customer - amt }


-- REGISTER USER

register :: IO ()
register = do
    customers <- loadCustomers

    putStrLn "Enter User ID:"
    uid <- getLine

    if any (\c -> userId c == uid) customers
        then putStrLn "User ID already exists!"
        else do
            putStrLn "Enter Username:"
            uname <- getLine

            putStrLn "Enter Password:"
            pass <- getHiddenInput

            putStrLn "Enter Phone Number:"
            ph <- getLine

            let newCustomer = Customer uid uname pass ph 0

            appendFile fileName (show newCustomer ++ "\n")

            putStrLn "Account created successfully!"


-- LOGIN USER

login :: IO ()
login = do
    customers <- loadCustomers

    putStrLn "Enter User ID:"
    uid <- getLine

    putStrLn "Enter Password:"
    pass <- getHiddenInput

    case findUser uid pass customers of
        Just customer -> do
            putStrLn ("Welcome " ++ username customer)
            userMenu customer customers

        Nothing ->
            putStrLn "Invalid credentials!"


-- USER MENU

userMenu :: Customer -> [Customer] -> IO ()
userMenu customer customers = do

    putStrLn "\n===== BANK MENU ====="
    putStrLn "1. Check Balance"
    putStrLn "2. Deposit"
    putStrLn "3. Withdraw"
    putStrLn "4. Logout"

    choice <- getLine

    case choice of

        "1" -> do
            putStrLn ("Current Balance: " ++ show (balance customer))
            userMenu customer customers


        "2" -> do
            putStrLn "Enter deposit amount:"
            amt <- getInt

            let updated = depositMoney amt customer
            let updatedList = updateCustomer updated customers

            saveCustomers updatedList

            putStrLn "Deposit successful!"
            userMenu updated updatedList


        "3" -> do
            putStrLn "Enter withdrawal amount:"
            amt <- getInt

            if amt > balance customer
                then do
                    putStrLn "Insufficient balance!"
                    userMenu customer customers

                else do
                    putStrLn "Enter your password to confirm withdrawal:"
                    pass <- getHiddenInput

                    if pass /= password customer
                        then do
                            putStrLn "Incorrect password!"
                            userMenu customer customers

                        else do
                            time <- getPOSIXTime
                            let otp = round time `mod` 1000

                            putStrLn ("OTP: " ++ formatOTP otp)

                            verifyOTP otp 3 customer customers amt


        "4" ->
            putStrLn "Logged out."


        _ -> do
            putStrLn "Invalid choice!"
            userMenu customer customers


-- OTP VERIFICATION

verifyOTP :: Int -> Int -> Customer -> [Customer] -> Int -> IO ()
verifyOTP _ 0 customer customers _ = do
    putStrLn "OTP failed. Transaction cancelled."
    userMenu customer customers

verifyOTP otp attempts customer customers amt = do

    putStrLn ("Enter OTP (" ++ show attempts ++ " attempts left):")

    input <- getLine

    case readMaybe input :: Maybe Int of

        Just val ->
            if val == otp
                then do
                    let updated = withdrawMoney amt customer
                    let updatedList = updateCustomer updated customers

                    saveCustomers updatedList

                    putStrLn "Withdrawal successful!"
                    userMenu updated updatedList

                else do
                    putStrLn "Incorrect OTP!"
                    verifyOTP otp (attempts - 1) customer customers amt


        Nothing -> do
            putStrLn "Invalid OTP!"
            verifyOTP otp attempts customer customers amt


-- OTP FORMAT

formatOTP :: Int -> String
formatOTP n
    | n < 10    = "00" ++ show n
    | n < 100   = "0" ++ show n
    | otherwise = show n


-- MAIN MENU

main :: IO ()
main = do

    putStrLn "\n===== Secure CLI Banking System ====="
    putStrLn "r. Register"
    putStrLn "l. Login"
    putStrLn "e. Exit"

    choice <- getLine

    case choice of

        "r" -> register >> main
        "l" -> login >> main
        "e" -> putStrLn "Thank you! Bye."

        _ -> do
            putStrLn "Invalid choice!"
            main