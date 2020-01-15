{-# LANGUAGE OverloadedStrings #-}
-- Importation des librairie necessaires
module Main where
import System.Info
import System.Environment
import System.Directory
import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Graph
import Data.Array
import Data.Time
import Data.Map
import System.IO

data User = User {idd :: String, pwd :: String} deriving Show
u1 = User "1" "user1"
u2 = User "2" "user2"
u3 = User "3" "user3"
type Id = String
type Name = String
type Prix = Int
type Date = (Int,Int,Int)
type Hour = (Int,Int)
type Guide = (Id, Name, Prix)
type Telephone = Int
type S_hour = Hour
type E_hour = Hour
type Cur_hour = Hour
type Cur_date = Date
type S_date = Date
type E_date = Date
type Waiting = Int
type Res = (Id,
            Telephone,
            Name,
            S_hour,
            S_date,
            Cur_hour,
            Cur_date,
            E_hour,
            E_date,
            Waiting,
            Guide)
type ListRes = [Res]


makeRes :: Res -> String
makeRes (id,t,n,sh,sd,ch,cd,eh,ed,w,(t1,nom,t3)) = id++"|"++show t++"|"++n++"|"++show sh++"|"++show sd++"|"++show ch++"|"++show cd++"|"++show eh++"|"++show ed++"|"++show w++"|"++nom


listRes :: ListRes -> ListRes
listRes l = l

makeDate :: Int -> Int -> Int -> Date
makeDate y m d = (y,m,d)

makeHour :: Int -> Int -> Hour
makeHour h m = (h,m)

makeGuide :: Id -> Name -> Prix -> Guide
makeGuide id name prix = (id,name,prix)

getInt :: String -> Int
getInt s = read s :: Int

getHour = do
    putStr "give hour :"
    t <- getLine
    let h = getInt t
    putStr "give minute :"
    t <- getLine
    let m = getInt t
    return (h,m)

getDate = do
    putStr "give year :"
    t <- getLine
    let y = getInt t
    putStr "give month :"
    t <- getLine
    let m = getInt t
    putStr "give day :"
    t <- getLine
    let d = getInt t
    return (y,m,d)

getGuide = do
    putStr "Id of guide :"
    t <- getLine
    let id = t
    putStr "Name of guide :"
    m <- getLine
    let name = m
    putStr "price of guide :"
    p <- getLine
    let price = getInt p
    return (id,name,price)

getRes = do
    putStr "ID -= "
    id <- getLine
    putStr "Tel -= "
    tel <- getLine
    putStr "Name -= "
    n <- getLine
    putStr "start hour hh:mm -= "
    sh <- getHour
    putStr "Start date yyyy-mm-dd -= "
    sd <- getDate
    putStr "current hour hh:mm -= "
    ch <- getHour
    putStr "Current date yyyy-mm-dd -= "
    cd <- getDate
    putStr "end hour hh:mm -= "
    eh <- getHour
    putStr "end date yyyy-mm-dd -= "
    ed <- getDate
    putStr "Waiting -="
    t <- getLine
    let w = getInt t
    putStr "Guide -="
    g <- getGuide

    -- makeRes (id,getInt tel,n,sh,sd,ch,cd,eh,ed,w,g)
    print "------------------------------------------------------"


success = do
    print "Login success"
    menu

login = do
    print "Page de login"
    putStr "Entre votre login\n LOGIN -="
    u_id <- getLine
    putStr "Entre votre mot de passe\n PASSWORD -="
    u_pwd <- getLine
    let tu = idd u1
    let tp = pwd u1
    if tu == u_id && tp == u_pwd
        then success  -- on a seulement gere un cas de login login = 1 et pwd = user1
        else do
            login


-- dataRes RE{idR=id,numR=numTel,nomR=nclt,sH=sh,sD=sd,cH=ch,cD=cd,eH=sh,sD=sd,wait=w,guide=g}

menu = do
    print "-----------------------MENU PRINCIPAL----------------------"
    print "1    --    Sauvegarder une reservation                     "
    print "2    --    Assigner un responsable                         "
    print "3    --    Afficher les reservations                       "
    print "4    --    Liste des livres disponible                     "
    print "5    --    Liste des responsables                          "
    print "6    --    Impression facture                              "
    print "7    --    Modifier reservation                            "
    print "8    --    Fonctions dadministration                       "
    print "9    --    Quitter                                         "
    putStr "Votre Choix -= "
    choix <- getLine 
    print "-----------------------------------------------------------"




main = do
    login