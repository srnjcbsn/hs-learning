module Planning.Loggable where

class Loggable l where
    logg :: l -> String -> String
    chart :: l -> ()
