import PDDL.Type
import FastDownward
import PDDL.Samples.SimpleBox

main :: IO ()
main = domainToFile sBDomain "dom.pddl"
