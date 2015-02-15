import PDDL.Type
import Planning.FastDownward
import PDDL.Samples.SimpleBox

main :: IO ()
main = domainToFile sBDomain "dom.pddl"
