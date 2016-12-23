module CSP where
import Data.List (find,nub,sortBy)

tab = "\t"

type Condition = String
type Expression = String
type SetName = String
type Event = String
type DataName = String
type ProcVar = String
type Var = String
type Declaration = [(Var,Events)]

data EventsType = Set | Seq | Chan deriving (Show,Eq)

data Events = SName SetName | List EventsType [Event] | Union Events Events |
              Unions [Events] | Diff Events Events |
              Comp EventsType Declaration Condition Expression deriving (Show,Eq)

data Process = Styling String Process String | Stop | Skip | Prefix Event Process |
               Extern Process Process | Timeout Process Process | ProcId ProcVar |
               Intern Process Process | Inter Process Process | Hide Process Events |
               Interrupt Process Process | SeqComp Process Process |
               Parcomp (Events,Process) (Events,Process) | Parinter Process Events Process |
               Indseqcomp (Var,Events) Process | Indextern (Var,Events) Process |
               Indintern (Var,Events) Process | Indparcomp (Var,Events) Events Process |
               Indinter (Var,Events) Process | Indparinter (Var,Events) Events Process |
               IfThenElse Condition Process Process deriving (Show,Eq)

type ProcessDef = (ProcVar,[Local],Process)
type SetDef = (SetName,Events)

data DataType = DList DataName [String] | DSet DataName Events deriving (Show,Eq)
data Data = DN DataName | DS Events deriving (Show,Eq)
data Channel = NData [String] | TData [String] [Data] deriving (Show,Eq)
data Local = LP ProcessDef | LS SetDef deriving (Show,Eq)
data Model = T | F | FD | R deriving (Show,Eq)

data Specification = Deter Model Process | Refine Model Process Process |
                     Deadlock Model Process | Livelock Process deriving (Show,Eq)

data Script = Script [DataType] [Channel] [ProcessDef] [SetDef] [Specification] deriving (Show,Eq)

datatype :: Script -> [DataType]
datatype (Script ds cs ps es sp) = ds

getDName :: DataType -> DataName
getDName (DList d ds) = d
getDName (DSet d es) = d

getData :: DataType -> Either [String] Events
getData (DList d ds) = Left ds
getData (DSet d es) = Right es

showdata :: DataType -> String
showdata (DList dn []) = ""
showdata (DList dn ls) = "datatype " ++ dn ++ " = " ++ sd (nub ls) ++ "\n"
showdata (DSet dn (List Set es)) = "nametype " ++ dn ++ " = " ++ CSP.showList Set es ++ "\n"
showdata _ = error "Incorrect comprehension\n"

sd :: [String] -> String
sd [l] = l
sd (l:ls) = l ++ " | " ++ sd ls

showDataType :: Script -> String
showDataType (Script ds cs ps es sp) = concatMap showdata ds

channels :: Script -> [Channel]
channels (Script ds cs ps es sp) = cs

getChName :: Channel -> [String]
getChName (NData ss) = ss
getChName (TData ss d) = ss

isCompound :: Channel -> Bool
isCompound (TData ss d) = True
isCompound _ = False

getCType :: Channel -> [Data]
getCType (TData ss d) = d

showChannels :: Script -> String
showChannels (Script ds cs ps es sp) = concatMap showchannel cs
  where showchannel (NData cs) = "channel " ++ sc cs ++ "\n"
        showchannel (TData cs ds) = "channel " ++ sc cs ++ " : " ++ sd ds ++ "\n"
        sc [] = ""
        sc [c] = c
        sc (c:cs) = c ++ " , " ++ sc cs
        sd [d] = sd2 d
        sd (d:ds) = sd2 d ++ "." ++ sd ds
        sd2 (DN n) = n
        sd2 (DS (List t es)) = CSP.showList t es

normchannels :: String -> String -> [DataType] -> [Channel] -> [Channel]
normchannels d c ds cs =
  let dt x (DList d lists) = d == x && (not.null) lists
      ct x (TData c _) = notElem x c
      ct x (NData _) = True
  in case find (dt d) ds of
        Just _ -> cs
        Nothing -> filter (ct c) cs

events :: Script -> [SetDef]
events (Script ds cs ps es sp) = es

showEvents :: Script -> String
showEvents (Script ds cs ps es sp) = showevent es
  where showevent [] = ""
        showevent ((n,es):ns) = n ++ " = " ++ showSet es ++ "\n" ++ showevent ns

getSet :: Events -> [Event]
getSet (List t es) = es

showSet :: Events -> String
showSet (List t es) = CSP.showList t es
showSet (Comp t d c e) = showComp t d c e
showSet (Union e f) = "union(" ++ showSet e ++ "," ++ showSet f ++ ")"
showSet (Unions es) = "Union{" ++ showUnions es ++ "}"
showSet (Diff e f) = "diff(" ++ showSet e ++ "," ++ showSet f ++ ")"
showSet (SName sn) = sn

showUnions :: [Events] -> String
showUnions [e] = showSet e
showUnions (e:es) = showSet e ++ "," ++ showUnions es

showList :: EventsType -> [Event] -> String
showList t es
  | t == Set =  "{ " ++ sg (nub es) ++ " }"
  | t == Seq =  "< " ++ sg es ++ " >"
  | t == Chan = "{| " ++ sg (nub es) ++ " |}"
  where sg [] = ""
        sg [e] = e
        sg (e:es) = e ++ ", " ++ sg es

showComp :: EventsType -> Declaration -> Condition -> Expression -> String
showComp Set d [] e = "{ " ++ showExp e ++ " | " ++ showDecl1 d ++ " }"
showComp Set d c e = "{ " ++ showExp e ++ " | " ++ showDecl1 d ++ " , " ++ c ++ " }"
showComp _ d [] e = "< " ++ showExp e ++ " | " ++ showDecl1 d ++ " >"
showComp _ d c e = "< " ++ showExp e ++ " | " ++ showDecl1 d ++ " , " ++ c ++ " >"

showExp :: Expression -> String
showExp e = e

showDecl1 :: Declaration -> String
showDecl1 [(v,es)] = v ++ " <- " ++ showSet es
showDecl1 ((v,es):ds) = v ++ " <- " ++ showSet es ++ " , " ++ showDecl1 ds

showSpecs :: Script -> String
showSpecs (Script ds cs ps es sp) = concatMap showSpec sp

showSpec :: Specification -> String
showSpec (Deter m p) = "assert " ++ showScript2 p ++ " :[ deterministic [" ++ showModel m ++ "] ]\n"
showSpec (Refine m p q) = "assert " ++ showScript2 p ++ " [" ++ showModel m ++ "= " ++ showScript2 q ++ "\n"
showSpec (Deadlock m p) = "assert " ++ showScript2 p ++ " :[ deadlock free [" ++ showModel m ++ "] ]\n"
showSpec (Livelock p) = "assert " ++ showScript2 p ++ " :[ divergence free ]\n"

showModel :: Model -> String
showModel T = "T"
showModel F = "F"
showModel FD = "FD"
showModel R = "R"

showCSP :: Script -> String
showCSP s = showDataType s ++ "\n" ++ showChannels s ++ "\n" ++ showCompress ++ "\n" ++ showEvents s ++ "\n" ++ showProcess s ++ "\n" ++ showSpecs s ++ "\n"

showProcess :: Script -> String
showProcess (Script ds cs ps es sp) = (showScript . sortBy (flip nameComp)) ps

nameComp :: ProcessDef -> ProcessDef -> Ordering
nameComp (pn,ls,pc) (pn2,ls2,pc2)
  | cmp1 > cmp2 = GT
       | cmp2 < cmp1 = LT
  | otherwise = EQ
  where cmp1 = takeWhile (/='(') pn
        cmp2 = takeWhile (/='(') pn2

showScript :: [ProcessDef] -> String
showScript [] =  ""
showScript ((v,[],p):ps) = v ++ " = " ++ showScript2 p ++ "\n\n" ++ showScript ps
showScript ((v,ls,p):ps) = "\n"++ v ++ " =\nlet\n" ++ concatMap ((++"\n").(++) "\t") ((lines.showLocal) ls) ++ "within\n\t" ++ showScript2 p ++ "\n\n" ++ showScript ps

showLocal :: [Local] -> String
showLocal [] = ""
showLocal ( LP p :ls) = showScript [p] ++ showLocal ls
showLocal ( LS s :ls) = showEvents (Script [] [] [] [s] []) ++ showLocal ls

showScript2 :: Process -> String
showScript2 p = case p of
    Stop -> "STOP"
    Skip -> "SKIP"
    (Prefix x y) -> x ++ " -> " ++ "( " ++ showScript2 y ++ " )"
    (Extern x y) -> "( " ++ showScript2 x ++ " )" ++ " [] " ++ "( " ++ showScript2 y ++ " )"
    (Timeout x y) -> "( " ++ showScript2 x ++ " )" ++ " [> " ++ "( " ++ showScript2 y ++ " )"
    (Intern x y) -> "( " ++ showScript2 x ++ " )" ++ " |~| " ++ "( " ++ showScript2 y ++ " )"
    (Hide x e) -> "( " ++ showScript2 x ++ " )" ++ " \\ " ++ "( " ++ showSet e ++ " )"
    (Inter x y) -> "( " ++ showScript2 x ++ " )" ++ " ||| " ++ "( " ++ showScript2 y ++ " )"
    (Parcomp (e1,x) (e2,y)) -> "( " ++ showScript2 x ++ " )" ++ "[ " ++ showSet e1 ++ " || " ++ showSet e2 ++" ]" ++ "( " ++ showScript2 y ++ " )"
    (Interrupt x y) -> "( " ++ showScript2 x ++ " )" ++ " /\\ " ++ "( " ++ showScript2 y ++ " )"
    (Parinter y xs z) -> "( " ++ showScript2 y ++ " )" ++ " [| " ++ showSet xs ++ " |] " ++ "( " ++ showScript2 z ++ " )"
    (SeqComp y z) -> "( " ++ showScript2 y ++ " )" ++ " ; " ++ "( " ++ showScript2 z ++ " )"
    (Indextern (v,vs) z) -> "[] "++ v ++ ":" ++ showSet vs ++ " @ " ++ "( " ++ showScript2 z ++ " )"
    (Indintern (v,vs) z) -> "|~| "++ v ++ ":" ++ showSet vs ++ " @ " ++ "( " ++ showScript2 z ++ " )"
    (Indparcomp (v,vs) e z) -> "|| "++ v ++ ":" ++ showSet vs ++ " @ " ++ "[ " ++ showSet e ++ " ] " ++ "( " ++ showScript2 z ++ " )"
    (Indinter (v,vs) z) -> "||| "++ v ++ ":" ++ showSet vs ++ " @ " ++ "( " ++ showScript2 z ++ " )"
    (Indparinter (v,vs) e z) -> "[| " ++ showSet e ++ " |] " ++ v ++ ":" ++ showSet vs ++ " @ " ++ "( " ++ showScript2 z ++ " )"
    (IfThenElse c x y) -> "if " ++ c ++ " then " ++ showScript2 x ++ " else " ++ showScript2 y
    (Styling s p t) -> s ++ showScript2 p ++ t
    (ProcId x) -> x

processes :: Script -> [ProcessDef]
processes (Script ds cs ps es sp) = ps

showCSPFile :: Script -> FilePath -> IO()
showCSPFile i j = writeFile j (showCSP i)

showCompress :: String
showCompress = "transparent diamond, sbisim\ncompress(P) = sbisim(diamond(P))\n"
