module BPMN where

type Seqflow = String
type Mgeflow = String
type TaskName = String
type BName = String
type ElementId = String
type PoolId = String
type DiagramId = String
type ErrorCode = String
type Loop = Int

data Inv = Inv IName IDose IMethod | NoDetail deriving (Eq,Show)

type IName = String
type IDose = String
type IMethod = String

data Loops = Fix Int | Ndet Int deriving (Eq,Show)
data FlowType = One | All deriving Show

data TaskType = DptT | InvT Inv | None deriving (Eq,Show)
data SubProcessType = Embedded | SeqB | RepB | DptB | InvB deriving (Eq,Show)

data Exception = Exception ErrorCode | AnyException deriving Show

data Type = Itime Time | Stime Time | Ierror Exception | Eerror Exception | Start | End |
            Smessage (Maybe Mgeflow) | Imessage (Maybe Mgeflow) | Emessage (Maybe Mgeflow) |
            Agate | Xgate | Exgate | Task TaskName TaskType |
            SubProcess BName SubProcessType |
            Miseq TaskName TaskType Loops FlowType |
            Miseqs BName SubProcessType Loops FlowType |
            Mipar TaskName TaskType Loops FlowType |
            Mipars BName SubProcessType Loops FlowType deriving Show

data Time = Time Integer Integer Integer Integer Integer Integer deriving Show

type Range = (Time,Time)

data Atom =  Atom {eid :: ElementId, etype :: Type, ins,outs :: [Seqflow],
                   exit :: [(Type,Seqflow)], range :: Range,
                   receive, send :: [Mgeflow]} deriving Show

data Element = Atomic Atom | Compound Atom [Element] deriving Show

type Diagram = [(PoolId,[Element])]

type BPMN = (DiagramId,Diagram)

zero = Time 0 0 0 0 0 0
norange = (zero,zero)
