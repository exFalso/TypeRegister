{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, TypeOperators, EmptyDataDecls, UndecidableInstances #-}

data Zero
data Succ n
-- for DEBUG
data Proxy t = Proxy

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five
type Seven = Succ Six
type Eight = Succ Seven
type Nine = Succ Eight
type Ten = Succ Nine

type family Update (v :: [*]) k (f :: * -> *) :: [*]
type instance Update (a ': v) Zero f = f a ': v
type instance Update (a ': v) (Succ k) f = a ': Update v k f

type family (:!:) (v :: [*]) k :: *
type instance (a ': v) :!: Zero = a 
type instance (a ': v) :!: (Succ n) = v :!: n

type family Eval i (instrs :: [*]) (regs :: [*])
type instance Eval (RP n ni) instrs regs = Eval (instrs :!: ni) instrs (Update regs n Succ)
type instance Eval (RM n ni1 ni2) instrs regs = Rep instrs regs n (regs :!: n) ni1 ni2
type instance Eval Stop instrs regs = regs :!: Zero
type instance Eval DEBUG instrs regs = Proxy regs

type family Rep (instrs :: [*]) (regs :: [*]) n reg ni1 ni2
type instance Rep instrs regs n Zero ni1 ni2 = Eval (instrs :!: ni2) instrs regs
type instance Rep instrs regs n (Succ r) ni1 ni2 = Eval (instrs :!: ni1) instrs (Replace regs n r)

type family Replace (v :: [*]) k x :: [*]
type instance Replace (a ': v) Zero x = x ': v
type instance Replace (a ': v) (Succ k) x = a ': Replace v k x

-- add one to reg[n] and jump to the (ni)th instruction
data RP n ni
-- subtract one from reg[n] and jump to (ni1)th instruction, or if reg[n]==0 jump to ni2(th) instr
data RM n ni1 ni2
-- return reg[0]
data Stop
-- return the current state of registers
data DEBUG

type RunRM instrs initConfig first = Eval (instrs :!: first) instrs initConfig

-- some examples

-- infinite loop
type InfRM = '[RP Zero Zero]
type InfInit = '[Zero]
type Inf = RunRM InfRM InfInit Zero

-- return r[0]
type RetRM = '[Stop]
type RetInit = '[Five]
type Ret = RunRM RetRM RetInit Zero

-- r[0] = r[0] + r[1]
type AddRM = '[ RM One One Two
              , RP Zero Zero
              , Stop
              ]
type AddInit = '[Two, Three]
type Add = RunRM AddRM AddInit Zero

-- r[0] = log2 (r[1])
type LogRM =  [ Stop
              , RM One Two Zero
              , RM One Three Zero
              , RP Zero Four
              , RP Two Five
              , RM One Six Eight
              , RM One Seven Eight
              , RP Two Five
              -- , DEBUG
              , RM Two Nine One
              , RP One Eight
              ]
type LogInit = '[Zero, Nine, Zero]
type Log = RunRM LogRM LogInit One
