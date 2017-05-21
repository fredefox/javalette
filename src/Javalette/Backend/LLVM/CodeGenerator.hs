{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
module Javalette.Backend.LLVM.CodeGenerator
  ( compileProg
  , CompilerErr
  ) where

import Control.Monad.Except
import Control.Monad.Writer (execWriterT)
import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as M

import qualified Javalette.Syntax as Jlt
import qualified Javalette.Backend.LLVM.Language as LLVM
import Javalette.PrettyPrint
import Javalette.TypeChecking (LValue(..), lvalue)
import Javalette.Backend.LLVM.Renamer (rename)
import Javalette.Backend.LLVM.Errors
import qualified Javalette.Backend.LLVM.StringCollector as StringCollector ( collect )

import Javalette.Debug

-- | The environment carries around information for generating unique labels.
data Env = Env
  -- Each time a variable is encountered it is mapped to the next number in a
  -- sequence.
  { counterLabels :: Int
  , counterRegs   :: Int
  } deriving (Show)

-- | Increments and returns the value of of the counter for labels.
incrCounterLabels :: MonadState Env m => m Int
incrCounterLabels = do
  lbl <- gets counterLabels
  modify (\e -> e { counterLabels = succ lbl })
  return lbl

-- | Increments and returns the value of of the counter for registers.
incrCounterRegs :: MonadState Env m => m Int
incrCounterRegs = do
  lbl <- gets counterRegs
  modify (\e -> e { counterRegs = succ lbl })
  return lbl

-- | Generate a unique label.
newLabel :: MonadState Env m => m LLVM.Label
newLabel = LLVM.Label . ('l':) . show <$> incrCounterLabels

-- | Generate a unique label with a given prefix (prefix is currently disabled).
newLabelNamed :: MonadCompile m => String -> m LLVM.Label
-- newLabelNamed s = LLVM.Label . (s ++ ) . show <$> incrCounterLabels
newLabelNamed _ = newLabel

-- | Generate a unique register.
newReg :: MonadCompile m => m LLVM.Name
newReg = LLVM.Local . ('t':) . show <$> incrCounterRegs

-- | Gets the llvm constant representing the given name or an error if no such
-- string can be found.
lookupString :: MonadCompile m => String -> m LLVM.Name
lookupString s = do
  m <- asks envConstants
  case M.lookup s m of
    Nothing -> throwError (Generic "Could not find string constant")
    Just x -> return x

-- | The initial compilation environment.
emptyEnv :: Env
emptyEnv = Env 0 0

-- | Contains mappings from names to string constants and functions.
data ReadEnv = ReadEnv
  { envConstants :: Constants
  , envFunctions :: Functions
  }

-- | Maps string values to the constant declaring them.
type Constants = Map String LLVM.Name

-- | Maps functions to their argument types.
type Functions = Map String [LLVM.Type]

-- | Lookup a global constant representing a function.
lookupFunction :: MonadCompile m => LLVM.Name -> m [LLVM.Type]
lookupFunction (LLVM.Global n) = do
  m <- asks envFunctions
  case M.lookup n m of
    Nothing -> throwError (Generic $ "Could not find function" ++ show n)
    Just f -> return f
lookupFunction _ = error "Functions are declared globally"

-- | Synonym for 'lookupFunction'.
getArgTypes :: MonadCompile m => LLVM.Name -> m [LLVM.Type]
getArgTypes = lookupFunction

-- | The compilation context.
--
-- Contains effects for
--   * Writing output
--   * Generating unique names
--   * Failing
--   * Looking up globally declared constants
type MonadCompile m =
  ( MonadWriter [AlmostInstruction] m
  , MonadState Env m
  , MonadError CompilerErr m
  , MonadReader ReadEnv m
  )

-- | Translate a Javalette program to it's LLVM representation.
compileProg :: Jlt.Prog -> Either CompilerErr LLVM.Prog
compileProg = runExcept . compileProgM . rename

-- | The translation function inside an error monad.
--
-- Note in particular that the RWS-effects are *not* shared across javalette
-- top-level definitions.
compileProgM :: MonadError CompilerErr m => Jlt.Prog -> m LLVM.Prog
compileProgM (Jlt.Program defs) = do
  pDefs <- mapM (trTopDef re) defs
  return LLVM.Prog
    { LLVM.pGlobals = map declString (M.toList cs)
    , LLVM.pDecls   = builtinDecls
    , LLVM.pDefs    = pDefs
    }
  where
    cs        = constantsMap $ concatMap StringCollector.collect defs
    declTypes = map (\d -> (unname (LLVM.declName d), LLVM.declArgs d)) builtinDecls
    defTypes  = map (\d -> (unname (trTopDefName d), trArgType d)) defs
    re        = ReadEnv cs (M.fromList (declTypes ++ defTypes))
    constantsMap :: [String] -> Constants
    constantsMap xs = M.fromList . map (fmap intToReg) $ zip xs [0..]
    intToReg :: Int -> LLVM.Name
    intToReg = LLVM.Global . (:) 's' . show
    declString :: (String, LLVM.Name) -> LLVM.GlobalVar
    declString (s, r) = LLVM.GlobalVar
      { LLVM.gvName = r , LLVM.gvType = stringType s , LLVM.gvVal = LLVM.Constant (s ++ "\00") }
    trArgType (Jlt.FnDef _ _ args _) = map argType args
    trTopDefName (Jlt.FnDef _ i _ _) = trName i
    unname :: LLVM.Name -> String
    unname (LLVM.Global s) = s
    unname (LLVM.Local s) = s

-- | Translates a javalette function definition to an llvm function definitions
-- inside the error monad.
trTopDef
  :: MonadError CompilerErr m
  => ReadEnv -> Jlt.TopDef -> m LLVM.Def
trTopDef re (Jlt.FnDef t i args blk) = do
  (_, _, bss) <- (\act -> runRWST act re emptyEnv) $ do
    entry <- newLabel
    fallThrough <- newLabel
    emitLabel entry
    mapM_ cgArg args
    trBlk fallThrough blk
    emitLabel fallThrough
  return LLVM.Def
    { LLVM.defType = trType t
    , LLVM.defName = trName i
    , LLVM.defArgs = map trArg args
    , LLVM.defBlks = almostToBlks bss
    }

-- | A simple mapping between a javalette argument and an llvm argument.
trArg :: Jlt.Arg -> LLVM.Arg
trArg (Jlt.Argument t i) = LLVM.Arg (trType t) (trNameToRegArg i)

-- | Code generation for an argument. Allocates memory and initializes the variable.
cgArg :: MonadCompile m => Jlt.Arg -> m ()
cgArg (Jlt.Argument t i) = varInit (trType t) (trNameToReg i) (Left (trNameToRegArg i))

-- | Translates a block.
trBlk
  :: MonadCompile m
  => LLVM.Label -> Jlt.Blk -> m ()
trBlk fallthrough (Jlt.Block stmts) = mapM_ (trStmt fallthrough) stmts

-- | `AlmostInstruction` is a simple sum-type of llvm- labels, instructions and
-- terminator instruction. These are easier to work with when doing
-- code-generation. They are then translated to actual llvm-blocks in
-- 'almostToBlks'. The name could perhaps have been better.
data AlmostInstruction
  = Label LLVM.Label
  | Instr LLVM.Instruction
  | TermInstr LLVM.TermInstr
  deriving (Show)

-- | Is it a label?
isLabel :: AlmostInstruction -> Bool
isLabel a = case a of Label{} -> True ; _ -> False

-- | Is it a terminator instruction?
isTermInstr :: AlmostInstruction -> Bool
isTermInstr a = case a of TermInstr{} -> True ; _ -> False

-- | Unwraps a terminator instruction.
unTermInstr :: AlmostInstruction -> LLVM.TermInstr
unTermInstr a = case a of TermInstr i -> i ; _ -> undefined

-- | The code-generator works with 'AlmostInstruction' for simplicity. These are
-- then mapped to llvm-blocks in this function. This function ensures that the
-- instructions are actually semantically correct according to llvm. In some
-- cases it will try to fixup the input. Like if the block it was just about to
-- assembly didn't end in a terminator instruction, then it will insert a
-- `unreachable`. This is, however, probably not what you want.
--
-- The implementation may look daunting, but it basically just stitches the flat
-- `[AlmostInstruction]` structure together into `[LLVM.Blk]`.
almostToBlks :: [AlmostInstruction] -> [LLVM.Blk]
almostToBlks = map (uncurry4 LLVM.Blk . combineLabels) . sepLbls
  where
    sepLbls :: [AlmostInstruction] -> [(LLVM.Label, [AlmostInstruction])]
    sepLbls (l@Label{} : xs) = map go . sepBy isLabel l $ xs
      where
        go (Label l', xs') = (l', xs')
        go _             = impossible "almostToBlks.sepLbkls.go"
    sepLbls _            = impossible "almostToBlks.sepLbls"
    combineLabels
      :: (LLVM.Label, [AlmostInstruction])
      -> (LLVM.Label, [LLVM.Instruction], LLVM.TermInstr, [LLVM.TermInstr])
    combineLabels (l, as) = (l, a, b, case c of [] -> [] ; _ -> tail c)
      where
        (a, b, c) = combineLabels' as
    combineLabels' :: [AlmostInstruction] -> ([LLVM.Instruction], LLVM.TermInstr, [LLVM.TermInstr])
    combineLabels' xs = foldl go ([], firstTi, []) xs
      where
        go (is, ti, tis) i = case i of
          Label{} -> undefined
          Instr i' -> (is ++ [i'], ti, tis)
          TermInstr i' -> (is, ti, tis ++ [LLVM.CommentedT i'])
        firstTi = unTermInstr $ head' (filter isTermInstr xs)
        head' [] = TermInstr LLVM.Unreachable
        head' (x:_) = x

-- | Use for yuuuuge profit!
uncurry4 :: (g -> r -> e -> a -> t) -> (g, r, e, a) -> t
uncurry4 g (r, e, a, t) = g r e a t

-- | @'sepBy' p i xs@ groups together all subsequent elements 'xs' from some
-- element 'x' that match the predicate 'p'. 'i' is the element in the first
-- group. An example is perhaps in order:
--
--     sepBy (== 2) 222 . take 10 . cycle $ [0..3]
--       = [(222,[0,1]),(2,[3,0,1]),(2,[3,0,1])]
--
sepBy :: (a -> Bool) -> a -> [a] -> [(a, [a])]
sepBy p d = synth . foldl go ((d, []), [])
  where
    -- go :: ([a], [(a, [a])]) -> a -> ([a], [(a, [a])])
    go (prev@(a, as), acc) x = if p x
      then ((x, [])       , acc ++ [prev])
      else ((a, as ++ [x]), acc)
    synth (prev, acc) = acc ++ [prev]

-- TODO Outputting a comment with new-lines is a no-go
-- | Writes instructions for a statement.
trStmt
  :: MonadCompile m
  => LLVM.Label -> Jlt.Stmt -> m ()
trStmt fallThrough s = {-instrComment ("[STATEMENT]: " ++ prettyShow s) *> -} case s of
  Jlt.Empty -> return ()
  Jlt.BStmt b -> trBlk fallThrough b
  Jlt.Decl typ its -> mapM_ (itemInit typ) its
  Jlt.Ass i e -> assign i e
  Jlt.Incr{} -> undefined "IMPOSSIBLE - removed by typechecker"
  Jlt.Decr{} -> undefined "IMPOSSIBLE - removed by typechecker"
  Jlt.Ret e -> llvmReturn e
  Jlt.VRet -> llvmVoidReturn
  Jlt.Cond e s0 -> do
    t <- newLabel
    l <- newLabel
    cond e t l
    emitLabel t
    trStmt l s0
    jumpToNew l
  Jlt.CondElse e s0 s1 -> do
    t <- newLabel
    f <- newLabel
    l <- newLabel
    cond e t f
    emitLabel t
    cont s0
    jumpTo l
    emitLabel f
    cont s1
    jumpTo l
    emitLabel l
  Jlt.While e s0 -> do
    lblCond <- newLabelNamed "whileCond"
    lblBody <- newLabelNamed "whileBody"
    lblAfterAWhile <- newLabel
    jumpToNew lblCond
    cond e lblBody lblAfterAWhile
    emitLabel lblBody
    trStmt lblCond s0
    jumpTo lblCond
    emitLabel lblAfterAWhile
  Jlt.SExp e -> void $ resultOfExpression e
  Jlt.For{} -> impossibleRemoved
  where
    cont = trStmt fallThrough

-- | Code-generation for a javalette assignment statement.
assign :: MonadCompile m
  => Jlt.Expr -> Jlt.Expr -> m ()
assign lv e = instrComment "assign" >> case lvalue lv of
  LValue i [] -> do
    op <- resultOfExpression e
    let reg    = trNameToReg i
        tpLLVM = trType (typeof e)
    instrStore tpLLVM op reg
  LValue i idxs -> do
    op <- resultOfExpression e
    let tpElemsJlt  = typeof e
        tpElemsLLVM = trType tpElemsJlt
        nestedArrayTypes
          = reverse
          . take (length idxs + 1)
          . iterate arrayLLVM
          $ tpElemsLLVM
        loadArrayPointer
          :: MonadCompile m
          => LLVM.Name -> [((LLVM.Type, LLVM.Operand), LLVM.Type)] -> m LLVM.Name
        loadArrayPointer reg [] = return reg
        loadArrayPointer reg
          ((idx, tpStructLLVM@(LLVM.Struct (_:LLVM.Pointer tpElems:_))):idxs')= do
          let p = LLVM.Pointer
          r0 <- instrGep tpStructLLVM reg
              [(LLVM.I 32, intOp 0), (LLVM.I 32, intOp 1)]
          r1 <- instrLoad (p tpElems) r0
          r2 <- instrGep tpElems r1 [idx]
          loadArrayPointer r2 idxs'
        loadArrayPointer _ _ = error "Not the llvm representation of a javalette array"
    idxsLLVM <- mapM typeValueOfIndex idxs
    ptr <- loadArrayPointer (trNameToReg i) (zip idxsLLVM nestedArrayTypes)
    instrStore tpElemsLLVM op ptr

-- | Int to llvm operand.
intOp :: Int -> LLVM.Operand
intOp = Right . LLVM.ValInt

-- XXX Surely the type of the index will always be an integer.
-- | The type and value of a javalette index-value.
typeValueOfIndex :: MonadCompile m => Jlt.Index -> m (LLVM.Type, LLVM.Operand)
typeValueOfIndex (Jlt.Indx e) = do
  r <- resultOfExpression e
  return (trType $ typeof e, r)

-- | The type of an (annotated) javalette expression. If the expression is not
-- annotated, then that is an error (presumably caused by the typechecker that
-- does this sort of stuff).
typeof :: Jlt.Expr -> Jlt.Type
typeof (Jlt.EAnn tp _) = tp
typeof _ = typeerror "All expressions should've been annotated by the type-checker."

-- | Code-generation for a return-statement.
llvmReturn
  :: MonadCompile m
  => Jlt.Expr -> m ()
llvmReturn e = do
  op <- resultOfExpression e
  let tp = trType (typeof e)
  emitTerminator (LLVM.Return tp op)

-- | Switches on the result of code-generation of an expression. Jumps to either
-- of the two provided labels.
cond :: MonadCompile m
  => Jlt.Expr -> LLVM.Label -> LLVM.Label -> m ()
cond e t f = do
  op <- resultOfExpression e
  emitTerminator $ LLVM.BranchCond op t f

-- | Assumes that the item is already initialized and exists in scope.
itemInit
  :: MonadCompile m
  => Jlt.Type -> Jlt.Item -> m ()
itemInit jltType itm = instrComment "init" >> do
  -- (reg, tp) <- undefined -- lookupItem itm >>= maybeToErr' (Generic "var init - cant find")
  let tp :: LLVM.Type
      tp = trType jltType
      reg :: LLVM.Name
      reg = trIdent . itemName $ itm
  case itm of
    Jlt.NoInit{} -> varInit tp reg (defaultValue jltType)
    -- Here we must first compute the value of the expression
    -- and store the result of that in the variable.
    Jlt.Init _ e -> do
      reg0 <- resultOfExpression e
      varInit tp reg reg0
    Jlt.InitObj _ (Jlt.ArrayCon Jlt.TypeCon{} e) -> do
      len <- resultOfExpression e
      arrayInit tp (typeOfElements jltType) reg len
    Jlt.InitObj _ cs -> do
      cts <- conTypes cs
      instrComment "nested arrays"
      multidimArrayInit reg cts

-- | Initializes a multi-dimensional array.
multidimArrayInit :: MonadCompile m => LLVM.Name -> [(LLVM.Type, LLVM.Operand)] -> m ()
multidimArrayInit n xs@((t, _):_) = do
  emitInstruction $ LLVM.Alloca t n
  let os = map snd xs
  void $ generalizedArrayInit t n os
multidimArrayInit _ _ = error "Expecting at least one array"

-- | Represent a loop in llvm. The call-back is handed an iterator and writes
-- the loop-body.
llvmLoop :: MonadCompile m => LLVM.Operand -> (LLVM.Operand -> m a) -> m ()
llvmLoop len act = do
  counter <- newCounter
  start <- newLabel
  emitTerminator $ LLVM.Branch start
  emitLabel start
  cVal <- incrCounter counter
  _ <- act cVal
  r0 <- instrIcmp LLVM.ULT (LLVM.I 32) cVal len
  stop  <- newLabel
  emitTerminator $ LLVM.BranchCond (Left r0) start stop
  emitLabel stop

-- | Initializes a new counter in llvm and returns it's name.
newCounter :: MonadCompile m => m LLVM.Name
newCounter = do
  let i32 = LLVM.I 32
  r <- instrAlloca i32
  instrStore i32 (intOp 0) r
  return r

-- | Returns the value that the counter had before incrementing it.
incrCounter :: MonadCompile m => LLVM.Name -> m LLVM.Operand
incrCounter n = do
  let i32 = LLVM.I 32
  r0 <- instrLoad i32 n
  r1 <- instrBinOp LLVM.Add i32 (Left r0) (intOp 1)
  instrStore i32 (Left r1) n
  -- TODO: Think it has to be `r0` here
  -- return (Left r1)
  return (Left r1)

-- It should be the case that
--
--     arrayInit tp _ = (void.) . generalizedArrayInit tp
--
-- | A generalized version of 'arrayInit' for multi-dimensional arrays.
generalizedArrayInit :: MonadCompile m => LLVM.Type -> LLVM.Name -> [LLVM.Operand] -> m ()
generalizedArrayInit structTp@(LLVM.Struct (_:LLVM.Pointer arrayElemTp:_)) array (len:lens) = do
  r0 <- newReg
  lengthOfArrayLLVM structTp array r0
  -- r1 <- newReg
  instrStore (LLVM.I 32) len r0
  r1 <- instrGep structTp array [(LLVM.I 32, intOp 0), (LLVM.I 32, intOp 1)]
  llvmLoop len $ \idx -> do
    let nestedTp = p arrayElemTp
    nstdp <- instrGep nestedTp r1 [(LLVM.I 32, idx)]
    ralc <- instrAlloca arrayElemTp
    instrStore nestedTp (Left ralc) nstdp
    case arrayElemTp of
      LLVM.Struct{} -> do
        nstd <- instrLoad nestedTp nstdp
        instrComment $ "now we wanna init "
            ++ prettyShow nstd ++ " which has type " ++ prettyShow (p arrayElemTp)
        generalizedArrayInit arrayElemTp nstd lens
      _             -> do
        instrComment $ "hit base-case: " ++ prettyShow arrayElemTp
        r2 <- instrCall i8p (LLVM.Global "calloc")
            [(LLVM.I 32, len), (LLVM.I 32, sizeOf arrayElemTp)]
        r3 <- instrBitCast i8p r2 nestedTp
        instrStore nestedTp (Left r3) nstdp
  where
    sizeOf t = Right $ LLVM.ValInt $ case t of
      LLVM.I n -> n `div` 8
      LLVM.Double -> 8
      _ -> error "malloc will not be called on other types"
    i8   = LLVM.I 8
    p    = LLVM.Pointer
    i8p  = p i8
generalizedArrayInit _ _ _ = error "LLVM-type does not represent an array"

-- | Returns the list of structs representing arrays that must be initialized
-- along with its length.
conTypes :: MonadCompile m => Jlt.Constructor -> m [(LLVM.Type, LLVM.Operand)]
conTypes c = case c of
  Jlt.ArrayCon c' e -> do
      len <- resultOfExpression e
      tps@((tp, _):_) <- case c' of
          Jlt.TypeCon t -> return [(trType t, len)]
          _             -> conTypes c'
      return $ (arrayLLVM tp,len):tps
  _ -> error "Not a constructor"

-- | The type of elements in an array.
typeOfElements :: Jlt.Type -> LLVM.Type
typeOfElements t = case t of
  Jlt.Array t' -> trType t'
  _ -> typeerror "Expected array"

-- We could generalize this to general structs and just say that the *type*:
--
--     t[n]
--
-- is equivalent to the c type:
--
--     struct { const int length ; t value[n] ; }
--
-- Where `t` is the type of the array and `n` is the length.
-- | Code-generation for a one-dimensional array.
arrayInit :: MonadCompile m => LLVM.Type -> LLVM.Type -> LLVM.Name -> LLVM.Operand -> m ()
arrayInit structTp arrayElemTp array len = do
  r0 <- newReg
  emitInstruction $ LLVM.Alloca structTp array
  lengthOfArrayLLVM structTp array r0
  instrStore (LLVM.I 32) len r0
  let artp = sndStructLLVM structTp
      i8   = LLVM.I 8
      i8p  = LLVM.Pointer i8
  r1 <- instrGep structTp array [(LLVM.I 32, intOp 0), (LLVM.I 32, intOp 1)]
  r2 <- instrCall i8p (LLVM.Global "calloc")
      [(LLVM.I 32, len), (LLVM.I 32, sizeOf arrayElemTp)]
  r3 <- instrBitCast i8p r2 artp
  instrStore artp (Left r3) r1
  where
    sizeOf t = Right $ LLVM.ValInt $ case t of
      LLVM.I n -> n `div` 8
      LLVM.Double -> 8
    sndStructLLVM (LLVM.Struct (_:x:_)) = x

-- | Code generation for accessing the lenght of an array.
lengthOfArrayLLVM :: MonadCompile m
  => LLVM.Type -> LLVM.Name -> LLVM.Name -> m ()
lengthOfArrayLLVM t n0 n1 =
  emitInstruction $ LLVM.GetElementPtr t (LLVM.Pointer t) n0
    [(LLVM.I 32, intOp 0), (LLVM.I 32, intOp 0)] n1

-- | Code-generation for initializing a variable with the given primitive type.
varInit
  :: MonadCompile m
  => LLVM.Type -> LLVM.Name -> LLVM.Operand -> m ()
varInit tp reg op = emitInstructions
  [ LLVM.Alloca tp reg
  , LLVM.Store tp op (LLVM.Pointer tp) reg
  ]

-- | Map javalette names (that are all unique thanks to meddling type-checker)
-- to llvm names.
trIdent :: Jlt.Ident -> LLVM.Name
trIdent (Jlt.Ident s) = LLVM.Local s

-- | The default value of a javalette type.
defaultValue :: Jlt.Type -> LLVM.Operand
defaultValue t = case t of
  Jlt.Int -> Right (LLVM.ValInt 0)
  Jlt.Doub -> Right (LLVM.ValDoub 0)
  _   -> impossible "Can only initialize ints and doubles"

-- | The name of an item.
itemName :: Jlt.Item -> Jlt.Ident
itemName itm = case itm of
  Jlt.NoInit i -> i
  Jlt.Init i _  -> i
  Jlt.InitObj i _ -> i

-- | Map a javalette identifier to an llvm global constant.
trName :: Jlt.Ident -> LLVM.Name
trName (Jlt.Ident s) = LLVM.Global s

-- | Map a javalette identifier to an llvm local identifier.
trNameToReg :: Jlt.Ident -> LLVM.Name
trNameToReg (Jlt.Ident s) = LLVM.Local s

-- | Map a javalette identifier to an llvm local identifier with a suffix. Used
-- for initializing arguments.
trNameToRegArg :: Jlt.Ident -> LLVM.Name
trNameToRegArg (Jlt.Ident s) = LLVM.Local (s ++ ".val")

-- | The type of a javalette argument.
argType :: Jlt.Arg -> LLVM.Type
argType (Jlt.Argument t _) = trType t

-- | Map javalette types to llvm-types.
trType :: Jlt.Type -> LLVM.Type
trType t = case t of
  Jlt.Int -> LLVM.I 32
  Jlt.Doub -> LLVM.Double
  Jlt.Bool -> LLVM.I 1
  Jlt.Void -> LLVM.Void
  Jlt.String -> impossible
    $  "The string type cannot be translated directly. "
    ++ "Strings of different length have different types"
  Jlt.Fun _t _tArgs -> undefined
  Jlt.Array t0 -> arrayLLVM (trType t0)

-- | All the built-in functions.
builtinDecls :: [LLVM.Decl]
builtinDecls =
  [ LLVM.Decl
    { LLVM.declType = LLVM.Void
    , LLVM.declName = LLVM.Global "printInt"
    , LLVM.declArgs = [LLVM.I 32]
    }
  , LLVM.Decl
    { LLVM.declType = LLVM.I 32
    , LLVM.declName = LLVM.Global "readInt"
    , LLVM.declArgs = []
    }
  , LLVM.Decl
    { LLVM.declType = LLVM.Void
    , LLVM.declName = LLVM.Global "printDouble"
    , LLVM.declArgs = [LLVM.Double]
    }
  , LLVM.Decl
    { LLVM.declType = LLVM.Double
    , LLVM.declName = LLVM.Global "readDouble"
    , LLVM.declArgs = []
    }
  , LLVM.Decl
    { LLVM.declType = LLVM.Void
    , LLVM.declName = LLVM.Global "printString"
    , LLVM.declArgs = [LLVM.Pointer (LLVM.I 8)]
    }
  , LLVM.Decl
    { LLVM.declType = LLVM.Pointer (LLVM.I 8)
    , LLVM.declName = LLVM.Global "calloc"
    , LLVM.declArgs = [LLVM.I 32, LLVM.I 32]
    }
  ]

-- | The type used to represent (dynamic) arrays in llvm.
arrayLLVM :: LLVM.Type -> LLVM.Type
arrayLLVM t = LLVM.Struct [LLVM.I 32, LLVM.Pointer t]

-- | Grabs the llvm-type from the llvm-representation of an array.
-- The inverse of `arrayLLVM`.
elemTpLLVM :: LLVM.Type -> LLVM.Type
elemTpLLVM (LLVM.Struct [_, t]) = t
elemTpLLVM _ = undefined

-- | Code generation for an expression with the given type.
resultOfExpressionTp
  :: MonadCompile m
  => Jlt.Type -> Jlt.Expr -> m LLVM.Operand
resultOfExpressionTp tp e = case e of
  Jlt.EVar i -> do
    let tp' = trType tp
    r <- instrLoad tp' (trNameToReg i)
    return (Left r)
  Jlt.ELitInt x -> return $ Right (LLVM.ValInt $ fromInteger x)
  Jlt.ELitDoub d -> return $ Right (LLVM.ValDoub d)
  Jlt.ELitTrue -> return $ Right (LLVM.ValInt 1)
  Jlt.ELitFalse -> return $ Right (LLVM.ValInt 0)
  Jlt.EAnn tp' e' -> resultOfExpressionTp tp' e'
  Jlt.EApp i es -> do
    es' <- mapM resultOfExpression es
    Left <$> call (trType tp) (trName i) es'
  Jlt.ERel e0 op e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    r <- newReg
    let tp' = trType $ typeof e0
        i   = case tp' of
          LLVM.I{} -> LLVM.Icmp
          LLVM.Double -> LLVM.Fcmp
          _ -> typeerror "Expected numerical type"
    emitInstructions [i (relOp op tp') tp' r0 r1 r]
    return (Left r)
  Jlt.EMul e0 op e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    let tp' = trType tp
    r <- instrBinOp (mulOp op tp') tp' r0 r1
    return (Left r)
  Jlt.EAdd e0 op e1 -> do
    r0 <- resultOfExpression e0
    r1 <- resultOfExpression e1
    let tp' = trType tp
    r <- instrBinOp (addOp op tp') tp' r0 r1
    return (Left r)
  Jlt.EAnd e0 e1 -> do
    t <- newLabel
    f <- newLabel
    l <- newLabel
    let tp' = LLVM.I 1
    res <- instrAlloca tp'
    cond e0 t f
    emitLabel t
    r1 <- resultOfExpression e1
    instrStore tp' r1 res
    jumpTo l
    emitLabel f
    instrStore tp' (Right (LLVM.ValInt 0)) res
    jumpTo l
    emitLabel l
    r <- instrLoad tp' res
    return (Left r)
  Jlt.EOr e0 e1 -> do
    t <- newLabel
    f <- newLabel
    l <- newLabel
    let tp' = LLVM.I 1
    res <- instrAlloca tp'
    cond e0 t f
    emitLabel f
    r1 <- resultOfExpression e1
    instrStore tp' r1 res
    jumpTo l
    emitLabel t
    instrStore tp' (Right (LLVM.ValInt 1)) res
    jumpTo l
    emitLabel l
    r <- instrLoad tp' res
    return (Left r)
  Jlt.Neg e0 -> do
    r0 <- resultOfExpression e0
    let tp' = trType tp
    r <- instrBinOp (addOp Jlt.Minus tp') tp' (zero tp') r0
    return (Left r)
  Jlt.Not e0 -> do
    r0 <- resultOfExpression e0
    let tp' = trType tp
        llvmTrue = LLVM.ValInt 1
    r <- instrBinOp LLVM.Xor tp' r0 (Right llvmTrue)
    return (Left r)
  Jlt.EString s -> do
    sReg <- lookupString s
    let tp' = stringType s
        path = [(LLVM.I 32, intOp 0), (LLVM.I 32, intOp 0)]
    r <- instrGep tp' sReg path
    return (Left r)
  Jlt.Dot e0 (Jlt.Ident i) -> do
    is <- execWriterT $ resultOfExpression e0
    unless (i == "length") (throwError (Generic $ "IMPOSSIBLE: " ++ i))
    (_, n) <- lastLoadToGep (const (LLVM.I 32))
      [(LLVM.I 32, intOp 0), (LLVM.I 32, intOp 0)] is
    return (Left n)
  Jlt.EIndex e0 idx -> do
    is <- execWriterT $ resultOfExpression e0
    (LLVM.Pointer tp', r) <- lastLoadToGep elemTpLLVM
      [(LLVM.I 32, intOp 0), (LLVM.I 32, intOp 1)] is
    idx' <- typeValueOfIndex idx
    r0 <- instrGep tp' r [idx']
    r1 <- instrLoad tp' r0
    return (Left r1)

-- | A safe version of last that also gives you the other elements.
splitLast :: [t] -> Maybe ([t], t)
splitLast [] = Nothing
splitLast [x] = Just ([], x)
splitLast (x:xs) = do
  (xs', l) <- splitLast xs
  return (x:xs', l)

-- | Used for accessing an element in an array rather than loading the whole
-- array.
--
-- Admittedly this is sort of a hack.
lastLoadToGep :: MonadCompile m
  => (LLVM.Type -> LLVM.Type)
  -> [(LLVM.Type, LLVM.Operand)]
  -> [AlmostInstruction]
  -> m (LLVM.Type, LLVM.Name)
lastLoadToGep f ops is = do
  r <- newReg
  case splitLast is of
    Just (xs, Instr (LLVM.Load t0 t1 n0 n1)) -> do
      let tp = f t0
      tell $ xs ++ map Instr
        [ LLVM.GetElementPtr t0 t1 n0 ops n1
        , LLVM.Load tp (LLVM.Pointer tp) n1 r
        ]
      return (tp, r)
    _ -> error "Last instruction was not load :("

-- | The zero value of the given javalette type in llvm.
zero :: LLVM.Type -> LLVM.Operand
zero t = case t of
  LLVM.I{} -> Right (LLVM.ValInt 0)
  LLVM.Double -> Right (LLVM.ValDoub 0)
  _ -> typeerror "no zero for non-numeric type"

-- The plus one is because we always append "\00" to the string we output which
-- is actually one character.
-- | The type of a string. Types of varying lengths have different types - hence
-- they depend on their value.
stringType :: String -> LLVM.Type
stringType s = LLVM.Array (length s + 1) (LLVM.I 8)

-- | Map a javalette (mult) operator into an llvm operator.
mulOp
  :: Jlt.MulOp
     -> LLVM.Type
     -> LLVM.Op
mulOp op tp = case tp of
  LLVM.I{} -> case op of
    Jlt.Times -> LLVM.Mul
    Jlt.Div   -> LLVM.SDiv
    Jlt.Mod   -> LLVM.SRem
  LLVM.Double -> case op of
    Jlt.Times -> LLVM.FMul
    Jlt.Div   -> LLVM.FDiv
    Jlt.Mod   -> typeerror "Can't take modulo of double values"
  _ -> typeerror "No mul-op for this type"

-- | Map a javalette (rel) operator into an llvm operator.
relOp
  :: Jlt.RelOp
  -> LLVM.Type
  -> LLVM.Comparison
relOp op tp = case tp of
  LLVM.I{} -> case op of
    Jlt.LTH -> LLVM.SLT
    Jlt.LE  -> LLVM.SLE
    Jlt.GTH -> LLVM.SGT
    Jlt.GE  -> LLVM.SGE
    Jlt.EQU -> LLVM.EQ
    Jlt.NE  -> LLVM.NE
  LLVM.Double -> case op of
    Jlt.LTH -> LLVM.OLT
    Jlt.LE  -> LLVM.OLE
    Jlt.GTH -> LLVM.OGT
    Jlt.GE  -> LLVM.OGE
    Jlt.EQU -> LLVM.OEQ
    Jlt.NE  -> LLVM.ONE
  _ ->  typeerror "No rel-op for this type"

-- | Map a javalette (add) operator into an llvm operator.
addOp
  :: Jlt.AddOp
     -> LLVM.Type
     -> LLVM.Op
addOp op tp = case tp of
  LLVM.I{} -> case op of
    Jlt.Plus  -> LLVM.Add
    Jlt.Minus -> LLVM.Sub
  LLVM.Double -> case op of
    Jlt.Plus  -> LLVM.FAdd
    Jlt.Minus -> LLVM.FSub
  _ -> typeerror "No add-op for this type"

-- | Code generation for annotated expressions. Calling this on a non-annotated
-- expression is an error.
resultOfExpression :: MonadCompile m
  => Jlt.Expr -> m LLVM.Operand
resultOfExpression e = case e of
  Jlt.EAnn tp e' -> resultOfExpressionTp tp e'
  _         -> impossible "was removed during type-checking"

-- NOTE `r` is only maybe used.
-- Call Type Name [(Type, Operand)] Reg
call :: MonadCompile m => LLVM.Type -> LLVM.Name -> [LLVM.Operand] -> m LLVM.Name
call t n ops = do
  opTypes <- getArgTypes n
  let tps = zip opTypes ops
  case t of
    -- NOTE: An unneccesarry register is allocated.
    LLVM.Void -> instrCallVoid t n tps >> newReg
    _ -> instrCall t n tps

-- * Helper methods emitting llvm-instructions

-- | Emit a instructions
emitInstructions :: MonadWriter [AlmostInstruction]  m => [LLVM.Instruction] -> m ()
emitInstructions = tell . map Instr

-- | Emit an instruction
emitInstruction :: MonadWriter [AlmostInstruction] m => LLVM.Instruction -> m ()
emitInstruction = emitInstructions . pure

-- | Emit a label
emitLabel :: MonadWriter [AlmostInstruction] m => LLVM.Label -> m ()
emitLabel = tell . pure . Label

-- | Emit a terminator instruction
emitTerminator :: MonadWriter [AlmostInstruction] m => LLVM.TermInstr -> m ()
emitTerminator = tell . pure . TermInstr

-- | Emit an assignment instruction
instrAssgn :: MonadCompile m => (LLVM.Name -> LLVM.Instruction) -> m LLVM.Name
instrAssgn f = do
  r <- newReg
  emitInstruction . f $ r
  return r

-- | Emit a binary operation instruction
instrBinOp :: MonadCompile m => LLVM.Op -> LLVM.Type -> LLVM.Operand -> LLVM.Operand -> m LLVM.Name
instrBinOp binOp tp op0 op1 = instrAssgn $ LLVM.BinOp binOp tp op0 op1

-- | Emit `alloca`
instrAlloca
  :: MonadCompile m
  => LLVM.Type -> m LLVM.Name
instrAlloca tp = instrAssgn $ LLVM.Alloca tp

-- | Emit `load`
instrLoad
  :: MonadCompile m
  => LLVM.Type
  -> LLVM.Name -> m LLVM.Name
instrLoad t0 n = instrAssgn $ LLVM.Load t0 (LLVM.Pointer t0) n

-- | Emit `getelementptr`
instrGep :: MonadCompile m
     => LLVM.Type
     -> LLVM.Name
     -> [(LLVM.Type, LLVM.Operand)]
     -> m LLVM.Name
instrGep t0 n ops = instrAssgn $ LLVM.GetElementPtr t0 (LLVM.Pointer t0) n ops

-- | Emit `store`
instrStore :: MonadCompile m
     => LLVM.Type
     -> LLVM.Operand
     -> LLVM.Name
     -> m ()
instrStore t0 op n = emitInstruction $ LLVM.Store t0 op (LLVM.Pointer t0) n

-- | Emit `icmp`
instrIcmp :: MonadCompile m
     => LLVM.Comparison
     -> LLVM.Type
     -> LLVM.Operand
     -> LLVM.Operand
     -> m LLVM.Name
instrIcmp c t op0 op1 = instrAssgn $ LLVM.Icmp c t op0 op1

-- | Emit `fcmp`
instrFcmp :: MonadCompile m
     => LLVM.Comparison
     -> LLVM.Type
     -> LLVM.Operand
     -> LLVM.Operand
     -> m LLVM.Name
instrFcmp c t op0 op1 = instrAssgn $ LLVM.Fcmp c t op0 op1

-- | Emit `call`
instrCall :: MonadCompile m
     => LLVM.Type
     -> LLVM.Name
     -> [(LLVM.Type, LLVM.Operand)]
     -> m LLVM.Name
instrCall t n ops = instrAssgn $ LLVM.Call t n ops

-- | Emit `call` with no return value
instrCallVoid :: MonadCompile m
     => LLVM.Type
     -> LLVM.Name
     -> [(LLVM.Type, LLVM.Operand)]
     -> m ()
instrCallVoid t n ops = emitInstruction $ LLVM.CallVoid t n ops

-- | Emit `bitcast`
instrBitCast :: MonadCompile m
     => LLVM.Type
     -> LLVM.Name
     -> LLVM.Type
     -> m LLVM.Name
instrBitCast t0 n t1 = instrAssgn $ LLVM.BitCast t0 n t1

-- | Emit a comment
instrComment :: MonadCompile m => String -> m ()
instrComment = emitInstruction . LLVM.Comment

-- | LLVM representation of void return.
llvmVoidReturn :: MonadCompile m => m ()
llvmVoidReturn = emitTerminator LLVM.VoidReturn

-- | @'jumpTo' lbl@ unconditional jump to @lbl@.
jumpTo :: MonadWriter [AlmostInstruction] m => LLVM.Label -> m ()
jumpTo lbl = emitTerminator (LLVM.Branch lbl)

-- | Create a new block and jump to it.
jumpToNew :: MonadWriter [AlmostInstruction] m => LLVM.Label -> m ()
jumpToNew lbl = do
  emitTerminator (LLVM.Branch lbl)
  emitLabel lbl
