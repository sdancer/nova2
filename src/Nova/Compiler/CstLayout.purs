-- | Layout algorithm for PureScript-style indentation-sensitive parsing.
-- | Ported from purescript-language-cst-parser/src/PureScript/CST/Layout.purs
module Nova.Compiler.CstLayout
  ( LayoutStack
  , LayoutDelim(..)
  , currentIndent
  , isIndented
  , insertLayout
  , lytToken
  , rootLayoutDelim
  ) where

import Prelude
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Nova.Compiler.Cst as Cst

type LayoutStack = List (Tuple Cst.SourcePos LayoutDelim)

data LayoutDelim
  = LytRoot
  | LytTopDecl
  | LytTopDeclHead
  | LytDeclGuard
  | LytCase
  | LytCaseBinders
  | LytCaseGuard
  | LytLambdaBinders
  | LytParen
  | LytBrace
  | LytSquare
  | LytIf
  | LytThen
  | LytProperty
  | LytForall
  | LytTick
  | LytLet
  | LytLetStmt
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo

derive instance eqLayoutDelim :: Eq LayoutDelim
derive instance ordLayoutDelim :: Ord LayoutDelim

-- Helper to return LytRoot (for modules that import this and need to construct a root stack)
rootLayoutDelim :: LayoutDelim
rootLayoutDelim = LytRoot

-- Must be defined before currentIndent
isIndented :: LayoutDelim -> Boolean
isIndented lyt = case lyt of
  LytLet -> true
  LytLetStmt -> true
  LytWhere -> true
  LytOf -> true
  LytDo -> true
  LytAdo -> true
  _ -> false

-- Helper for currentIndent - recursive, must be defined after isIndented
currentIndentGo :: LayoutStack -> Maybe Cst.SourcePos
currentIndentGo stk = case stk of
  Tuple pos lyt : rest ->
    if isIndented lyt
    then Just pos
    else currentIndentGo rest
  _ -> Nothing

currentIndent :: LayoutStack -> Maybe Cst.SourcePos
currentIndent = currentIndentGo

isTopDecl :: Cst.SourcePos -> LayoutStack -> Boolean
isTopDecl tokPos stack = case stack of
  Tuple lytPos lyt1 : rest ->
    if lyt1 == LytWhere
      then case rest of
        Tuple _ lyt2 : Nil -> lyt2 == LytRoot && tokPos.column == lytPos.column
        _ -> false
      else false
  _ -> false

lytToken :: Cst.SourcePos -> Cst.Token -> Cst.SourceToken
lytToken pos value =
  { range: { start: pos, end: pos }
  , leadingComments: Nil
  , trailingComments: Nil
  , value
  }

-- | Insert layout tokens based on the current token and layout state
-- | Returns the new layout stack and list of tokens (including inserted layout tokens)
insertLayout :: Cst.SourceToken -> Cst.SourcePos -> LayoutStack -> Tuple LayoutStack (List (Tuple Cst.SourceToken LayoutStack))
insertLayout src nextPos stack =
  insert (Tuple stack Nil)
  where
  tokPos = src.range.start
  tok = src.value

  insert state = case tok of
    -- `data` declarations need masking (LytTopDecl) because the usage of `|`
    -- should not introduce a LytDeclGuard context.
    Cst.TokLowerName Nothing "data" ->
      case state # insertDefault of
        state'@(Tuple stk' _) | isTopDecl tokPos stk' ->
          state' # pushStack tokPos LytTopDecl
        state' ->
          state' # popStack (_ == LytProperty)

    -- `class` declaration heads need masking (LytTopDeclHead) because the
    -- usage of commas in functional dependencies.
    Cst.TokLowerName Nothing "class" ->
      case state # insertDefault of
        state'@(Tuple stk' _) | isTopDecl tokPos stk' ->
          state' # pushStack tokPos LytTopDeclHead
        state' ->
          state' # popStack (_ == LytProperty)

    Cst.TokLowerName Nothing "where" ->
      case stack of
        Tuple _ LytTopDeclHead : stk' ->
          Tuple stk' (snd state) # insertToken src # insertStart LytWhere
        Tuple _ LytProperty : stk' ->
          Tuple stk' (snd state) # insertToken src
        _ ->
          state # collapse whereP # insertToken src # insertStart LytWhere

    Cst.TokLowerName Nothing "in" ->
      case collapse inP state of
        -- `let/in` is not allowed in `ado` syntax
        Tuple ((Tuple pos1 LytLetStmt) : (Tuple pos2 LytAdo) : stk') acc' ->
          Tuple stk' acc' # insertEnd pos1.column # insertEnd pos2.column # insertToken src
        Tuple (Tuple pos1 lyt : stk') acc' ->
          if isIndented lyt
          then Tuple stk' acc' # insertEnd pos1.column # insertToken src
          else state # insertDefault # popStack (_ == LytProperty)
        _ ->
          state # insertDefault # popStack (_ == LytProperty)

    Cst.TokLowerName Nothing "let" ->
      state # insertKwProperty letNext

    Cst.TokLowerName _ "do" ->
      state # insertKwProperty (insertStart LytDo)

    Cst.TokLowerName _ "ado" ->
      state # insertKwProperty (insertStart LytAdo)

    -- `case` heads need masking due to commas.
    Cst.TokLowerName Nothing "case" ->
      state # insertKwProperty (pushStack tokPos LytCase)

    Cst.TokLowerName Nothing "of" ->
      case collapse indentedP state of
        -- When `of` is matched with a `case`, we are in a case block
        Tuple (Tuple _ LytCase : stk') acc' ->
          Tuple stk' acc' # insertToken src # insertStart LytOf # pushStack nextPos LytCaseBinders
        state' ->
          state' # insertDefault # popStack (_ == LytProperty)

    -- `if/then/else` is considered a delimiter context
    Cst.TokLowerName Nothing "if" ->
      state # insertKwProperty (pushStack tokPos LytIf)

    Cst.TokLowerName Nothing "then" ->
      case state # collapse indentedP of
        Tuple (Tuple _ LytIf : stk') acc' ->
          Tuple stk' acc' # insertToken src # pushStack tokPos LytThen
        _ ->
          state # insertDefault # popStack (_ == LytProperty)

    Cst.TokLowerName Nothing "else" ->
      case state # collapse indentedP of
        Tuple (Tuple _ LytThen : stk') acc' ->
          Tuple stk' acc' # insertToken src
        _ ->
          -- Don't insert a layout separator for top-level `else` in instance chains
          case state # collapse offsideP of
            state'@(Tuple stk' _) | isTopDecl tokPos stk' ->
              state' # insertToken src
            state' ->
              state' # insertSep # insertToken src # popStack (_ == LytProperty)

    -- `forall` binders need masking because the usage of `.` should not
    -- introduce a LytProperty context.
    Cst.TokForall ->
      state # insertKwProperty (pushStack tokPos LytForall)

    -- Lambdas need masking
    Cst.TokBackslash ->
      state # insertDefault # pushStack tokPos LytLambdaBinders

    Cst.TokRightArrow ->
      state # collapse arrowP # popStack guardP # insertToken src

    Cst.TokEquals ->
      case state # collapse equalsP of
        Tuple (Tuple _ LytDeclGuard : stk') acc' ->
          Tuple stk' acc' # insertToken src
        _ ->
          state # insertDefault

    -- Guards need masking because of commas.
    Cst.TokPipe ->
      case collapse offsideEndP state of
        state'@(Tuple (Tuple _ LytOf : _) _) ->
          state' # pushStack tokPos LytCaseGuard # insertToken src
        state'@(Tuple (Tuple _ LytLet : _) _) ->
          state' # pushStack tokPos LytDeclGuard # insertToken src
        state'@(Tuple (Tuple _ LytLetStmt : _) _) ->
          state' # pushStack tokPos LytDeclGuard # insertToken src
        state'@(Tuple (Tuple _ LytWhere : _) _) ->
          state' # pushStack tokPos LytDeclGuard # insertToken src
        _ ->
          state # insertDefault

    -- Ticks can either start or end an infix expression
    Cst.TokTick ->
      case state # collapse indentedP of
        Tuple (Tuple _ LytTick : stk') acc' ->
          Tuple stk' acc' # insertToken src
        _ ->
          state # collapse offsideEndP # insertSep # insertToken src # pushStack tokPos LytTick

    -- Commas should close all indented contexts
    Cst.TokComma ->
      case state # collapse indentedP of
        -- If we see a LytBrace, we are in a record
        state'@(Tuple (Tuple _ LytBrace : _) _) ->
          state' # insertToken src # pushStack tokPos LytProperty
        state' ->
          state' # insertToken src

    -- Dot tokens usually entail property access
    Cst.TokDot ->
      case state # insertDefault of
        Tuple (Tuple _ LytForall : stk') acc' ->
          Tuple stk' acc'
        state' ->
          state' # pushStack tokPos LytProperty

    Cst.TokLeftParen ->
      state # insertDefault # pushStack tokPos LytParen

    Cst.TokLeftBrace ->
      state # insertDefault # pushStack tokPos LytBrace # pushStack tokPos LytProperty

    Cst.TokLeftSquare ->
      state # insertDefault # pushStack tokPos LytSquare

    Cst.TokRightParen ->
      state # collapse indentedP # popStack (_ == LytParen) # insertToken src

    Cst.TokRightBrace ->
      state # collapse indentedP # popStack (_ == LytProperty) # popStack (_ == LytBrace) # insertToken src

    Cst.TokRightSquare ->
      state # collapse indentedP # popStack (_ == LytSquare) # insertToken src

    Cst.TokString _ _ ->
      state # insertDefault # popStack (_ == LytProperty)

    Cst.TokLowerName Nothing _ ->
      state # insertDefault # popStack (_ == LytProperty)

    Cst.TokOperator _ _ ->
      state # collapse offsideEndP # insertSep # insertToken src

    _ ->
      state # insertDefault

  insertDefault state =
    state # collapse offsideP # insertSep # insertToken src

  insertStart lyt state@(Tuple stk _) =
    -- Only insert a new layout start when it's going to increase indentation.
    case findIndented stk of
      Just (Tuple pos _) ->
        if nextPos.column <= pos.column
        then state
        else state # pushStack nextPos lyt # insertToken (lytToken nextPos (Cst.TokLayoutStart nextPos.column))
      _ -> state # pushStack nextPos lyt # insertToken (lytToken nextPos (Cst.TokLayoutStart nextPos.column))

  -- Local find function to avoid depending on external import
  findIndented Nil = Nothing
  findIndented (item@(Tuple _ lyt) : rest) =
    if isIndented lyt
    then Just item
    else findIndented rest

  insertSep state@(Tuple stk acc) =
    let sepTok = lytToken tokPos (Cst.TokLayoutSep tokPos.column)
    in case stk of
      -- LytTopDecl is closed by a separator.
      Tuple lytPos LytTopDecl : stk' ->
        if sepP lytPos
        then Tuple stk' acc # insertToken sepTok
        else state
      -- LytTopDeclHead can be closed by a separator if there is no `where`.
      Tuple lytPos LytTopDeclHead : stk' ->
        if sepP lytPos
        then Tuple stk' acc # insertToken sepTok
        else state
      Tuple lytPos lyt : _ ->
        if indentSepP lytPos lyt
        then case lyt of
          -- If a separator is inserted in a case block, we need to push an
          -- additional LytCaseBinders context for comma masking.
          LytOf -> state # insertToken sepTok # pushStack tokPos LytCaseBinders
          _ -> state # insertToken sepTok
        else state
      _ ->
        state

  insertKwProperty k state =
    case state # insertDefault of
      Tuple (Tuple _ LytProperty : stk') acc' ->
        Tuple stk' acc'
      state' ->
        k state'

  insertEnd indent =
    insertToken (lytToken tokPos (Cst.TokLayoutEnd indent))

  insertToken token (Tuple stk acc) =
    Tuple stk (acc <> ((Tuple token stk) : Nil))

  pushStack lytPos lyt (Tuple stk acc) =
    Tuple (Tuple lytPos lyt : stk) acc

  popStack p state = case state of
    Tuple (Tuple _ lyt : stk') acc ->
      if p lyt
      then Tuple stk' acc
      else state
    _ -> state

  collapse p = go
    where
    go state = case state of
      Tuple (Tuple lytPos lyt : stk') acc ->
        if p lytPos lyt
        then go (Tuple stk' $
              if isIndented lyt
              then acc <> ((Tuple (lytToken tokPos (Cst.TokLayoutEnd lytPos.column)) stk') : Nil)
              else acc)
        else state
      _ -> state

  indentedP _ lyt = isIndented lyt

  offsideP lytPos lyt =
    isIndented lyt && tokPos.column < lytPos.column

  offsideEndP lytPos lyt =
    isIndented lyt && tokPos.column <= lytPos.column

  indentSepP lytPos lyt =
    isIndented lyt && sepP lytPos

  sepP lytPos =
    tokPos.column == lytPos.column && tokPos.line /= lytPos.line

  -- `where` always closes do blocks
  -- `where` closes layout contexts even when indented at the same level
  whereP _ LytDo = true
  whereP lytPos lyt = offsideEndP lytPos lyt

  inP _ LytLet = false
  inP _ LytAdo = false
  inP _ lyt = isIndented lyt

  letNext state'@(Tuple stk' _) = case stk' of
    Tuple p LytDo : _ ->
      if p.column == tokPos.column
      then state' # insertStart LytLetStmt
      else state' # insertStart LytLet
    Tuple p LytAdo : _ ->
      if p.column == tokPos.column
      then state' # insertStart LytLetStmt
      else state' # insertStart LytLet
    _ ->
      state' # insertStart LytLet

  arrowP _ LytDo = true
  arrowP _ LytOf = false
  arrowP lytPos lyt = offsideEndP lytPos lyt

  guardP LytCaseBinders = true
  guardP LytCaseGuard = true
  guardP LytLambdaBinders = true
  guardP _ = false

  equalsP _ LytWhere = true
  equalsP _ LytLet = true
  equalsP _ LytLetStmt = true
  equalsP _ _ = false
