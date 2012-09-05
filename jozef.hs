module Jozef where

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Expr

data Const16 = LabelName String
             | Number16 Integer
             | HashConst8 Const8
             | RegName String
  deriving (Show, Eq)

data Const8  = ConstL Const16
             | ConstH Const16
             | Number8 Integer
  deriving (Show, Eq)

data AST = Operation1 String Const16
         | Operation2 String Const16 Const16
         | Operation3 String Const16 Const16 Const16
         | Label String
         | Ret
         | Var8 String Const8
         | Var16 String Const16
         | VarStr String String
         | Array String Integer
         | Seq [AST]
  deriving (Show, Eq)


def = emptyDef{ commentStart = "/*"
              , commentEnd = "*/"
              , identStart = letter
              , identLetter = (alphaNum <|> char '_')
              --, opStart = oneOf "~&=:"
              --, opLetter = oneOf "~&=:"
              --, reservedOpNames = ["~", "&", "=", ":="]
              , reservedNames = ["exit", "jmp", "putc",
                                 "push", "pop", "call",
                                 "jmpnz", "add", "notl",
                                 "or", "mov", "ret",
                                 "int8", "int16", "strz", "array"]
              }

registers = [("EXIT", 0xfeff)
            ,("PUTC", 0xfefe)
            ,("ALOP1", 0xfefd)
            ,("ALOP2", 0xfefc)
            ,("ALOR", 0xfefb)
            ,("ALSL", 0xfefa)
            ,("JMPNZ", 0xfef9)
            ,("JMPL", 0xfef8)
            ,("JMPH", 0xfef7)
            ,("PTRL", 0xfef6)
            ,("PTRH", 0xfef5)
            ,("PTRA", 0xfef4)
            ,("ALADD", 0xfef3)
            ,("ALNOTL", 0xfef2)
            ,("SPL", 0xfef1)
            ,("SPH", 0xfef0)
            ,("STACK", 0xfeef)
            ,("CALL", 0xfeee)]


TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , natural = m_natural
           , colon = m_colon
           , stringLiteral = m_stringLiteral
           , whiteSpace = m_whiteSpace } = makeTokenParser def

simpleOne :: Parser AST
simpleOne = m_whiteSpace >> statementParser <* eof
  where
    statementParser = fmap Seq (m_semiSep1 stmt)
    stmt = do { op <- oper1
              ; num <- p_const16
              ; return $ Operation1 op num} <|>
           do { op <- oper2
              ; num1 <- p_const16
              ; num2 <- p_const16
              ; return $ Operation2 op num1 num2} <|>
           do { op <- oper3
              ; num1 <- p_const16
              ; num2 <- p_const16
              ; num3 <- p_const16
              ; return $ Operation3 op num1 num2 num3} <|>
           do { lab <- ourlabel
              ; return $ Label lab
              } <|> ( m_reserved "ret" >> return Ret ) <|>
           do { m_reserved "int8"
              ; ident <- m_identifier
              ; num <- p_const8
              ; return $ Var8 ident num } <|>
           do { m_reserved "int16"
              ; ident <- m_identifier
              ; num <- p_const16
              ; return $ Var16 ident num } <|>
           do { m_reserved "strz"
              ; ident <- m_identifier
              ; str <- m_stringLiteral
              ; return $ VarStr ident str } <|>
           do { m_reserved "array"
              ; ident <- m_identifier
              ; num <- m_natural
              ; return $ Array ident num }

ourlabel = m_identifier >>= (\ident -> m_colon >> return ident)

oper1 = foldr1 (<|>) $
    map (\x -> m_reserved x >> return x) ["exit", "putc", "jmp", "push", "pop", "call"]

oper2 = foldr1 (<|>) $
    map (\x -> m_reserved x >> return x) ["mov", "jmpnz", "notl"]

oper3 = foldr1 (<|>) $
    map (\x -> m_reserved x >> return x) ["add", "or"]

p_const16 = do { ident <- m_identifier
               ; return $ LabelName ident } <|>
            do { num <- m_natural
               ; return $ Number16 num } <|>
            do { char '#'
               ; num <- p_const8
               ; return $ HashConst8 num } <|>
            do { num <- m_natural
               ; return $ Number16 num }

p_const8 = do { num <- m_natural
              ; return $ Number8 num } <|>
           do { num <- p_const16
              ; char '/'
              ; l_or_h num }

l_or_h num = do { char 'l'
                ; return $ ConstL num } <|>
             do { char 'h'
                ; return $ ConstH num }

is_var (Var8 _ _) = True
is_var (Var16 _ _) = True
is_var (VarStr _ _) = True
is_var (Array _ _) = True
is_var _ = False

extract_vars = filter is_var

extract_ops = filter $ not . is_var

mov_ar x y = (x, (LabelName y))
mov_ra x y = ((LabelName x), y)
mov_hr x y = (HashConst8 $ ConstH x, (LabelName y))
mov_lr x y = (HashConst8 $ ConstL x, (LabelName y))
mov_nr x y = (HashConst8 $ Number8 x, (LabelName y))

op_to_mov (Operation1 "exit" x) = [mov_ar x "EXIT"]
op_to_mov (Operation1 "putc" x) = [mov_ar x "PUTC"]
op_to_mov (Operation1 "jmp" x) = [mov_hr x "JMPH",
                                  mov_lr x "JMPL",
                                  mov_nr 1 "JMPNZ"]
op_to_mov (Operation1 "push" x) = [mov_ar x "STACK"]
op_to_mov (Operation1 "pop" x) = [mov_ra "STACK" x]
op_to_mov (Operation1 "call" x) = [mov_hr x "JMPH",
                                   mov_lr x "JMPL",
                                   mov_nr 1 "CALL"]
op_to_mov (Operation2 "mov" x y) = [(y, x)]
op_to_mov (Operation2 "jmpnz" x y) = [mov_hr x "JMPH",
                                      mov_lr x "JMPL",
                                      mov_ar y "JMPNZ"]
op_to_mov (Operation2 "notl" x y) = [mov_ar y "ALOP1",
                                     mov_ra "ALNOTL" x]
op_to_mov (Operation3 "add" x y z) = [mov_ar y "ALOP1",
                                      mov_ar z "ALOP2",
                                      mov_ra "ALADD" x]
op_to_mov (Operation3 "or" x y z) = [mov_ar y "ALOP1",
                                     mov_ar z "ALOP2",
                                     mov_ra "ALOR" x]
op_to_mov Ret = [mov_nr 1 "JMPNZ"]
