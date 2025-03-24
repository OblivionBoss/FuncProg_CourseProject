type AttributeName = String
type Datatype = String
type AttributeList = [(AttributeName, Datatype)]
type Record = [Value]

data Value = VInt Int | VDouble Double | VString String | VBool Bool | NULL deriving (Eq, Ord)

instance Show Value where
  show (VInt num) = show num
  show (VDouble num) = show num
  show (VString str) = show str
  show (VBool b) = show b
  show NULL = "NULL"

data Relation = Relation
  { relationName :: String
  , attributes   :: AttributeList
  , records      :: [Record]
  }

instance Show Relation where
  show r = relationName r ++ "\n"
    ++ showAttributeList (attributes r) ++ "\n"
    ++ showRecords (records r)

showAttributeList :: AttributeList -> String
showAttributeList [] = "|"
showAttributeList ((attName, _):l) = "| " ++ attName ++ " " ++ showAttributeList l

showRecords :: [Record] -> String
showRecords [] = ""
showRecords (r:l) = showRecord r ++ "\n" ++ showRecords l

showRecord :: Record -> String
showRecord [] = "|"
showRecord (v:l) = "| " ++ show v ++ " " ++ showRecord l

-- data AttributeData = AttributeData 
--   { attributeName :: AttributeName
--   , datatype :: Datatype
--   , dataList :: [Value]
--   } deriving (Show)

-- data Relation' = Relation'
--   { relationName' :: String
--   , attributeDatas :: [AttributeData]
--   } deriving (Show)

data RelationalDatabase = RDB { relation :: [(String, Relation)]}

student :: Relation
student = Relation {
  relationName = "STUDENT",
  attributes = [("ID", "Int"), ("Name", "String"), ("GPA", "Double")],
  records = [
      [VInt 1, VString "John", VDouble 4.0],
      [VInt 2, VString "Alice", VDouble 3.5],
      [VInt 3, VString "Bob", VDouble 3.25]
  ]
}

employee :: Relation
employee = Relation {
  relationName = "EMPLOYEE",
  attributes = [("EMP_ID", "Int"), ("Name", "String"), ("Salary", "Int"), ("D_No", "Int")],
  records = [
      [VInt 1001, VString "John", VInt 80000, VInt 1],
      [VInt 1002, VString "Sarah", VInt 95000, VInt 2],
      [VInt 1003, VString "Michael", VInt 88000, VInt 3],
      [VInt 1004, VString "Emily", VInt 77000, VInt 1],
      [VInt 1005, VString "James", VInt 85000, VInt 2]
  ]
}

department :: Relation
department = Relation {
  relationName = "DEPARTMENT",
  attributes = [("Dnumber", "Int"), ("DName", "String"), ("Manager_ID", "Int")],
  records = [
      [VInt 1, VString "Research", VInt 1001],
      [VInt 2, VString "Finance", VInt 1002],
      [VInt 3, VString "IT", VInt 1003]
  ]
}

-- student' :: Relation'
-- student' = Relation' {
--   relationName' = "Student",
--   attributeDatas = 
--   [   AttributeData "ID" "Int" [VInt 1, VInt 2, VInt 3]
--   ,   AttributeData "Name" "String" [VString "John", VString "Alice", VString "Bob"]
--   ,   AttributeData "GPA" "Double" [VDouble 4.0, VDouble 3.5, VDouble 3.25]
--   ]
-- }

-- project' :: Relation' -> [AttributeName] -> Relation'
-- project' relation attNameList =
--   Relation' {
--     relationName' = relationName' relation,
--     attributeDatas = [attData | att' <- attNameList, attData <- attributeDatas relation, attributeName attData == att']
--   }

-- project :: Relation -> [AttributeName] -> Relation
-- project relation attNameList =
--   Relation {
--     relationName = relationName relation,
--     attributes = [(att, dt) | att' <- attNameList, (att, dt) <- attributes relation, att == att'],
--     records = map ((\record -> [record !! i | i <- reorderList]) . filterValue) (records relation)
--   }
--   where
--     label = map ((`elem` attNameList) . fst) (attributes relation)
--     filterValue = map snd . filter fst . zip label
--     reorderList = reverse $ reorder (attributes relation) 0 []
--     reorder [] _ res = res
--     reorder ((att, _):xs) i res = 
--       reorder xs (if isOccur then i+1 else i) (if isOccur then i:res else res)
--       where isOccur = elem att attNameList

project :: Relation -> [AttributeName] -> Relation
project relation attNameList =
  Relation {
    relationName = relationName relation,
    attributes = [(att, dt) | att' <- attNameList, (att, dt) <- attributes relation, att == att'],
    records = map filterValue (records relation)
  }
  where
    label = map ((`elem` attNameList) . fst) (attributes relation)
    filterValue = map snd . filter fst . zip label

cross :: Relation -> Relation -> Relation
cross relation1 relation2 =
  Relation {
    relationName = relationName relation1 ++ " X " ++ relationName relation2,
    attributes = attributes relation1 ++ attributes relation2,
    records = [record1 ++ record2 | record1 <- records relation1, record2 <- records relation2]
  }

joinOn :: Relation -> Relation -> BExpr -> Relation
joinOn = (select .) . cross

select :: Relation -> BExpr -> Relation
select relation bexpr =
  Relation {
    relationName = relationName relation,
    attributes = attributes relation,
    records = filter condition (records relation)
    -- records = [record | record <- records relation, condition record]
  }
  where condition record = runReader (evalBExpr bexpr) (RecordEnv (attributes relation) record)

data Expr = 
  Ident String
  | EInteger Int
  | EDouble Double
  | EString String
  | EBool Bool
  | ENull

data BExpr =
   And BExpr BExpr
  | Or BExpr BExpr
  | Not BExpr
  | Equal Expr Expr
  | Gt Expr Expr
  | Lt Expr Expr
  | Geq Expr Expr
  | Leq Expr Expr
  | BValue Bool

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader r r
ask = Reader id

instance Functor (Reader r) where
  fmap f (Reader rf) = Reader $ f . rf

instance Applicative (Reader r) where
  pure x = Reader $ const x
  (Reader rf) <*> (Reader rx) = Reader $ rf <*> rx

instance Monad (Reader r) where
  (Reader ra) >>= f = Reader $ \r -> runReader (f (ra r)) r

data RecordEnv = RecordEnv
  { attList :: AttributeList
  , rcd :: Record
  }

evalBExpr :: BExpr -> Reader RecordEnv Bool
evalBExpr bexpr = do
  case bexpr of
    And bexpr1 bexpr2 -> (evalBExpr bexpr1) >>= (\b1 -> if b1 then evalBExpr bexpr2 else return False)
    -- And expr1 expr2 -> do
    --   b1 <- evalBExpr expr1
    --   b2 <- evalBExpr expr2
    --   return $ b1 && b2
    Or bexpr1 bexpr2 -> (evalBExpr bexpr1) >>= (\b1 -> if b1 then return True else evalBExpr bexpr2)
    -- Or expr1 expr2 -> do
    --   b1 <- evalBExpr expr1
    --   b2 <- evalBExpr expr2
    --   return $ b1 || b2
    Not bexpr' -> (evalBExpr bexpr') >>= return . not
    Equal expr1 expr2 -> comp (==) expr1 expr2
    -- Equal expr1 expr2 -> do
    --   v1 <- evalExpr expr1
    --   v2 <- evalExpr expr2
    --   return $ v1 == v2
    Gt expr1 expr2 -> comp (>) expr1 expr2
    Lt expr1 expr2 -> comp (<) expr1 expr2
    Geq expr1 expr2 -> comp (>=) expr1 expr2
    Leq expr1 expr2 -> comp (<=) expr1 expr2
    BValue b -> return b
  where
    comp :: (Value -> Value -> Bool) -> Expr -> Expr -> Reader RecordEnv Bool
    comp f e1 e2 = do
      v1 <- evalExpr e1
      v2 <- evalExpr e2
      return $ f v1 v2

evalExpr :: Expr -> Reader RecordEnv Value
evalExpr expr = 
  case expr of
    Ident str -> evalIdent str
    EInteger num -> return $ VInt num
    EDouble num -> return $ VDouble num
    EString str -> return $ VString str
    EBool b -> return $ VBool b
    ENull -> return NULL
  where
    evalIdent :: String -> Reader RecordEnv Value
    evalIdent str = do
      recordEnv <- ask
      return $ findValue recordEnv str

findValue :: RecordEnv -> String -> Value
findValue recordEnv attName =
  let attributes = attList recordEnv
      record = rcd recordEnv
  in matchAttName attributes record
  where
    matchAttName :: AttributeList -> Record -> Value
    matchAttName [] [] = error $ "no such column: " ++ attName
    matchAttName (att:atts) (v:vs) =
      if fst att == attName then v else matchAttName atts vs
    matchAttName _ _ = error "input mismatch"