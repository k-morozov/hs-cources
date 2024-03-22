module TestDataType where
import DataType ( someFunc, cmp, LogLevel(..) )
import Test.HUnit

testDataType :: Test
testDataType = TestList [TestLabel "TestDataType" 
    testSomeFunc
    ,testCmpGt
    ]

testSomeFunc :: Test
testSomeFunc = TestCase (assertEqual "simple check" 1 (someFunc 1))

testCmpGt :: Test
testCmpGt = TestCase (assertEqual "check gt" GT (cmp Error Warning))