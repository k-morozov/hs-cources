module TestDataType where
import DataType ( someFunc )
import Test.HUnit

testDataType :: Test
testDataType = TestList [TestLabel "TestDataType" 
    testSomeFunc
    ]

testSomeFunc :: Test
testSomeFunc = TestCase (assertEqual "simple check" 1 (someFunc 1))