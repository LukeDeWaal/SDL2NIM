import test_types

echo "hello, world"

var i : MyInt = 5

var j : MyInt = 0

var testBool : MyBool = false
var testEnum : MyEnum = hello
echo "boolean (set to false): ", testBool
echo "enumerated: ", testEnum

echo "test if 5 == 0:"
echo (MyInt_Equal(addr i, addr j))

MyInt_Initialize (addr i)

echo "test if 0 == 0:"
echo (MyInt_Equal(addr i, addr j))

var testSeq : MySeq
testSeq.a = 42
testSeq.b = true
testSeq.c = world
echo (testSeq)
