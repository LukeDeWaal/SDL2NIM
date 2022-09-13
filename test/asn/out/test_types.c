/*
Code automatically generated by asn1scc tool
*/
#include <limits.h>
#include <string.h>
#include <math.h>


#include "test_types.h"




flag MyInt_Equal(const MyInt* pVal1, const MyInt* pVal2)
{
	return (*(pVal1)) == (*(pVal2));

}

void MyInt_Initialize(MyInt* pVal)
{
	(void)pVal;


	(*(pVal)) = 0;
}

flag MyInt_IsConstraintValid(const MyInt* pVal, int* pErrCode)
{
    flag ret = TRUE;
    ret = ((*(pVal)) <= 255UL);
    *pErrCode = ret ? 0 :  ERR_MYINT; 

	return ret;
}



flag MyBool_Equal(const MyBool* pVal1, const MyBool* pVal2)
{
	return (*(pVal1)) == (*(pVal2));

}

void MyBool_Initialize(MyBool* pVal)
{
	(void)pVal;


	(*(pVal)) = FALSE;
}

flag MyBool_IsConstraintValid(const MyBool* pVal, int* pErrCode)
{
    flag ret = TRUE;
	(void)pVal;
    ret = TRUE;
    *pErrCode = 0;

	return ret;
}



flag MyEnum_Equal(const MyEnum* pVal1, const MyEnum* pVal2)
{
	return (*(pVal1)) == (*(pVal2));

}

void MyEnum_Initialize(MyEnum* pVal)
{
	(void)pVal;


	(*(pVal)) = hello;
}

flag MyEnum_IsConstraintValid(const MyEnum* pVal, int* pErrCode)
{
    flag ret = TRUE;
    ret = ((((*(pVal)) == hello)) || (((*(pVal)) == world)));
    *pErrCode = ret ? 0 :  ERR_MYENUM; 

	return ret;
}



flag MySeq_Equal(const MySeq* pVal1, const MySeq* pVal2)
{
	flag ret=TRUE;

    ret = (pVal1->a == pVal2->a);

    if (ret) {
        ret = (pVal1->b == pVal2->b);

        if (ret) {
            ret = (pVal1->c == pVal2->c);

        }

    }

	return ret;

}

void MySeq_Initialize(MySeq* pVal)
{
	(void)pVal;



	/*set a */
	MyInt_Initialize((&(pVal->a)));
	/*set b */
	MyBool_Initialize((&(pVal->b)));
	/*set c */
	MyEnum_Initialize((&(pVal->c)));
}

flag MySeq_IsConstraintValid(const MySeq* pVal, int* pErrCode)
{
    flag ret = TRUE;
    ret = MyInt_IsConstraintValid((&(pVal->a)), pErrCode);
    if (ret) {
        ret = MyBool_IsConstraintValid((&(pVal->b)), pErrCode);
        if (ret) {
            ret = MyEnum_IsConstraintValid((&(pVal->c)), pErrCode);
        }
    }

	return ret;
}
