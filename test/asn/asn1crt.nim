import std/macros

template pass*() = 
  discard nil;

macro label*(labelName, body: untyped): untyped =
    expectKind(labelName, nnkIdent)
    let name = repr(labelName)
    result = quote do:
      {.emit: `name` & ":".}
      `body`

macro goto*(labelName: untyped): untyped =
    expectKind(labelName, nnkIdent)
    let name = repr(labelName)
    result = quote do:
      {.emit: "goto " & `name` & ";".}  

when not defined(NULL):
    const
        NULL* = 0
when not defined(TRUE):
    const
        TRUE* = true
when not defined(FALSE):
    const
        FALSE* = false
when not defined(WORD_SIZE):
    const
        WORD_SIZE* = 8
when not defined(FP_WORD_SIZE):
    const
        FP_WORD_SIZE* = 8
when not defined(PRId32):
    const
        PRId32* = "d"
when not defined(PRId64):
    const
        PRId64* = "lld"
when not defined(PRIu32):
    const
        PRIu32* = "u"
when not defined(PRIu64):
    const
        PRIu64* = "llu"
const
    OBJECT_IDENTIFIER_MAX_LENGTH* = 20  

type
    asn1Real32* = cfloat
    asn1Real64* = cdouble
    asn1byte* = byte
    asn1bool* = bool
    asn1SccSint16* = int16
    asn1SccUint16* = uint16
    asn1SccSint32* = int32
    asn1SccUint32* = uint32
    asn1SccSint64* = int64
    asn1SccUint64* = uint64

when WORD_SIZE == 8:
    type
        asn1SccUint* = asn1SccUint64
        asn1SccSint* = asn1SccSint64
    const
        ASN1SCC_PRId* = PRId64
        ASN1SCC_PRIu* = PRIu64
else:
    type
        asn1SccUint* = asn1SccUint32
        asn1SccSint* = asn1SccSint32
    const
        ASN1SCC_PRId* = PRId32
        ASN1SCC_PRIu* = PRIu32

proc int2uint*(v: asn1SccSint): asn1SccUint {.importc: "int2uint".}
proc uint2int*(v: asn1SccUint; uintSizeInBytes: cint): asn1SccSint {.importc: "uint2int".}

when FP_WORD_SIZE == 8:
    type
        asn1Real* = asn1Real64
else:
    type
        asn1Real* = asn1Real32

type
    flag* = bool
    NullType* = char

discard "forward decl of BitStream_t"
type
    BitStream* {.bycopy.} = object
        buf*: ptr byte
        count*: clong
        currentByte*: clong        ##  Next available bit for writting.
                          ## 	Possible vallues 0..7, 0 is most significant
                          ## 	bit of current byte
        currentBit*: cint          ## PushDataFnc pushData;
        pushDataPrm*: pointer      ## FetchDataFnc fetchData;
        fetchDataPrm*: pointer

    ByteStream* {.bycopy.} = object
        buf*: ptr byte
        count*: clong
        currentByte*: clong
        EncodeWhiteSpace*: flag

    Token* {.bycopy.} = object
        TokenID*: cint
        Value*: array[100, char]

    XmlAttribute* {.bycopy.} = object
        Name*: array[50, char]
        Value*: array[100, char]

    XmlAttributeArray* {.bycopy.} = object
        attrs*: array[20, XmlAttribute]
        nCount*: cint

    Asn1ObjectIdentifier* {.bycopy.} = object
        nCount*: cint
        values*: array[OBJECT_IDENTIFIER_MAX_LENGTH, asn1SccUint]

const
    ERR_INSUFFICIENT_DATA* = 101
    ERR_INCORRECT_PER_STREAM* = 102
    ERR_INVALID_CHOICE_ALTERNATIVE* = 103
    ERR_INVALID_ENUM_VALUE* = 104
    ERR_INVALID_XML_FILE* = 200
    ERR_INVALID_BER_FILE* = 201
    ERR_BER_LENGTH_MISMATCH* = 202

proc GetCharIndex*(ch: char; allowedCharSet: ptr byte; setLen: cint): cint {.importc: "GetCharIndex".}
proc ObjectIdentifier_Init*(pVal: ptr Asn1ObjectIdentifier) {.importc: "ObjectIdentifier_Init".}
proc ObjectIdentifier_equal*(pVal1: ptr Asn1ObjectIdentifier; pVal2: ptr Asn1ObjectIdentifier): flag {.importc: "ObjectIdentifier_equal".}
proc ObjectIdentifier_isValid*(pVal: ptr Asn1ObjectIdentifier): flag {.importc: "ObjectIdentifier_isValid".}
proc RelativeOID_isValid*(pVal: ptr Asn1ObjectIdentifier): flag {.importc: "RelativeOID_isValid".}

type
    Asn1TimeZone* {.bycopy.} = object
        sign*: cint                ## -1 or +1
        hours*: cint
        mins*: cint

    Asn1TimeWithTimeZone* {.bycopy.} = object
        hours*: cint
        mins*: cint
        secs*: cint
        fraction*: cint
        tz*: Asn1TimeZone

    Asn1UtcTime* {.bycopy.} = object
        hours*: cint
        mins*: cint
        secs*: cint
        fraction*: cint

    Asn1LocalTime* {.bycopy.} = object
        hours*: cint
        mins*: cint
        secs*: cint
        fraction*: cint

    Asn1Date* {.bycopy.} = object
        years*: cint
        months*: cint
        days*: cint

    Asn1DateLocalTime* {.bycopy.} = object
        date*: Asn1Date
        time*: Asn1LocalTime

    Asn1DateUtcTime* {.bycopy.} = object
        date*: Asn1Date
        time*: Asn1UtcTime

    Asn1DateTimeWithTimeZone* {.bycopy.} = object
        date*: Asn1Date
        time*: Asn1TimeWithTimeZone

    Asn1TimeZoneClass* = enum
        Asn1TC_LocalTimeStamp, Asn1TC_UtcTimeStamp, Asn1TC_LocalTimeTZStamp
    BerTag* = asn1SccUint

template CHECK_BIT_STREAM*(pBitStrm: untyped): untyped =
    assert((pBitStrm).currentByte * 8 + (pBitStrm).currentBit <= (pBitStrm).count * 8)

template ASSERT_OR_RETURN_FALSE*(Expression: untyped): void =
    while true:
        assert(Expression)
        if not (Expression):
            return FALSE
        if not 0:
           break