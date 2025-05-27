#pragma once

enum IdentifierClass {

    baseDFNameClass = 1,
    indexedDFNameClass = 2,
    forIteratorNameClass = 3,
    whileIteratorNameClass = 4,
    valueNameClass = 5,
    letNameClass = 6,
    mutableArgNameClass = 7,
    immutableArgNameClass = 8

};

enum VertexType {//todo rename this stupid ass shit

    forVF = 1,
    ifVF = 2,
    whileVF = 3,
    letVF = 4,
    importVF = 5,
    subVF = 6

};

enum ExpressionType {

    // binary operations
    addNode = 1, // +
    subtractNode = 2, // -
    multiplyNode = 3, // *
    divideNode = 4, // /
    modulusNode = 5, // %
    greaterNode = 6, // >
    greaterOrEqualNode = 7, // >=
    lesserNode = 8, // <
    lesserOrEqualNode = 9, // <=
    equalNode = 10, // ==
    nonEqualNode = 11, // !=
    andNode = 12, // &&
    orNode = 13, // ||

    // identifier
    identifierNode = 15,

    // constants
    stringNode = 16,
    intNode = 17,
    realNode = 18,
    
    // unary operators
    intCastNode = 19,
    realCastNode = 20,
    stringCastNode = 21,

    noneNode = 22 // means error happened somewhere, or expression can not exist

};

enum ExpressionEquality {
    definitelyEqual = 1,
    maybeEqual = 2,
    notEqual = 3
};

// used for Identifiers and DeclaredArgs structs (as LuNA types)
enum ValueType {

    // LuNA types
    intType = 1,
    realType = 2,
    stringType = 3,
    valueType = 4,
    nameType = 5,

    noneType = 6, // used for BaseDFName for example
    nonCalculatable = 7, // it's not possible to determine the type
    notCalculated = 8 // used for lazy initialization
};

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};

enum CFType {

    subCF = 1,
    importCF = 2

};
