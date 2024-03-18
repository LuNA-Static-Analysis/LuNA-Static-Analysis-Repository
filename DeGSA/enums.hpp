#pragma once

enum IdentifierType {

    subArgNameType = 1,
    baseDFNameType = 2,
    indexedDFNameType = 3,
    forIteratorNameType = 4,
    whileIteratorNameType = 5,
    valueNameType = 6,
    letNameType = 7,
    mainArgNameType = 8

};

enum VertexType {

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

    // ternary operator: (condition) ? left : right
    ternaryNode = 14,

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

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};
