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

    //todo ternary operator

    assignNode = 15, // =; todo does it exist?

    // identifier
    identifierNode = 16,

    // constants
    stringNode = 17,
    intNode = 18,
    realNode = 19,
    
    // unary operators
    intCastNode = 20,
    realCastNode = 21,
    stringCastNode = 22,

    noneNode = 23 // means error happened

};

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};
