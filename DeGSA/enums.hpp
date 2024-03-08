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

    addNode = 1,
    subtractNode = 2,
    multiplyNode = 3,
    divideNode = 4,
    assignNode = 5,
    identifierNode = 6,
    stringNode = 7,
    intNode = 8,
    realNode = 9,
    castNode = 10,
    greaterNode = 11, // >
    lesserNode = 12 // <
    //TODO add more

};

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};
