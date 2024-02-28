#pragma once

enum IdentifierType {

    subArgName = 1,
    baseDFName = 2,
    indexedDFName = 3,
    forIteratorName = 4,
    whileIteratorName = 5,
    whileOutName = 6,
    valueName = 7,
    letName = 8,
    mainArgName = 9

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
    noneNode = 6, //temp TODO
    identifierNode = 7,
    stringNode = 8,
    intNode = 9,
    realNode = 10,
    castNode = 11
    //TODO add more

};

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};
