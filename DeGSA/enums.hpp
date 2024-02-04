#pragma once

enum IdentifierType {

    subArgName = 1,
    baseDFName = 2,
    indexedDFName = 3,
    forId = 4, //todo rename
    whileId = 5, //todo rename
    valueId = 6, //todo rename
    letId = 7 //todo rename

};

enum VertexType {

    forVF = 1,
    ifVF = 2,
    whileVF = 3,
    letVF = 4,
    importVF = 5,
    subVF = 6

};

enum UseDef {

    use = 1,
    def = 2,
    useAndDef = 3

};
