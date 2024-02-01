#pragma once

enum IdType {

    baseDFId = 1,
    indexedDFId = 2,
    forId = 3,
    whileId = 4,
    valueId = 5,
    letId = 6

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
