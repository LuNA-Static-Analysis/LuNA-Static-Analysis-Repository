#include "ids.hpp"

std::string Id::getName(){
    return this->name;
}

IdType Id::getType(){
    return this->type;
}

BaseDFId::BaseDFId(std::string name){
    this->name = name;
    this->type = baseDFId;
    this->sizeAndUseDefToVertexVector = {};
}

IndexedDFId::IndexedDFId(std::string name, Id* base, std::vector<expr*> expressionsVector){
    this->name = name;
    this->type = indexedDFId;

    if ((base->getType() != baseDFId) && (base->getType() != letId)){
        //TODO error!
    }

    this->base = base;
    this->expressionsVector = expressionsVector;
}
