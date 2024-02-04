#include "ids.hpp"

std::string Identifier::getName(){
    return this->name;
}

IdentifierType Identifier::getType(){
    return this->type;
}

std::map<BaseDFName*, std::set<int>> Identifier::getRoots(){//todo DFR
    return {};
}

Identifier::Identifier(){}

Identifier::~Identifier(){}

std::set<Identifier*> SubArgName::getNameReferenceSet(){
    return this->nameReferenceSet;
}

SubArgName::SubArgName(std::string name, std::set<Identifier*> nameReferenceSet){
    this->type = subArgName;
    this->name = name;
    this->nameReferenceSet = nameReferenceSet;
}

SubArgName::~SubArgName(){}

void BaseDFName::addUse(int size, Vertex* vertex){
    auto maybePair = this->sizeToUseDefVectors.find(size);
    if (maybePair != this->sizeToUseDefVectors.end()){ // found this IDF being used or defined already
        auto maybeUseVector = maybePair->second.first;
        if (maybeUseVector == nullptr) // check if it was defined and create new vector if not
            maybeUseVector = new std::vector<Vertex*>;
        // then add this vertex as use
        maybeUseVector->push_back(vertex);
    } else { // create new pair and a new vector
        this->sizeToUseDefVectors.insert(std::make_pair(size, std::make_pair(
            new std::vector<Vertex*>(),
            new std::vector<Vertex*>()
        )));
        this->sizeToUseDefVectors.find(size)->second.first->push_back(vertex);
    }
}

// I do now know if objects' condition changes or not
void BaseDFName::addDef(int size, Vertex* vertex){
    auto maybePair = this->sizeToUseDefVectors.find(size);
    if (maybePair != this->sizeToUseDefVectors.end()){ // found this IDF being used or defined already
        auto maybeDefVector = maybePair->second.second;
        if (maybeDefVector == nullptr) // check if it was defined and create new vector if not
            maybeDefVector = new std::vector<Vertex*>;
        // then add this vertex as def
        maybeDefVector->push_back(vertex);
    } else { // create new pair and a new vector
        this->sizeToUseDefVectors.insert(std::make_pair(size, std::make_pair(
            new std::vector<Vertex*>(),
            new std::vector<Vertex*>()
        )));
        this->sizeToUseDefVectors.find(size)->second.second->push_back(vertex);
    }
}

std::map<int, std::pair<std::vector<Vertex*>*, std::vector<Vertex*>*>> BaseDFName::getMap(){
    return this->sizeToUseDefVectors;
}

BaseDFName::BaseDFName(std::string name){
    this->name = name;
    this->type = baseDFName;
    this->sizeToUseDefVectors = {};
}

BaseDFName::~BaseDFName(){}

IndexedDFName::IndexedDFName(std::string name, Identifier* base, std::vector<expr*> expressionsVector){
    this->name = name;
    this->type = indexedDFName;

    if ((base->getType() != baseDFName) && (base->getType() != letId)){
        //TODO error!
    }

    this->base = base;
    this->expressionsVector = expressionsVector;
}

IndexedDFName::~IndexedDFName(){}

Identifier* IndexedDFName::getBase(){
    return this->base;
}

ForId::ForId(std::string iteratorName, expr* leftBorder, expr* rightBorder){
    this->name = iteratorName;
    this->type = forId;
    this->leftBorder = leftBorder;
    this->rightBorder = rightBorder;
}

ForId::~ForId(){}
