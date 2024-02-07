#include "ids.hpp"

std::string Identifier::getName(){
    return this->name;
}

IdentifierType Identifier::getType(){
    return this->type;
}

Identifier::Identifier(){}

Identifier::~Identifier(){}

std::set<Identifier*> SubArgName::getNameReferenceSet(){
    return this->nameReferenceSet;
}

std::set<std::pair<Identifier*, int>> SubArgName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    for (auto i: this->getNameReferenceSet()){
        auto temp = i->getRoots();
        for (auto j: temp){
            result.insert(j);
        }
    }

    return result;
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

// I do not know if objects' condition changes or not :)
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

std::set<std::pair<Identifier*, int>> BaseDFName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    result.insert(std::make_pair(this, 0));
    return result;
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

Identifier* IndexedDFName::getBase(){
    return this->base;
}

std::set<std::pair<Identifier*, int>> IndexedDFName::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    std::set<std::pair<Identifier*, int>> baseRoots = this->base->getRoots();
    for (auto br: baseRoots){
        result.insert(std::make_pair(br.first, br.second + this->expressionsVector.size()));
    }
    return result;
}

std::vector<expr*> IndexedDFName::getExpressionsVector(){
    return this->expressionsVector;
}

IndexedDFName::~IndexedDFName(){}

std::set<std::pair<Identifier*, int>> ForId::getRoots(){
    std::set<std::pair<Identifier*, int>> result = {};
    result.insert(std::make_pair(this, 0));
    return result;
}

ForId::ForId(std::string iteratorName, expr* leftBorder, expr* rightBorder){
    this->name = iteratorName;
    this->type = forId;
    this->leftBorder = leftBorder;
    this->rightBorder = rightBorder;
}

ForId::~ForId(){}
