#include "ids.hpp"
#include "json_reporter.cpp"

//todo check if this is true
int Identifier::getLine(){
    if (m_vertex != nullptr){
        return m_vertex->getLine();
    } else {
        return -1;
    }
}

void Identifier::calculateValueType(){
    if (m_reference != nullptr)
        m_valueType = m_reference->getValueType();
    else
        m_valueType = nonCalculatable;
}

void BaseDFName::markAsUse(Vertex* vertex, int size){
    m_useSet.insert(vertex);

    std::cout << "Base DF name " << m_name << " is being marked as used" << std::endl;

    auto maybePair = _sizeToUseDefVectors.find(size);
    if (maybePair != _sizeToUseDefVectors.end()){ // found this IDF being used or defined already
        auto maybeUseVector = maybePair->second.first;
        if (maybeUseVector == nullptr) // check if it was defined and create new vector if not
            maybeUseVector = new std::vector<Vertex*>;
        // then add this vertex as use
        maybeUseVector->push_back(vertex);
    } else { // create new pair and a new vector
        _sizeToUseDefVectors.insert( { size, { new std::vector<Vertex*>(), new std::vector<Vertex*>() } } );
        _sizeToUseDefVectors.find(size)->second.first->push_back(vertex);
    }
}

void BaseDFName::markAsDef(Vertex* vertex, int size){
    m_defSet.insert(vertex);

    std::cout << "Base DF name " << m_name << " is being marked as defined" << std::endl;

    auto maybePair = _sizeToUseDefVectors.find(size);
    if (maybePair != _sizeToUseDefVectors.end()){ // found this IDF being used or defined already
        auto maybeDefVector = maybePair->second.second;
        if (maybeDefVector == nullptr) // check if it was defined and create new vector if not
            maybeDefVector = new std::vector<Vertex*>;
        // then add this vertex as def
        maybeDefVector->push_back(vertex);
    } else { // create new pair and a new vector
        _sizeToUseDefVectors.insert( { size, { new std::vector<Vertex*>(), new std::vector<Vertex*>() } } );
        _sizeToUseDefVectors.find(size)->second.second->push_back(vertex);
    }
}

void IndexedDFName::markAsUse(Vertex* currentVertex, int size){
    m_useSet.insert(currentVertex);

    std::cout << "Indexed DF name " << m_name << " of size " << _expressionsVector.size() << " is being marked as used" << std::endl;
    for (auto exp: _expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFName.markAsUse()" << std::endl;
        } else {
            exp->markAsUse(currentVertex, 0);
        }
    }
    if (_base != nullptr)
        _base->markAsUse(currentVertex, size + _expressionsVector.size());
    else
        std::cout << "INTERNAL ERROR: markAsDef encountered nullptr base of an IndexedDF" << std::endl;
}

void IndexedDFName::markAsDef(Vertex* currentVertex, int size){
    m_defSet.insert(currentVertex);

    std::cout << "Indexed DF name " << m_name << " of size " << _expressionsVector.size() << " is being marked as defined" << std::endl;
    for (auto exp: _expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFName.markAsDef()" << std::endl;
        } else {
            exp->markAsUse(currentVertex, 0);
        }
    }
    if (_base != nullptr)
        _base->markAsDef(currentVertex, size + _expressionsVector.size());
    else
        std::cout << "INTERNAL ERROR: markAsDef encountered nullptr base of an IndexedDF" << std::endl;
}

bool IndexedDFName::isIndexable() {
    //todo
    return true;
}

IndexedDFName::IndexedDFName(std::string name, Vertex* currentVertex, Identifier* base, std::vector<Expression*> expressionsVector) : 
    Identifier(name, nullptr, currentVertex, indexedDFNameClass, notCalculated /*todo when to init it accurately?*/) {

    if (
        (base->getClass() == baseDFNameClass) ||
        (base->getClass() == letNameClass) ||
        (base->getClass() == mutableArgNameClass)
    ){
        _base = base;
    } else {
        _base = nullptr;
        REPORTS.push_back(JsonReporter::createSYN11(name, currentVertex));
        std::cout << "INTERNAL ERROR: indexation of an unsuitable Identifier" << std::endl;
    }

    _expressionsVector = expressionsVector;
}

Expression* ForIteratorName::getLeftBorder(){
    if (m_vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<ForVertex*>(m_vertex)->getLeftBorder();
    }
}

Expression* ForIteratorName::getRightBorder(){
    if (m_vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<ForVertex*>(m_vertex)->getRightBorder();
    }
}

void ForIteratorName::markAsUse(Vertex* currentVertex, int size){
    m_useSet.insert(currentVertex);

    std::cout << "For iterator " << m_name << " is being marked as used" << std::endl;
    m_useSet.insert(currentVertex);
}

void ForIteratorName::markAsDef(Vertex* currentVertex, int size){
    m_defSet.insert(currentVertex);//todo is it ok?
    //todo: if marking as def, then there is only error 26 left; if not, then there will be more errors like using of nondefined shit
    // decide what philosophy we are pursuing here
    std::cout << "INTERNAL ERROR: for iterator " << m_name << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined by ForVertex automatically)
    
    REPORTS.push_back(JsonReporter::createSYN1(
        m_name,
        currentVertex
    ));
}

Expression* WhileIteratorName::getConditionExpr(){
    if (m_vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<WhileVertex*>(m_vertex)->getConditionExpr();
    }
}

Expression* WhileIteratorName::getStartExpr(){
    if (m_vertex == nullptr){
        return nullptr;
    } else {
        return dynamic_cast<WhileVertex*>(m_vertex)->getStartExpr();
    }
}

void WhileIteratorName::markAsUse(Vertex* currentVertex, int size){
    m_useSet.insert(currentVertex);

    std::cout << "While iterator " << m_name << " is being marked as used" << std::endl;
    m_useSet.insert(currentVertex);
}

void WhileIteratorName::markAsDef(Vertex* currentVertex, int size){
    m_defSet.insert(currentVertex);//todo is it ok?

    std::cout << "INTERNAL ERROR: while iterator " << m_name << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined by WhileVertex automatically)
    //todo
    REPORTS.push_back(JsonReporter::createSYN1(
        m_name,
        currentVertex
    ));
}

void LetName::markAsUse(Vertex* currentVertex, int size){
    m_useSet.insert(currentVertex);

    std::cout << "Let name " << m_name << " is being marked as used" << std::endl;
    m_reference->markAsUse(currentVertex, size);
}

void LetName::markAsDef(Vertex* currentVertex, int size){
    m_defSet.insert(currentVertex);

    std::cout << "Let name " << m_name << " is being marked as defined" << std::endl;
    m_reference->markAsDef(currentVertex, size);
}

void MutableArgName::markAsUse(Vertex* currentVertex, int size){
    m_useSet.insert(currentVertex);

    std::cout << "Mutable arg name name " << m_name << " is being marked as used" << std::endl;
    if (m_reference != nullptr)
        m_reference->markAsUse(currentVertex, size);
    else
        std::cout << "INTERNAL ERROR: nullptr at m_reference marking as used in a mutable arg name" << std::endl;
}

void MutableArgName::markAsDef(Vertex* currentVertex, int size){
    m_defSet.insert(currentVertex);

    std::cout << "Mutable arg name name " << m_name << " is being marked as defined" << std::endl;
    if (m_reference != nullptr)
        m_reference->markAsDef(currentVertex, size);
    else
        std::cout << "INTERNAL ERROR: nullptr at m_reference marking as defined in a mutable arg name" << std::endl;
}

void ImmutableArgName::markAsUse(Vertex* currentVertex, int size){
    m_useSet.insert(currentVertex);

    std::cout << "Immutable arg name " << m_name << " is being marked as used" << std::endl;
    if (m_reference != nullptr)
        m_reference->markAsUse(currentVertex, size);
    else
        std::cout << "INTERNAL ERROR: nullptr at m_reference marking as used in an immutable arg name" << std::endl;
}

void ImmutableArgName::markAsDef(Vertex* currentVertex, int size){
    m_defSet.insert(currentVertex);

    std::cout << "Immutable arg name " << m_name << " is being marked as defined" << std::endl;
    //todo
    REPORTS.push_back(JsonReporter::createSYN1(
        m_name,
        currentVertex
    ));
}

// it is used inside Expression constructor when engaging indexed name
//todo perhaps don't use that and use just constructor
IndexedDFName* parseIndexedDFExpression(expr* expression, std::map<std::string, Identifier*> nameTable, int line,
    Vertex* currentVertex){

    // expression could be either simple or a complex DF
    int indices = -1;
    std::string baseName;
    // complexDF is used to avoid corrupting expression object
    expr* complexDF = expression; // in our terms every DF based on a baseName is complex
    while(true){
        indices++;
        if (dynamic_cast<complex_id*>(complexDF) == nullptr){ // base name found (no indices)
            baseName = complexDF->to_string();
            break;
        } else {
            complexDF = dynamic_cast<complex_id*>(complexDF)->id_; // still has indices
        }
    }

    // now create an IndexedDFName and initialize it
    std::vector<Expression*> expressionsVector(indices);
    complex_id* newComplexDF = dynamic_cast<complex_id*>(expression);
    for (int i = 0; i < indices; i++){
        expressionsVector[indices - i - 1] = new Expression(newComplexDF->expr_, nameTable, currentVertex);
        newComplexDF = dynamic_cast<complex_id*>(newComplexDF->id_);
    }
    auto base = nameTable.find(baseName);
    if (base != nameTable.end()){

        if (base->second->isIndexable()){
            IndexedDFName* temp = new IndexedDFName(baseName, currentVertex, base->second, expressionsVector);
            std::cout << "Created new IndexedDFName object with base name " << baseName << " and expressionsVector: ";
            std::cout << std::flush;
            for (auto e: temp->getExpressionsVector()){
                std::cout << e->getASTExpr()->to_string() << " " << std::flush;
            }
            std::cout << std::endl;
        return temp;
        } else {
            std::cout << "INTERNAL ERROR: aborted creating new IndexedDFName object -- base is not indexable: " << baseName << std::endl;
            //std::string report = "ERROR: not indexable name \"" + baseName + "\" indexed at line " + std::to_string(line) + "\n";
            //todo
            REPORTS.push_back(JsonReporter::createSYN11(
                baseName,
                currentVertex
            ));
            return nullptr;
        }
    } else {
        std::cout << "INTERNAL ERROR: aborted creating new IndexedDFName object -- no base name found visible: " << baseName << std::endl;
        //std::string report = "ERROR: no name \"" + baseName + "\" found at line " + std::to_string(line) + "\n";
        REPORTS.push_back(JsonReporter::createSYN9(//todo
            base->second
        ));
        return nullptr;
    }
}
