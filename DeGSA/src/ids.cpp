#include "ids.hpp"
#include "json_reporter.cpp"

//todo check if this is true
int Identifier::getLine() const{
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



void BaseDFName::markAsUse(Vertex* vertex, const std::vector<Expression*>& indexesExpression){
    std::cout << "Base DF name " << m_name << " is being marked as used, nothing happens" << std::endl;
}

void BaseDFName::markAsDef(Vertex* vertex, const std::vector<Expression*>& indexesExpression){
    std::cout << "Base DF name " << m_name << " is being marked as defined, nothing happens" << std::endl;
}

TrueIndexedDFData BaseDFName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    return TrueIndexedDFData(this, currentIndexedDF.indicesExpressions);
}



IndexedDFName::IndexedDFName(Vertex* currentVertex, BaseDFName* trueBaseName, std::vector<Expression*> expressionsVector) : 
    Identifier(trueBaseName->getName(), nullptr, currentVertex, indexedDFNameClass, notCalculated /*todo when to init it accurately?*/) {
    _base = trueBaseName;
    _expressionsVector = expressionsVector;
}



IndexedDFValueName::IndexedDFValueName(Vertex* currentVertex, BaseDFName* trueBaseName, std::vector<Expression*> expressionsVector) : 
    IndexedDFName(currentVertex, trueBaseName, expressionsVector) {
    _base->AddNewIndexedDFValueName(this);
}

void IndexedDFValueName::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_useSet.insert(currentVertex);

    std::cout << "Indexed DF value name " << m_name << " of size " << _expressionsVector.size() << " is being marked as used" << std::endl;
    for (auto exp: _expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFValueName.markAsUse()" << std::endl;
        } else {
            exp->markAsUse(currentVertex, indexesExpression);
        }
    }

    if (_base == nullptr)
        logInternalError("markAsUse encountered nullptr base of an IndexedDFValueName");
}

void IndexedDFValueName::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_defSet.insert(currentVertex);

    std::cout << "Indexed DF value name " << m_name << " of size " << _expressionsVector.size() << " is being marked as defined" << std::endl;
    for (auto exp: _expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFValueName.markAsDef()" << std::endl;
        } else {
            exp->markAsUse(currentVertex, indexesExpression);
        }
    }
    
    if (_base == nullptr)
        logInternalError("markAsDef encountered nullptr base of an IndexedDFValueName");
}

TrueIndexedDFData IndexedDFValueName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    auto wholeExpressionsVector = getExpressionsVector();
    for (auto exp : currentIndexedDF.indicesExpressions)
        wholeExpressionsVector.push_back(exp);
    auto tempData = TrueIndexedDFData(currentIndexedDF.trueBaseName, wholeExpressionsVector);
    return _base->getTrueBaseNameIndexedDF(tempData);
}

// it is used inside Expression constructor when engaging indexed name
//todo perhaps don't use that and use just constructor
IndexedDFValueName* IndexedDFValueName::TryCreateIndexedDFValueName(
    expr* expression, std::map<std::string, Identifier*> nameTable, int line,
    Vertex* currentVertex){

    // expression could be either simple or a complex DF
    int indices = -1;
    std::string untrueBaseName;
    const expr* complexDF = expression; // in our terms every DF based on a baseName is complex
    while(true){
        indices++;
        if (dynamic_cast<const complex_id*>(complexDF) == nullptr){ // base name found (no indices)
            untrueBaseName = complexDF->to_string();
            break;
        } else {
            complexDF = dynamic_cast<const complex_id*>(complexDF)->id_; // still has indices
        }
    }

    // now create an IndexedDFValueName and initialize it
    std::vector<Expression*> expressionsVector(indices);
    const complex_id* newComplexDF = dynamic_cast<const complex_id*>(expression);
    for (int i = 0; i < indices; i++){
        expressionsVector[indices - i - 1] = new Expression(newComplexDF->expr_, nameTable, currentVertex);
        newComplexDF = dynamic_cast<const complex_id*>(newComplexDF->id_);
    }
    auto untrueBaseEntry = nameTable.find(untrueBaseName);
    if (untrueBaseEntry != nameTable.end()){ // untrue name was resolved! no syntax error therefore
        Identifier* untrueBase = untrueBaseEntry->second;

        // now get true BaseDFName
        TrueIndexedDFData currentIndexedDFData(nullptr, expressionsVector);
        TrueIndexedDFData data = untrueBase->getTrueBaseNameIndexedDF(currentIndexedDFData);

        if (data.trueBaseName != nullptr){
            IndexedDFValueName* temp = new IndexedDFValueName(currentVertex, data.trueBaseName, data.indicesExpressions);
            std::cout << "Created new IndexedDFName object with base name " << data.trueBaseName->getName() << " and expressionsVector: ";
            std::cout << std::flush;
            for (auto e: temp->getExpressionsVector()){
                std::cout << e->getASTExpr()->to_string() << " " << std::flush;
            }
            std::cout << std::endl;
            return temp;
        } else {
            std::cout << "INTERNAL ERROR: aborted creating new IndexedDFValueName object -- base is not indexable: " << untrueBaseName << std::endl;
            //todo
            REPORTS.push_back(JsonReporter::createSYN11(
                untrueBaseName,
                currentVertex
            ));
            return nullptr;
        }
    } else {
        std::cout << "INTERNAL ERROR: aborted creating new IndexedDFName object -- no base name found visible: " << untrueBaseName << std::endl;
        /*REPORTS.push_back(JsonReporter::createSYN9(//todo
            untrueBase
        ));*/
        return nullptr;
    }
}



IndexedDFAliasName::IndexedDFAliasName(Vertex* currentVertex, BaseDFName* trueBaseName, std::vector<Expression*> expressionsVector) : 
    IndexedDFName(currentVertex, trueBaseName, expressionsVector) {
    _base->AddNewIndexedDFAliasName(this);
}

void IndexedDFAliasName::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_useSet.insert(currentVertex);

    std::cout << "Indexed DF alias name " << m_name << " of size " << _expressionsVector.size() << " is being marked as used" << std::endl;
    for (auto exp: _expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFAliasName.markAsUse()" << std::endl;
        } else {
            exp->markAsUse(currentVertex, indexesExpression);
        }
    }

    if (_base == nullptr)
        logInternalError("markAsUse encountered nullptr base of an IndexedDFAliasName");
}

void IndexedDFAliasName::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_defSet.insert(currentVertex);

    std::cout << "Indexed DF alias name " << m_name << " of size " << _expressionsVector.size() << " is being marked as defined" << std::endl;
    for (auto exp: _expressionsVector){
        if (exp == nullptr){
            std::cout << "INTERNAL ERROR: encountered nullptr Expression at IndexedDFAliasName.markAsDef()" << std::endl;
        } else {
            exp->markAsUse(currentVertex, indexesExpression);
        }
    }
    
    if (_base == nullptr)
        logInternalError("markAsDef encountered nullptr base of an IndexedDFAliasName");
}

TrueIndexedDFData IndexedDFAliasName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    auto wholeExpressionsVector = getExpressionsVector();
    for (auto exp : currentIndexedDF.indicesExpressions)
        wholeExpressionsVector.push_back(exp);
    auto tempData = TrueIndexedDFData(currentIndexedDF.trueBaseName, wholeExpressionsVector);
    return _base->getTrueBaseNameIndexedDF(tempData);
}

// it is used inside Expression constructor when engaging indexed name
//todo perhaps don't use that and use just constructor
IndexedDFAliasName* IndexedDFAliasName::TryCreateIndexedDFAliasName(expr* expression, std::map<std::string, Identifier*> nameTable, int line,
    Vertex* currentVertex){

    // expression could be either simple or a complex DF
    int indices = -1;
    std::string untrueBaseName;
    // complexDF is used to avoid corrupting expression object
    expr* complexDF = expression; // in our terms every DF based on a baseName is complex
    while(true){
        indices++;
        if (dynamic_cast<complex_id*>(complexDF) == nullptr){ // base name found (no indices)
            untrueBaseName = complexDF->to_string();
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
    auto untrueBaseEntry = nameTable.find(untrueBaseName);
    if (untrueBaseEntry != nameTable.end()){
        Identifier* untrueBase = untrueBaseEntry->second;

        TrueIndexedDFData currentIndexedDFData(nullptr, expressionsVector);
        TrueIndexedDFData data = untrueBase->getTrueBaseNameIndexedDF(currentIndexedDFData);

        if (data.trueBaseName != nullptr){
            IndexedDFAliasName* temp = new IndexedDFAliasName(currentVertex, data.trueBaseName, data.indicesExpressions);
            std::cout << "Created new IndexedDFAliasName object with base name " << data.trueBaseName->getName() << " and expressionsVector: ";
            std::cout << std::flush;
            for (auto e: temp->getExpressionsVector()){
                std::cout << e->getASTExpr()->to_string() << " " << std::flush;
            }
            std::cout << std::endl;
            return temp;
        } else {
            std::cout << "INTERNAL ERROR: aborted creating new IndexedDFAliasName object -- base is not indexable: " << untrueBaseName << std::endl;
            //todo
            REPORTS.push_back(JsonReporter::createSYN11(
                untrueBaseName,
                currentVertex
            ));
            return nullptr;
        }
    } else {
        std::cout << "INTERNAL ERROR: aborted creating new IndexedDFAliasName object -- no base name found visible: " << untrueBaseName << std::endl;
        /*REPORTS.push_back(JsonReporter::createSYN9(//todo
            untrueBase
        ));*/
        return nullptr;
    }
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

void ForIteratorName::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_useSet.insert(currentVertex);

    std::cout << "For iterator " << m_name << " is being marked as used" << std::endl;
    m_useSet.insert(currentVertex);
}

void ForIteratorName::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    std::cout << "INTERNAL ERROR: for iterator " << m_name << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined at the time of a ForVertex creation)
    REPORTS.push_back(JsonReporter::createSYN1(
        m_name,
        currentVertex,
        nullptr
    ));
}

TrueIndexedDFData ForIteratorName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    logInternalError("getTrueBaseNameIndexedDF used on ForIterator");
    return TrueIndexedDFData(nullptr, currentIndexedDF.indicesExpressions);
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

void WhileIteratorName::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_useSet.insert(currentVertex);
    std::cout << "While iterator " << m_name << " is being marked as used" << std::endl;
}

void WhileIteratorName::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    std::cout << "INTERNAL ERROR: while iterator " << m_name << " is being marked as defined" << std::endl;
    // error (iterator should be marked as defined at the time of a WhileVertex creation)
    REPORTS.push_back(JsonReporter::createSYN1(
        m_name,
        currentVertex,
        nullptr
    ));
}

TrueIndexedDFData WhileIteratorName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    logInternalError("getTrueBaseNameIndexedDF used on WhileIterator");
    return TrueIndexedDFData(nullptr, currentIndexedDF.indicesExpressions);
}



void LetName::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_useSet.insert(currentVertex);

    std::cout << "Let name " << m_name << " is being marked as used" << std::endl;
    m_reference->markAsUse(currentVertex, indexesExpression);
}

void LetName::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_defSet.insert(currentVertex);

    std::cout << "Let name " << m_name << " is being marked as defined" << std::endl;
    m_reference->markAsDef(currentVertex, indexesExpression);
}

TrueIndexedDFData LetName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    return m_reference->getTrueBaseNameIndexedDF(currentIndexedDF);
}



void MutableArgName::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_useSet.insert(currentVertex);

    std::cout << "Mutable arg name name " << m_name << " is being marked as used" << std::endl;
    if (m_reference != nullptr)
        m_reference->markAsUse(currentVertex, indexesExpression);
    else
        std::cout << "INTERNAL ERROR: nullptr at m_reference marking as used in a mutable arg name" << std::endl;
}

void MutableArgName::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_defSet.insert(currentVertex);

    std::cout << "Mutable arg name name " << m_name << " is being marked as defined" << std::endl;
    if (m_reference != nullptr)
        m_reference->markAsDef(currentVertex, indexesExpression);
    else
        std::cout << "INTERNAL ERROR: nullptr at m_reference marking as defined in a mutable arg name" << std::endl;
}

TrueIndexedDFData MutableArgName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    return m_reference->getTrueBaseNameIndexedDF(currentIndexedDF);
}



void ImmutableArgName::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_useSet.insert(currentVertex);

    std::cout << "Immutable arg name " << m_name << " is being marked as used" << std::endl;
    if (m_reference != nullptr)
        m_reference->markAsUse(currentVertex, indexesExpression);
    else
        std::cout << "INTERNAL ERROR: nullptr at m_reference marking as used in an immutable arg name" << std::endl;
}

void ImmutableArgName::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    m_defSet.insert(currentVertex);

    std::cout << "Immutable arg name " << m_name << " is being marked as defined" << std::endl;
    //todo
    REPORTS.push_back(JsonReporter::createSYN1(
        m_name,
        currentVertex,
        nullptr
    ));
}

TrueIndexedDFData ImmutableArgName::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    logInternalError("getTrueBaseNameIndexedDF used on ImmutableArg");
    return TrueIndexedDFData(nullptr, currentIndexedDF.indicesExpressions);
}
