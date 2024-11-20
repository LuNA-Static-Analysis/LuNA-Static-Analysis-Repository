#include "json_reporter.cpp"

bool Vertex::Binding::operator<(const Binding b) const {//todo check that it works (it was working afaik)
    if ((long)(_pointerTo) < (long)b._pointerTo){
        return true;
    } else if ((long)(_pointerFrom) > (long)(b._pointerFrom)){
        return true;
    } else if ((long)(_identifier) > (long)(b._identifier)){
        return true;
    } else {
        return false;
    }
}

void Vertex::bindTo(Vertex* pointerTo, Identifier* identifier) {
    auto binding = new Binding(pointerTo, this, identifier);
    m_outSet.insert(binding);
    pointerTo->m_inSet.insert(binding);
}

void Vertex::printCallStack(std::ostream* outputTarget){
    *outputTarget << "Address: " << this;
    *outputTarget << "; type: " << this->getVertexType();
    *outputTarget << "; line: " << this->getLine() << std::endl;
    if (this->getParent() != nullptr){
        this->getParent()->printCallStack(outputTarget);
    }
}

void Vertex::printGenericInfo(std::ostream* outputTarget) {
    *outputTarget << "Vertex address: " << this << std::endl;
    *outputTarget << "Vertex line: " << m_line << std::endl;
    *outputTarget << "Vertex depth: " << m_depth << std::endl;

    *outputTarget << "Declared outside DFs:";
    for (auto i: m_declaredOutsideIdsMap){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Declared inside DFs:";
    for (auto i: m_declaredInsideIdsMap){
        *outputTarget << " " << i.first;
    }
    *outputTarget << std::endl;

    *outputTarget << "Use DFs:";
    for (auto i: m_useSet){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Def DFs:";
    for (auto i: m_defSet){
        *outputTarget << " " << i;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices inside:";
    for (auto v: m_insideSet){
        *outputTarget << " " << v;
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices before (\"in\"):";
    for (auto b: m_inSet){
        //todo perhaps redo?
        *outputTarget << " " << b->getPointerTo() << "(" << b->getId() << ")";
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertices after (\"out\"):";
    for (auto b: m_outSet){
        *outputTarget << " " << b->getPointerTo() << "(" << b->getId() << ")";
    }
    *outputTarget << std::endl;

    *outputTarget << "Vertex type: ";
    std::string type;
    switch (m_vertexType) {
        case forVF: {
            type = "for";
            break;
        }
        case ifVF: {
            type = "if";
            break;
        }
        case whileVF: {
            type = "while";
            break;
        }
        case letVF: {
            type = "let";
            break;
        }
        case importVF: {
            type = "import";
            break;
        }
        case subVF: {
            type = "sub";
            break;
        }
    }
    *outputTarget << type << std::endl;

    *outputTarget << "Facts amount: ";
    *outputTarget << m_facts.size() << std::endl;
}

void SubVertex::initializeVertex() {
    _arguments = {};
    m_declaredBothIdsMap = {};
    m_declaredInsideIdsMap = {};
    m_declaredOutsideIdsMap = {};

    if (_callArgs.size() != _declaredArgs.size() && m_name != "main") {
        //todo wip report error (old code luna06)
        //REPORTS.push_back(JsonReporter::createSYN3());
        std::cout << "INTERNAL ERROR: call args size and declared args size are not equal in call " << m_name << " at line " << m_line << std::endl;
        return;
    }

    std::vector<Identifier*> declaredNamesVector = {};

    // in case of a "sub": add args as an inside Ids and map them to call args
    // IMPORTANT EXCEPTION: no mapping done to args of main(); also these names cannot be initialized and
    // indexed, so they are pretty special and have a class of their own: MainArgName

    // iterate through this vector and for every arg create a MutableArgName or an ImmutableArgName object
    for (int i = 0; i < _declaredArgs.size(); i++){
        auto declaredArg = _declaredArgs[i];

        // name of a declared argument
        std::string identifierDeclaredName = declaredArg.name;

        if (m_declaredBothIdsMap.find(identifierDeclaredName) == m_declaredBothIdsMap.end()){
            Identifier* argName = nullptr;
            if (m_name == "main") {
                if (declaredArg.type == nameType) {
                    argName = new MutableArgName(identifierDeclaredName, nullptr, this, nameType);
                } else {
                    argName = new ImmutableArgName(identifierDeclaredName, nullptr, this, declaredArg.type);
                }
            } else {
                if (declaredArg.type == nameType) {
                    argName = new MutableArgName(identifierDeclaredName, _callArgs[i], this, nameType/*todo calculate later dynamically*/);
                } else {
                    argName = new ImmutableArgName(identifierDeclaredName, _callArgs[i], this, declaredArg.type);
                }
            }

            _arguments.push_back(argName);
            m_declaredBothIdsMap.insert( { identifierDeclaredName, argName } );
            m_declaredInsideIdsMap.insert( { identifierDeclaredName, argName } );

        } else {
            std::cout << "INTERNAL ERROR: created nullptr sub/main arg name because of name duplication" << std::endl;

            std::vector<std::string> dfList = {};//todo
            //dfList.push_back(JsonReporter::createDF(identifierDeclaredName, "[]", "[]", "[]"));//todo callstacks
            //dfList.push_back(JsonReporter::createDF(identifierDeclaredName, "[]", "[]", "[]"));//todo callstacks
            REPORTS.push_back(JsonReporter::createSYN8(
                dfList
            ));
        }
    }

    enterBlock();
}

void ImportVertex::initializeVertex() {
    _arguments = {};

    std::cout << "Import entered" << std::endl;
    for (int i = 0; i < _callArgs.size(); i++){
        Expression* expression = _callArgs[i];
        switch(_declaredArgs[i].type){
            case nameType:
                if (expression != nullptr) {
                    expression->markAsDef(this, 0);//todo why zero??
                    break;
                }
            case intType:
            case realType:
            case stringType:
            case valueType:
                if (expression != nullptr) {
                    expression->markAsUse(this, 0);
                    break;
                }
            default:
                std::cout << "INTERNAL ERROR: initializeVertex -- import: found DF with unexpected UseDef!" << std::endl;
        }
    }
    return;
}

void ForVertex::initializeVertex() {
    const for_statement* innerStatementForVF = dynamic_cast<const for_statement*>(m_statement);

    // check for duplicate name
    std::string forIteratorString = innerStatementForVF->name_->to_string();
    if (m_declaredOutsideIdsMap.find(forIteratorString) != m_declaredOutsideIdsMap.end()){
        std::vector<std::string> dfList = {};
        Identifier* identifier = m_declaredOutsideIdsMap.find(forIteratorString)->second;
        dfList.push_back(JsonReporter::createDF(identifier));//todo callstacks
        //dfList.push_back(JsonReporter::createDF(forIteratorString, "[]", "[]", "[]"));//todo callstacks
        //todo find all duplicate dfs
        REPORTS.push_back(JsonReporter::createSYN8(
            dfList
        ));

        std::cout << "INTERNAL ERROR: aborted initializing \"for\" vertex" << std::endl;
        return;
    }

    _iterator = new ForIteratorName(forIteratorString, this);

    m_declaredInsideIdsMap.insert( { forIteratorString, _iterator } );
    m_declaredBothIdsMap.insert( { forIteratorString, _iterator } );

    // all names inside "for" expressions must be marked as used
    _leftBorder = new Expression(innerStatementForVF->expr_1_, m_declaredOutsideIdsMap, this);
    
    _rightBorder = new Expression(innerStatementForVF->expr_2_, m_declaredOutsideIdsMap, this);

    if (_leftBorder != nullptr)
        _leftBorder->markAsUse(this, 0);//todo why 0
    if (_rightBorder != nullptr)
        _rightBorder->markAsUse(this, 0);//todo why 0

    Expression* iteratorExpression = new Expression(_iterator, intNode, this);
    m_facts.insert(new GLeNFact(greaterOrEqualNode, iteratorExpression, _leftBorder));
    m_facts.insert(new GLeNFact(lesserOrEqualNode, iteratorExpression, _rightBorder));

    enterBlock();
}

void WhileVertex::initializeVertex() {
    const while_statement* innerStatementWhileVF = dynamic_cast<const while_statement*>(m_statement);

    // check for duplicate name
    std::string whileIteratorString = innerStatementWhileVF->left_->to_string();
    if (m_declaredOutsideIdsMap.find(whileIteratorString) != m_declaredOutsideIdsMap.end()){

        std::vector<std::string> dfList = {};
        Identifier* identifier = m_declaredOutsideIdsMap.find(whileIteratorString)->second;
        dfList.push_back(JsonReporter::createDF(identifier));//todo callstacks
        //dfList.push_back(JsonReporter::createDF(whileIteratorString, "[]", "[]", "[]"));//todo callstacks
        //todo find all duplicate dfs
        REPORTS.push_back(JsonReporter::createSYN8(
            dfList
        ));

        std::cout << "INTERNAL ERROR: aborted initializing \"while\" vertex" << std::endl;
        return;
    }

    _iterator = new WhileIteratorName(whileIteratorString, this);
    m_declaredInsideIdsMap.insert( { whileIteratorString, _iterator } );
    m_declaredBothIdsMap.insert( { whileIteratorString, _iterator } );

    Expression* whileOutNameExpr = new Expression(innerStatementWhileVF->id_, m_declaredOutsideIdsMap, this);
    _outName = whileOutNameExpr->getAsIdentifier();
    if ((_outName == nullptr) || ((_outName->getClass() != indexedDFNameClass) && (_outName->getClass() != mutableArgNameClass))){
        std::cout << "INTERNAL ERROR: unsuitable expression at \"while\" out name leads to nullpointer" << std::endl;
        _outName = nullptr;
    }

    // "while" out name is an IndexedDF that must be marked as "def"
    // everything inside condition and iterator start must be marked as used
    _conditionExpr = new Expression(innerStatementWhileVF->expr_, m_declaredOutsideIdsMap, this);

    _startExpr = new Expression(innerStatementWhileVF->right_, m_declaredOutsideIdsMap, this);

    if (_conditionExpr != nullptr)
        _conditionExpr->markAsUse(this, 0);//todo why 0

    if (_startExpr != nullptr)
        _startExpr->markAsUse(this, 0);//todo why 0

    if (_outName != nullptr){
        _outName->markAsDef(this, 0); //todo why 0
    }

    if (_outName == nullptr)
        REPORTS.push_back(JsonReporter::createSYN1(
            whileOutNameExpr->getASTExpr()->to_string(),
            this
        ));

    Expression* iteratorExpression = new Expression(_iterator, intNode, this);
    m_facts.insert(new GLeNFact(greaterOrEqualNode, iteratorExpression, _startExpr));
    m_facts.insert(new GLeNFact(nonEqualNode, _conditionExpr, new Expression("0", intNode, this)));

    enterBlock();
}

void IfVertex::initializeVertex() {
    const if_statement* innerStatementIfVF = dynamic_cast<const if_statement*>(m_statement);

    _conditionExpr = new Expression(innerStatementIfVF->expr_, m_declaredOutsideIdsMap, this);

    // all identifiers inside "if" expression must be marked as "used"
    if (_conditionExpr != nullptr)
        _conditionExpr->markAsUse(this, 0);

    m_facts.insert(new GLeNFact(nonEqualNode, _conditionExpr, new Expression("0", intNode, this)));

    enterBlock();
}

void LetVertex::initializeVertex() {
    const let_statement* innerStatementLetVF = dynamic_cast<const let_statement*>(m_statement);

    // map expressions to new letNames
    std::vector<assign*>* assignmentsVector = innerStatementLetVF->assign_seq_->assign_seq_;
    std::vector<LetName*>* letNamesVector = new std::vector<LetName*>();
    for (auto assignment: *assignmentsVector){

        // check for duplicate name
        std::string letString = *(assignment->name_->get_value());
        if (m_declaredOutsideIdsMap.find(letString) != m_declaredOutsideIdsMap.end()){

            std::vector<std::string> dfList = {};
            Identifier* identifier = m_declaredOutsideIdsMap.find(letString)->second;
            dfList.push_back(JsonReporter::createDF(identifier));//todo callstacks
            //dfList.push_back(JsonReporter::createDF(letString, "[]", "[]", "[]"));//todo callstacks
            //todo find all duplicate dfs
            REPORTS.push_back(JsonReporter::createSYN8(dfList));

            std::cout << "INTERNAL ERROR: aborted initializing \"let\" vertex" << std::endl;
            return;
        }

        Expression* letExpr = new Expression(assignment->expr_, m_declaredOutsideIdsMap, this);
        LetName* letName = new LetName(*(assignment->name_->get_value()), letExpr, this);
        m_declaredInsideIdsMap.insert( { letString, letName } );
        m_declaredBothIdsMap.insert( { letString, letName } );
        letNamesVector->push_back(letName);
    }

    enterBlock();
}

void Vertex::enterBlock() {
    std::cout << "Entering block " << m_block << "; name: " + m_name << std::endl;
    /* find DF declarations in current block; we can declare DF once in every block!
    scanForDFDecls returns map with no duplicates */
    scanForDFDecls();
    /* iterate through statements, collect vertices and their use-defs by calling initializeVertex on each,
    initialize currentVertex' use-defs and return it */
    iterateThroughBlockStatements();
    
    // SYN5.6
    if (m_vertexType == VertexType::subVF)
        dynamic_cast<SubVertex*>(this)->checkUnusedArgs();
}

// scanForDFDecls is a function that scans block for DF declarations
// DFs, according to LuNA rules, must be declared on the very first line, and no other declarations shall follow
//todo refactor, rename
void Vertex::scanForDFDecls() {
    // short-circuit magic for null pointer checking:
    if ((m_block->opt_dfdecls_ != NULL) && (m_block->opt_dfdecls_->dfdecls_ != NULL)) { // found some DF declarations

        // get names of declared DFs
        std::vector<luna_string*> DFNames = *(m_block->opt_dfdecls_->dfdecls_->name_seq_->names_);
        
        for (luna_string* nextLunaDFName: DFNames){
            std::string nextDFName = *(nextLunaDFName->value_);
            auto previousDFIterator = m_declaredBothIdsMap.find(nextDFName);
            if (previousDFIterator == m_declaredBothIdsMap.end()){
                BaseDFName* newBaseDFName = new BaseDFName(nextDFName, this, m_block->opt_dfdecls_->dfdecls_->line_);
                m_declaredInsideIdsMap.insert( { nextDFName, newBaseDFName } );
                m_declaredBothIdsMap.insert( { nextDFName, newBaseDFName } );
                BASENAMES.insert(newBaseDFName);
            } else {
                Identifier* previousDF = previousDFIterator->second;
                // error code: 13
                // df list
                std::vector<std::string> dfList = {};
                dfList.push_back(JsonReporter::createDF(previousDF));//todo callstacks
                //dfList.push_back(JsonReporter::createDF(dfName, "[]", "[]", "[]"));//todo callstacks
                //todo find all duplicate dfs (just create new and then immediately delete)
                REPORTS.push_back(JsonReporter::createSYN8(
                    dfList
                ));
            }
        }

        std::cout << "DFs in block " << m_block << ":";
        for (auto nextDFName: m_declaredInsideIdsMap){
            std::cout << " " << nextDFName.second->getName();
        }
        std::cout << std::endl;

    } else {
        std::cout << "No DF declarations found" << std::endl;
    }
}

//todo redo this: do not use dynamic cast
void Vertex::iterateThroughBlockStatements() {
    // iterate through current block's statements
    int statementNumber = 0; // for logging
    for (statement* innerStatement: *(m_block->statement_seq_->statements_)){
        statementNumber++;
        std::cout << "Statement number " << statementNumber << ": " << std::endl;

        // ---- handling cf
        cf_statement* cfStatement = dynamic_cast<cf_statement*>(innerStatement);
        if (cfStatement != NULL){
            std::string cfName = *(cfStatement->code_id_->value_);
            auto cfDeclaration = CFDECLARATIONS.find(*(cfStatement->code_id_->value_));
            if (cfDeclaration != CFDECLARATIONS.end()) {
                if (cfDeclaration->second.type == importCF) {
                    handleImport(cfStatement);
                } else {
                    handleSub(cfStatement);
                }
            } else {
                std::cout << "INTERNAL ERROR: no sub with name " << cfName << " found" << std::endl;
                // error code: 02
                // callstack entry
                REPORTS.push_back(JsonReporter::createSYN2(
                    m_fileName,
                    cfStatement->line_,
                    cfName
                ));
            }
            continue;
        }

        // ---- handling for
        for_statement* forStatement = dynamic_cast<for_statement*>(innerStatement);
        if (forStatement != NULL){
            handleFor(forStatement);
            continue;
        }

        // ---- handling while
        // example: while (128 < 129), i = 0 .. out N { /* body */ }
        while_statement* whileStatement = dynamic_cast<while_statement*>(innerStatement);
        if (whileStatement != NULL){
            handleWhile(whileStatement);
            continue;
        }

        // ---- handling if
        // example: if 128 < 129 { /* body */ }
        if_statement* ifStatement = dynamic_cast<if_statement*>(innerStatement);
        if (ifStatement != NULL){
            handleIf(ifStatement);
            continue;
        }

        // ---- handling let
        // example: let b = a[1], message = "Success" { /* body */ }
        let_statement* letStatement = dynamic_cast<let_statement*>(innerStatement);
        if (letStatement != NULL){
            handleLet(letStatement);
            continue;
        }

        // TODO ---- handling other statements
    }
}

void Vertex::handleSub(cf_statement* cfStatement) {
    std::string calledSubName = *(cfStatement->code_id_->value_);

    // vector of call args of type expr (raw expressions)
    std::vector<expr*>* rawArguments;
    if (cfStatement->opt_exprs_->exprs_seq_ != nullptr)
        rawArguments = cfStatement->opt_exprs_->exprs_seq_->expr_;
    else
        rawArguments = new std::vector<expr*>();
    // create Expression objects for call args
    std::vector<Expression*> callArgs = {};
    for (auto rawArgument : *rawArguments)
        callArgs.push_back(new Expression(rawArgument, m_declaredBothIdsMap, this));

    auto cfDeclarationPair = CFDECLARATIONS.find(calledSubName);
    if (cfDeclarationPair == CFDECLARATIONS.end()) {
        //todo report non-existing CF?
        std::cout << "INTERNAL ERROR" << std::endl;
        return;
    }
    CFDeclaration cfDeclaration = cfDeclarationPair->second; // todo this will be deleted? do pointers?
    CFDECLARATIONS.find(calledSubName)->second.isUsed = true;

    SubVertex* nextVertex = new SubVertex(calledSubName, this, subVF, m_depth + 1, cfStatement->line_, m_fileName, cfDeclaration.cfBlock, cfStatement, m_declaredBothIdsMap, m_facts, callArgs, cfDeclaration.declaredArgs);
    if (cfDeclaration.declaredArgs.size() != callArgs.size()) {
        REPORTS.push_back(JsonReporter::createSYN3(nextVertex));
        std::cout << "INTERNAL ERROR: call of CF " << calledSubName << " has wrong amount of args" << std::endl;
        return;
    } else {
        for (int i = 0; i < cfDeclaration.declaredArgs.size(); i++) {
            if (callArgs[i]->getValueType() != cfDeclaration.declaredArgs[i].type) {
                REPORTS.push_back(JsonReporter::createSYN1(
                    callArgs[i]->getASTExpr()->to_string(),
                    nextVertex
                ));
            }
        }
    }
    
    VERTICES.push_back(nextVertex);
    addInside(nextVertex);
    nextVertex->initializeVertex();
}

void Vertex::handleImport(cf_statement* cfStatement) {
    std::string calledImportName = *(cfStatement->code_id_->value_);

    // vector of call args of type expr (raw expressions)
    std::vector<expr*>* rawArguments;
    if (cfStatement->opt_exprs_->exprs_seq_ != nullptr)
        rawArguments = cfStatement->opt_exprs_->exprs_seq_->expr_;
    else
        rawArguments = new std::vector<expr*>();
    // create Expression objects for call args
    std::vector<Expression*> callArgs = {};

    for (auto rawArgument : *rawArguments)
        callArgs.push_back(new Expression(rawArgument, m_declaredBothIdsMap, this));//TODO WIP this is wrong, create Identifier and send it (update 23.10.2024 what the fuck does that mean)

    auto cfDeclarationPair = CFDECLARATIONS.find(calledImportName);
    if (cfDeclarationPair == CFDECLARATIONS.end()) {
        //todo report non-existing CF?
        std::cout << "INTERNAL ERROR" << std::endl;
        return;
    }
    CFDeclaration cfDeclaration = cfDeclarationPair->second; // todo this will be deleted? do pointers?
    CFDECLARATIONS.find(calledImportName)->second.isUsed = true;

    ImportVertex* nextVertex = new ImportVertex(calledImportName, this, importVF, m_depth + 1, cfStatement->line_, m_fileName, cfStatement->block_, cfStatement, m_declaredBothIdsMap, m_facts, callArgs, cfDeclaration.declaredArgs);
    //todo wip check for LUNA04 and LUNA06
    if (cfDeclaration.declaredArgs.size() != callArgs.size()) {
        REPORTS.push_back(JsonReporter::createSYN3(nextVertex));
        std::cout << "INTERNAL ERROR: call of CF " << calledImportName << " has wrong amount of args" << std::endl;
        return;
    } else {
        for (int i = 0; i < cfDeclaration.declaredArgs.size(); i++) {
            if (callArgs[i]->getValueType() != cfDeclaration.declaredArgs[i].type) {
                REPORTS.push_back(JsonReporter::createSYN1(
                    callArgs[i]->getASTExpr()->to_string(),
                    nextVertex
                ));
            }
        }
    }

    VERTICES.push_back(nextVertex);
    addInside(nextVertex);
    nextVertex->initializeVertex();
}

void Vertex::handleFor(for_statement* forStatement) {
    ForVertex* nextVertex = new ForVertex(this, m_depth + 1, forStatement->line_, m_fileName, forStatement->block_, forStatement, m_declaredBothIdsMap, m_facts);
    VERTICES.push_back(nextVertex);
    addInside(nextVertex);
    nextVertex->initializeVertex();
}

void Vertex::handleWhile(while_statement* whileStatement) {
    WhileVertex* nextVertex = new WhileVertex(this, m_depth + 1, whileStatement->line_, m_fileName, whileStatement->block_, whileStatement, m_declaredBothIdsMap, m_facts);
    VERTICES.push_back(nextVertex);
    addInside(nextVertex);
    nextVertex->initializeVertex();
}

void Vertex::handleIf(if_statement* ifStatement) {
    IfVertex* nextVertex = new IfVertex(this, m_depth + 1, ifStatement->line_, m_fileName, ifStatement->block_, ifStatement, m_declaredBothIdsMap, m_facts);
    VERTICES.push_back(nextVertex);
    addInside(nextVertex);
    nextVertex->initializeVertex();
}

void Vertex::handleLet(let_statement* letStatement) {
    LetVertex* nextVertex = new LetVertex(this, m_depth + 1, letStatement->line_, m_fileName, letStatement->block_, letStatement, m_declaredBothIdsMap, m_facts);
    VERTICES.push_back(nextVertex);
    addInside(nextVertex);
    nextVertex->initializeVertex();
}

void SubVertex::printInfo(std::ostream* outputTarget) {
    printGenericInfo(outputTarget);

    *outputTarget << "Exact type: structured CF, name: " << this->getName() << std::endl;
}

void SubVertex::checkUnusedArgs() {
    for (auto arg : _arguments) {
        if (arg->getUseSet().empty())
            REPORTS.push_back(JsonReporter::createSYN5_6(arg));
    }
}

void ImportVertex::printInfo(std::ostream* outputTarget) {
    printGenericInfo(outputTarget);

    *outputTarget << "Exact type: atomic CF, name: " << this->getName() << std::endl;
}

void ForVertex::printInfo(std::ostream* outputTarget){
    printGenericInfo(outputTarget);

    *outputTarget << "Iterator: " + this->getIterator()->getName() << std::endl;
    *outputTarget << "Left border: " + this->getLeftBorder()->getASTExpr()->to_string() << std::endl;
    *outputTarget << "Right border: " + this->getRightBorder()->getASTExpr()->to_string() << std::endl;
}

void WhileVertex::printInfo(std::ostream* outputTarget){
    printGenericInfo(outputTarget);

    *outputTarget << "Iterator: " + _iterator->getName() << std::endl;
    *outputTarget << "Out name: " + _outName->getName() << std::endl;
    *outputTarget << "Condition expression: " + _conditionExpr->getASTExpr()->to_string() << std::endl;
    *outputTarget << "Start expression: " + _startExpr->getASTExpr()->to_string() << std::endl;
}

void IfVertex::printInfo(std::ostream* outputTarget){
    printGenericInfo(outputTarget);

    *outputTarget << "Condition expression: " + this->getConditionExpr()->getASTExpr()->to_string() << std::endl;
}

void LetVertex::printInfo(std::ostream* outputTarget){
    printGenericInfo(outputTarget);

    *outputTarget << "Assigned expressions and their names: " << std::endl;
    for (auto assignment: getLetNamesVector()){
        *outputTarget << assignment->getName() << " = " << assignment->getReference()->getASTExpr()->to_string() << std::endl;
    }
}
