#include "expr.hpp"
#include "json_reporter.cpp"

#include "../symengine-0.14.0/symengine/refine.h"

using SymEngine::Assumptions;

unsigned int AliasTable::nextNumber = 0;
std::vector<std::pair<std::string, Identifier*>> AliasTable::table = {};
std::vector<AliasTable::ExpressionAssumption> AliasTable::ruleSet = {};

std::optional<std::pair<std::string, Identifier*>> AliasTable::GetIdentifierByAlias(const std::string& iAlias) {
    for (int i = 0; i < AliasTable::table.size(); i++)
        if (AliasTable::table[i].first == iAlias)
            return table[i];

    return std::nullopt;
}

std::optional<std::pair<std::string, Identifier*>> AliasTable::GetAliasByIdentifier(Identifier* iIdentifier) {
    for (int i = 0; i < AliasTable::table.size(); i++)
        if (AliasTable::table[i].second == iIdentifier)
            return AliasTable::table[i];

    return std::nullopt;
}

std::optional<std::string> AliasTable::CreateAlias(Identifier* iIdentifier) {
    auto maybeMapping = AliasTable::GetAliasByIdentifier(iIdentifier);
    if (!maybeMapping) {
        std::string nextAlias = "t" + std::to_string(nextNumber);
        nextNumber++;
        AliasTable::table.push_back({ nextAlias, iIdentifier });
        return nextAlias;
    } else // Identifier already has a mapping
        return std::nullopt;
}

bool AliasTable::AddRule(std::string iLeft, std::string iRight, AliasTable::OperationType iOperation) {//todo operation type
    auto newRule = AliasTable::ExpressionAssumption(iLeft, iRight, iOperation);
    for (int i = 0; i < AliasTable::ruleSet.size(); i++) {
        if (AliasTable::ruleSet[i].Equals(newRule))
            return false;
    }
    AliasTable::ruleSet.push_back(newRule);
    return true;
}


/* 
    this function gets an expression, recursively goes through it and
    returns an obejct of type Expression, plus creates objects for IndexedDFs
    nameTable stores information about what Ids are visible currently, and we can
    find the Identifier object by its name
    if there is no required Identifiers, then it's an error
*/
//TODO stop parsing strings and use jsons or inheritance
Expression::Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, Vertex* currentVertex){

    this->_ASTexpr = ASTexpr;

    // default initialization
    this->_constant = "";
    this->_identifier = nullptr;
    this->_leftExpr = nullptr;
    this->_rightExpr = nullptr;
    this->_expressionType = noneNode;
    this->_valueType = notCalculated;
    this->_vertex = currentVertex;

    luna_string* lunaString = dynamic_cast<luna_string*>(ASTexpr);
    if (lunaString != NULL){

        std::string s = *(lunaString->get_value());
        if (std::regex_match(s, std::regex("[0-9]+"))){ // int
            this->_expressionType = intNode;
        }
        if (std::regex_match(s, std::regex("[0-9]+[.][0-9]+"))){ // real
            this->_expressionType = realNode;
        }
        if (std::regex_match(s, std::regex("\"[^\"]*\""))){ // string
            this->_expressionType = stringNode;
        }

        this->_constant = s;
        return;
    }

    luna_cast* lunaCast = dynamic_cast<luna_cast*>(ASTexpr);
    if (lunaCast != nullptr){

        to_int* intCast = dynamic_cast<to_int*>(lunaCast);
        if (intCast != nullptr)
            this->_expressionType = intCastNode;
        
        to_real* realCast = dynamic_cast<to_real*>(lunaCast);
        if (realCast != nullptr)
            this->_expressionType = realCastNode;

        to_str* stringCast = dynamic_cast<to_str*>(lunaCast);
        if (stringCast != nullptr)
            this->_expressionType = stringCastNode;

        this->_leftExpr = new Expression(lunaCast->expr_, nameTable, currentVertex);
        return;
    }

    bin_op* lunaBinOp = dynamic_cast<bin_op*>(ASTexpr);
    if (lunaBinOp != NULL){

        // deduction of binary operation type
        std::string op = (lunaBinOp->to_string()).substr(lunaBinOp->left_->to_string().size(), 
            lunaBinOp->to_string().size() - lunaBinOp->left_->to_string().size() - lunaBinOp->right_->to_string().size());
        while (op[0] == ' '){
            op = op.substr(1, op.size() - 1);
        }
        switch(op[0]){
            case '+': _expressionType = addNode; break;
            case '-': _expressionType = subtractNode; break;
            case '*': _expressionType = multiplyNode; break;
            case '/': _expressionType = divideNode; break;
            case '%': _expressionType = modulusNode; break;
            case '>':
                if (op.size() > 1 && op[1] == '=') {
                    _expressionType = greaterOrEqualNode; break;
                } else {
                    _expressionType = greaterNode; break;
                }
            case '<':
                if (op.size() > 1 && op[1] == '=') {
                    _expressionType = lesserOrEqualNode; break;
                } else {
                    _expressionType = lesserNode; break;
                }
            case '=': if (op.size() > 1 && op[1] == '=') { _expressionType = equalNode; break; }
            case '!': if (op.size() > 1 && op[1] == '=') { _expressionType = nonEqualNode; break; }
            case '&':
                if (op.size() > 1 && op[1] == '&') {
                    _expressionType = andNode;
                }
                break;
            case '|':
                if (op.size() > 1 && op[1] == '|') {
                    _expressionType = andNode;
                }
                break;
            default: std::cout << "INTERNAL ERROR: unknown operation in Expression constructor at line " << ASTexpr->line_ << ": " << op[0] << std::endl;
                _expressionType = noneNode;
        }

        _leftExpr = new Expression(lunaBinOp->left_, nameTable, currentVertex);
        _rightExpr = new Expression(lunaBinOp->right_, nameTable, currentVertex);
        return;
    }

    // name found
    id* df = dynamic_cast<id*>(ASTexpr);
    if (df != NULL){

        _expressionType = identifierNode;

        simple_id* simpleDF = dynamic_cast<simple_id*>(ASTexpr);
        // this is whatever name -- for iterator, while iterator, DF...
        // it is not necessarily a correct expression! it could be an indexed "for" iterator, for example
        if (simpleDF != NULL){

            std::string simpleDFName = *(simpleDF->value_->value_);
            auto identifierName = nameTable.find(simpleDFName);
            if (identifierName != nameTable.end()){

                switch(identifierName->second->getClass()){
                    case baseDFNameClass: { // create simple DF (no indices)
                        //todo how to understand if this is alias or not?
                        //perhaps depending on the vertex type?
                        //temporary solution: just use value everywhere, then revalidate architecture
                        _identifier = IndexedDFValueName::TryCreateIndexedDFValueName(simpleDF, nameTable, ASTexpr->line_, currentVertex);
                        break;
                    }
                    case forIteratorNameClass:
                    case whileIteratorNameClass:
                    case valueNameClass:
                    case letNameClass:
                    case mutableArgNameClass:
                    case immutableArgNameClass: {
                        _identifier = identifierName->second;
                        break;
                    }
                    default: {
                        std::cout << "INTERNAL ERROR: unknown identifier type" << std::endl;
                        break;
                    }
                }
            } else {
                //todo fake Id to use it in report
                //todo what is this even
                Identifier* nonExistingIdentifier = new ForIteratorName(simpleDFName, _vertex);
                //nonExistingIdentifier->markAsUse(currentVertex, 0);
                //todo
                REPORTS.push_back(JsonReporter::createSYN9(
                    nonExistingIdentifier
                ));
            }

            return;
        }
        
        complex_id* complexDF = dynamic_cast<complex_id*>(ASTexpr);
        if (complexDF != NULL){

            // creating DF with more than zero indices
            //todo same temporary solution here
            IndexedDFValueName* temp = IndexedDFValueName::TryCreateIndexedDFValueName(complexDF, nameTable, ASTexpr->line_, currentVertex);
            if (temp != nullptr){
                _identifier = temp;
            } else {
                std::cout << "INTERNAL ERROR: creating Expression object (IndexedDFName) at line " + std::to_string(ASTexpr->line_) + "\n";
            }

            return;
        }

        std::cout << "INTERNAL ERROR: creating Expression object (Identifier) at line " + std::to_string(ASTexpr->line_) + "\n";
    }

    std::cout << "INTERNAL ERROR: creating Expression object at line " + std::to_string(ASTexpr->line_) + "\n";
    return;
}

void Expression::calculateValueType(){
    switch(_expressionType) {
        case addNode:
        case subtractNode:
        case multiplyNode:
        case divideNode:
        case modulusNode: {
            ExpressionType leftValueType = _leftExpr->getExpressionType();
            ExpressionType rightValueType = _rightExpr->getExpressionType();
            if (leftValueType == realNode && rightValueType == realNode ||
                leftValueType == intNode && rightValueType == realNode || 
                leftValueType == realNode && rightValueType == intNode){ // result is real
                if (_expressionType == modulusNode)
                    _valueType = nonCalculatable; // modulus can not be used with real
                else
                    _valueType = realType;
            } else if (leftValueType == intNode && rightValueType == intNode){ // result is int
                _valueType = intType;
            } else { // erroreous operation
                _valueType = nonCalculatable;
            }
            return;
        }

        case greaterNode:
        case greaterOrEqualNode:
        case lesserNode:
        case lesserOrEqualNode:
        case equalNode:
        case nonEqualNode:
        case andNode:
        case orNode:
        case intNode:
        case intCastNode: {
            _valueType = intType;
            return;
        }

        case stringNode:
        case stringCastNode: {
            _valueType = stringType;
            return;
        }

        case realNode:
        case realCastNode: {
            _valueType = realType;
            return;
        }

        // identifier
        case identifierNode: {
            Identifier* identifier = getAsIdentifier();
            if (identifier != nullptr)
                _valueType = identifier->getValueType();
            else
                _valueType = nonCalculatable;
            return;
        }

        case noneNode: {
            _valueType = nonCalculatable;
            return;
        }
    }
}

std::string Expression::getAsTrueString(){
    switch(_expressionType) {
        case addNode: {
            return "(" + _leftExpr->getAsTrueString() + "+" + _rightExpr->getAsTrueString() + ")";
        }
        case subtractNode: {
            return "(" + _leftExpr->getAsTrueString() + "-" + _rightExpr->getAsTrueString() + ")";
        }
        case multiplyNode: {
            return "(" + _leftExpr->getAsTrueString() + "*" + _rightExpr->getAsTrueString() + ")";
        }
        case divideNode: {
            return "(" + _leftExpr->getAsTrueString() + "/" + _rightExpr->getAsTrueString() + ")";
        }
        case modulusNode: {
            return "(" + _leftExpr->getAsTrueString() + "%" + _rightExpr->getAsTrueString() + ")";
        }
        case greaterNode: {
            return "(" + _leftExpr->getAsTrueString() + ">" + _rightExpr->getAsTrueString() + ")";
        }
        case greaterOrEqualNode: {
            return "(" + _leftExpr->getAsTrueString() + ">=" + _rightExpr->getAsTrueString() + ")";
        }
        case lesserNode: {
            return "(" + _leftExpr->getAsTrueString() + "<" + _rightExpr->getAsTrueString() + ")";
        }
        case lesserOrEqualNode: {
            return "(" + _leftExpr->getAsTrueString() + "<=" + _rightExpr->getAsTrueString() + ")";
        }
        case equalNode: {
            return "(" + _leftExpr->getAsTrueString() + "==" + _rightExpr->getAsTrueString() + ")";
        }
        case nonEqualNode: {
            return "(" + _leftExpr->getAsTrueString() + "!=" + _rightExpr->getAsTrueString() + ")";
        }
        case andNode: {
            return "(" + _leftExpr->getAsTrueString() + "&&" + _rightExpr->getAsTrueString() + ")";
        }
        case orNode: {
            return "(" + _leftExpr->getAsTrueString() + "||" + _rightExpr->getAsTrueString() + ")";
        }
        case intNode: {
            return _constant;
        }
        case intCastNode: {
            return "int(" + _leftExpr->getAsTrueString() + ")";
        }
        case stringNode: {
            return _constant;
        }
        case stringCastNode: {
            return "string(" + _leftExpr->getAsTrueString() + ")";
        }
        case realNode: {
            return _constant;
        }
        case realCastNode: {
            return "real(" + _leftExpr->getAsTrueString() + ")";
        }
        // identifier
        case identifierNode: {
            if (getAsIdentifier() != nullptr)
                return getAsIdentifier()->getAsTrueString();
            else
                logInternalError("Expression::getAsTrueString used on a nullptr identifier");
        }
        case noneNode: {
            logInternalError("Expression::getAsTrueString used on noneNode");
            return "";
        }
        default: {
            logInternalError("Expression::getAsTrueString reached default");
            return "";
        }
    }
}

Sympression Expression::toSympression(){
    //todo
    //recursively create nodes of corresponding types
    switch(_expressionType){
        // operations
        case addNode: return SymEngine::add(_leftExpr->toSympression(), _rightExpr->toSympression());
        case subtractNode: return SymEngine::sub(_leftExpr->toSympression(), _rightExpr->toSympression());
        case multiplyNode: return SymEngine::mul(_leftExpr->toSympression(), _rightExpr->toSympression());
        case divideNode: return SymEngine::div(_leftExpr->toSympression(), _rightExpr->toSympression());
        //TODO
        //case modulusNode: return SymEngine::mod(_leftExpr->toSympression(), _rightExpr->toSympression());//todo
        case greaterNode: return SymEngine::Lt(_rightExpr->toSympression(), _leftExpr->toSympression());
        case greaterOrEqualNode: return SymEngine::Le(_rightExpr->toSympression(), _leftExpr->toSympression());
        case lesserNode: return SymEngine::Lt(_leftExpr->toSympression(), _rightExpr->toSympression());
        case lesserOrEqualNode: return SymEngine::Le(_leftExpr->toSympression(), _rightExpr->toSympression());
        case equalNode: return SymEngine::Eq(_leftExpr->toSympression(), _rightExpr->toSympression());
        case nonEqualNode: return SymEngine::Ne(_leftExpr->toSympression(), _rightExpr->toSympression());
        //TODO
        //case andNode: return SymEngine::logical_and(_leftExpr->toSympression(), _rightExpr->toSympression());//todo
        //TODO
        //case orNode: return SymEngine::logical_or(_leftExpr->toSympression(), _rightExpr->toSympression());//todo

        // constants
        case intNode: return SymEngine::integer(std::stoi(_constant));
        //case stringNode: return SymEngine::string(_constant, stringNode, nullptr);//todo strings are not supported?
        case realNode: return SymEngine::real_double(std::stod(_constant));

        // identifiers
        case identifierNode: {
            if (_identifier != nullptr) {
                LetName* letName = dynamic_cast<LetName*>(_identifier);
                if (letName != nullptr) {
                    return letName->getReference()->toSympression();
                }

                ImmutableArgName* immutableArgName = dynamic_cast<ImmutableArgName*>(_identifier);
                if (immutableArgName != nullptr) {
                    auto reference = immutableArgName->getReference();
                    if (reference != nullptr) {
                        return reference->toSympression();
                    } else {
                        // main arg, has to be mapped
                        //todo
                    }
                }

                MutableArgName* mutableArgName = dynamic_cast<MutableArgName*>(_identifier);
                if (mutableArgName != nullptr) {
                    return mutableArgName->getReference()->toSympression();
                }

                //todo other identifiers
            } else {
                logInternalError("toSympression got nullptr identifier");
            }
        }

        // casts
        //TODO
        /*case intCastNode: {
            Expression insideExpression = _leftExpr->getAsConstant();
            switch(insideExpression._expressionType){
                case intNode: return insideExpression;
                case realNode: return Expression(std::to_string((int)std::stod(insideExpression._constant)), intNode, nullptr);
                default: return Expression("", noneNode, nullptr);
            }
        }*/

        //TODO
        /*case realCastNode: {
            Expression insideExpression = _leftExpr->getAsConstant();
            switch(insideExpression._expressionType){
                case intNode: {
                    return Expression(std::to_string((double)std::stoi(insideExpression._constant)), realNode, nullptr);
                }
                case realNode: return insideExpression;
                default: return Expression("", noneNode, nullptr);
            }
        }*/

        //todo string cast

        default:
            std::cout << "INTERNAL ERROR: toSympression reached default in switch" << std::endl;
            //todo return something bad
            return SymEngine::acos(_leftExpr->toSympression());//todo!
    }
}

Expression* Expression::simplify(
    const Expression* currentExpression, const Vertex* currentVertex
){
    //todo
    //convert to Exprtk
    //simplify
    //convert back
    //return
    return nullptr;
}

std::pair<Expression*, Expression> Expression::mimplify(
    const Expression* leftExpression, const Vertex* leftVertex, const Expression* rightExpression, const Vertex* rightVertex
){
    //todo
    //convert both to Exprtk
    //simplify mutually
    //convert both back
    //return
    return {nullptr, *leftExpression};
}

ExpressionEquality Expression::equals(
    const Expression* leftExpression, const Vertex* leftVertex, const Expression* rightExpression, const Vertex* rightVertex
){
    //todo
    //convert both to exprtk
    //"equals"?
    //return
    return ExpressionEquality::notEqual;
}

Expression Expression::calculateValue(){
    Expression left = _leftExpr->getAsConstant();
    Expression right = _rightExpr->getAsConstant();

    if (left._expressionType == realNode && right._expressionType == realNode ||
        left._expressionType == intNode && right._expressionType == realNode || 
        left._expressionType == realNode && right._expressionType == intNode){ // result is real

        double l = std::stod(left._constant);
        double r = std::stod(right._constant);
        switch(_expressionType){
            case addNode: return Expression(std::to_string(l + r), realNode, nullptr);
            case subtractNode: return Expression(std::to_string(l - r), realNode, nullptr);
            case multiplyNode: return Expression(std::to_string(l * r), realNode, nullptr);
            case divideNode: return Expression(std::to_string(l / r), realNode, nullptr);
            case modulusNode:
                std::cout << "INTERNAL ERROR: attempt to use modulus with double" << std::endl;
                return Expression("", noneNode, nullptr);
            case greaterNode: return Expression(std::to_string(l > r), intNode, nullptr);
            case greaterOrEqualNode: return Expression(std::to_string(l >= r), intNode, nullptr);
            case lesserNode: return Expression(std::to_string(l < r), intNode, nullptr);
            case lesserOrEqualNode: return Expression(std::to_string(l <= r), intNode, nullptr);
            case equalNode: return Expression(std::to_string(l == r), intNode, nullptr);
            case nonEqualNode: return Expression(std::to_string(l != r), intNode, nullptr);
            case andNode: return Expression(std::to_string(l && r), intNode, nullptr);
            case orNode: return Expression(std::to_string(l || r), intNode, nullptr);

            default:
                std::cout << "INTERNAL ERROR: CalculateValue reached default in switch" << std::endl;
                return Expression("", noneNode, nullptr);
        }
            
    } else if (left._expressionType == intNode && right._expressionType == intNode){ // result is int
        int l = std::stoi(left._constant);
        int r = std::stoi(right._constant);
        switch(_expressionType){
            case addNode: return Expression(std::to_string(l + r), intNode, nullptr);
            case subtractNode: return Expression(std::to_string(l - r), intNode, nullptr);
            case multiplyNode: return Expression(std::to_string(l * r), intNode, nullptr);
            case divideNode: return Expression(std::to_string(l / r), intNode, nullptr);
            case modulusNode: return Expression(std::to_string(l % r), intNode, nullptr);
            case greaterNode: return Expression(std::to_string(l > r), intNode, nullptr);
            case greaterOrEqualNode: return Expression(std::to_string(l >= r), intNode, nullptr);
            case lesserNode: return Expression(std::to_string(l < r), intNode, nullptr);
            case lesserOrEqualNode: return Expression(std::to_string(l <= r), intNode, nullptr);
            case equalNode: return Expression(std::to_string(l == r), intNode, nullptr);
            case nonEqualNode: return Expression(std::to_string(l != r), intNode, nullptr);
            case andNode: return Expression(std::to_string(l && r), intNode, nullptr);
            case orNode: return Expression(std::to_string(l || r), intNode, nullptr);
            
            default:
                std::cout << "INTERNAL ERROR: calculateValue reached default in switch" << std::endl;
                return Expression("", noneNode, nullptr);
        }

    } else {
        std::cout << "INTERNAL ERROR: calculateValue calculation used with unsuitable type:" << std::endl;
        std::cout << "Left: " << left._expressionType << "; right: " << right._expressionType << std::endl;
        return Expression("", noneNode, nullptr);
    }
}

Identifier* Expression::getAsIdentifier(){
    if (_expressionType == identifierNode){
        return _identifier;
    } else {
        return nullptr;
    }
}

// naive implementation: returns noneNode once encounters an identifier
// creates new object and returns a pointer; never return already existing object!
Expression Expression::getAsConstant(){
    switch (_expressionType){

        // binary operations
        case addNode:
        case subtractNode:
        case multiplyNode:
        case divideNode:
        case modulusNode:
        case greaterNode:
        case greaterOrEqualNode:
        case lesserNode:
        case lesserOrEqualNode:
        case equalNode:
        case nonEqualNode:
        case andNode:
        case orNode:
            return calculateValue();
        
        // constants
        case intNode:
            return Expression(_constant, intNode, nullptr);
        case stringNode:
            return Expression(_constant, stringNode, nullptr);
        case realNode:
            return Expression(_constant, realNode, nullptr);

        case identifierNode: {
            // in case of a let or sub, we can try to get value
            //todo redo this using some generic Identifier thing
            if (_identifier != nullptr) {
                LetName* letName = dynamic_cast<LetName*>(_identifier);
                if (letName != nullptr) {
                    return letName->getReference()->getAsConstant();
                }
                ImmutableArgName* immutableArgName = dynamic_cast<ImmutableArgName*>(_identifier);
                if (immutableArgName != nullptr) {
                    auto reference = immutableArgName->getReference();
                    if (reference != nullptr) {
                        return reference->getAsConstant();
                    }
                    std::cout << "INTERNAL ERROR: getAsConstant returned noneNode from immutableArgName" << std::endl;
                    return Expression("", noneNode, nullptr);
                }
                MutableArgName* mutableArgName = dynamic_cast<MutableArgName*>(_identifier);
                if (mutableArgName != nullptr) {
                    return mutableArgName->getReference()->getAsConstant();//todo wip do this much later
                }
                return Expression("", noneNode, nullptr);//todo temporary
            }
            return Expression("", noneNode, nullptr);
        }

        case intCastNode: {
            Expression insideExpression = _leftExpr->getAsConstant();
            switch(insideExpression._expressionType){
                case intNode: return insideExpression;
                case realNode: return Expression(std::to_string((int)std::stod(insideExpression._constant)), intNode, nullptr);
                default: return Expression("", noneNode, nullptr);
            }
        }

        case realCastNode: {
            Expression insideExpression = _leftExpr->getAsConstant();
            switch(insideExpression._expressionType){
                case intNode: {
                    return Expression(std::to_string((double)std::stoi(insideExpression._constant)), realNode, nullptr);
                }
                case realNode: return insideExpression;
                default: return Expression("", noneNode, nullptr);
            }
        }

        // expression unsuitable for being a constant
        default:
            std::cout << "INTERNAL ERROR: getAsConstant returned noneNode (default)" << std::endl;
            return Expression("", noneNode, nullptr);
    }
}

void Expression::markAsUse(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    std::cout << "Expression " << _ASTexpr->to_string() << " is being marked as used" << std::endl;
    switch(_expressionType){

        case addNode:
        case subtractNode:
        case multiplyNode:
        case divideNode:
        case modulusNode:
        case greaterNode:
        case greaterOrEqualNode:
        case lesserNode:
        case lesserOrEqualNode:
        case equalNode:
        case nonEqualNode:
        case andNode:
        case orNode:
            if (_leftExpr != nullptr)
                _leftExpr->markAsUse(currentVertex, indexesExpression);
            if (_rightExpr != nullptr)
                _rightExpr->markAsUse(currentVertex, indexesExpression);
            return;
        
        case identifierNode:
            if (_identifier != nullptr)
                _identifier->markAsUse(currentVertex, indexesExpression);
            return;
        
        case stringNode:
        case intNode:
        case realNode: 
            return;

        case realCastNode: 
        case intCastNode:
        case stringCastNode:
            if (_leftExpr != nullptr)
                _leftExpr->markAsUse(currentVertex, indexesExpression);
            return;
        
        // only noneNode must be left
        default: std::cout << "INTERNAL ERROR: Expression.markAsUse ended as \"default\"" << std::endl;

    }

    return;
}

void Expression::markAsDef(Vertex* currentVertex, const std::vector<Expression*>& indexesExpression){
    std::cout << "Expression " << _ASTexpr->to_string() << " is being marked as defined" << std::endl;
    switch(_expressionType){
        case addNode:
        case subtractNode:
        case multiplyNode:
        case divideNode:
        case modulusNode:
        case greaterNode:
        case greaterOrEqualNode:
        case lesserNode:
        case lesserOrEqualNode:
        case equalNode:
        case nonEqualNode:
        case andNode:
        case orNode:{
            REPORTS.push_back(JsonReporter::createSYN1(
                this->getASTExpr()->to_string(),
                currentVertex,
                nullptr
            ));
            return;
        }
        
        case identifierNode:

            if (_identifier != nullptr)
            switch(_identifier->getClass()){

                case mutableArgNameClass: { //ok
                    MutableArgName* mutableArgName = dynamic_cast<MutableArgName*>(_identifier);
                    mutableArgName->getReference()->markAsDef(currentVertex, indexesExpression);
                    return;
                }
                
                case baseDFNameClass: {//ok
                    BaseDFName* baseDFName = dynamic_cast<BaseDFName*>(_identifier);
                    baseDFName->markAsDef(currentVertex, indexesExpression);
                    return;
                }

                case indexedDFNameClass: {//ok
                    IndexedDFName* indexedDFName = dynamic_cast<IndexedDFName*>(_identifier);
                    indexedDFName->markAsDef(currentVertex, indexesExpression);
                    return;
                }

                case forIteratorNameClass: {//error
                    REPORTS.push_back(JsonReporter::createSYN1(
                        this->getASTExpr()->to_string(),
                        currentVertex,
                        nullptr
                    ));
                    return;
                }

                case whileIteratorNameClass:{ //error
                    REPORTS.push_back(JsonReporter::createSYN1(
                        this->getASTExpr()->to_string(),
                        currentVertex,
                        nullptr
                    ));
                    return;
                }
                
                case valueNameClass://todo
                    return;
                
                case letNameClass: {//ok
                    LetName* letName = dynamic_cast<LetName*>(_identifier);
                    letName->getReference()->markAsDef(currentVertex, indexesExpression);
                    return;
                }

                case immutableArgNameClass: {//error
                    REPORTS.push_back(JsonReporter::createSYN1(
                        this->getASTExpr()->to_string(),
                        currentVertex,
                        nullptr
                    ));
                    return;
                }
                
                default:
                    std::cout << "INTERNAL ERROR (WARNING): Expression.markAsDef() ended by default at identifier node" << std::endl;
                    return;
                
            }
        
        case stringNode:
        case intNode:
        case realNode:
        case intCastNode:
        case realCastNode:
        case stringCastNode:
            REPORTS.push_back(JsonReporter::createSYN1(
                _ASTexpr->to_string(),
                currentVertex,
                nullptr
            ));
            return;

        // only noneNode must be left
        default: 
            std::cout << "INTERNAL ERROR (WARNING): Expression.markAsDef ended as \"default\"" << std::endl;
            return;
    }

    return;
}

bool Expression::isIndexable(){
    // only identifiers are indexable
    if (_expressionType == identifierNode)
        return _identifier->isIndexable();
    else
        return false;
}

TrueIndexedDFData Expression::getTrueBaseNameIndexedDF(TrueIndexedDFData currentIndexedDF){
    if (_expressionType == identifierNode)
        return _identifier->getTrueBaseNameIndexedDF(currentIndexedDF);
    else
        return TrueIndexedDFData(nullptr, {});
}
