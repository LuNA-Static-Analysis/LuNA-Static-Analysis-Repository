#include "expr.hpp"

ExpressionType Expression::getType(){
    return noneNode;
}

expr* Expression::getExpr(){
    return this->ASTexpr;
}

Expression::Expression(expr* ASTexpr){
    this->ASTexpr = ASTexpr;
    this->constant = "";
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;
    this->type = noneNode;
}

Expression::Expression(expr* ASTexpr, std::map<std::string, Identifier*> nameTable, std::vector<std::string>* errorReports){
    this->ASTexpr = ASTexpr;

    // default initialization
    this->constant = "";
    this->identifier = nullptr;
    this->leftExpr = nullptr;
    this->rightExpr = nullptr;

    luna_string* lunaString = dynamic_cast<luna_string*>(ASTexpr);
    if (lunaString != NULL){
        this->type = stringNode;
        this->constant = lunaString->to_string();
        return;
    }

    integer* lunaInteger = dynamic_cast<integer*>(ASTexpr);
    if (lunaInteger != NULL) {
        this->type = intNode;
        this->constant = lunaInteger->to_string();
        return;
    }

    real* lunaReal = dynamic_cast<real*>(ASTexpr);
    if (lunaReal != NULL) {
        this->type = realNode;
        this->constant = lunaReal->to_string();
        return;
    }

    luna_cast* lunaCast = dynamic_cast<luna_cast*>(ASTexpr);
    if (lunaCast != NULL){
        this->type = castNode;//todo add type to which we are casting
        this->leftExpr = new Expression(lunaCast->expr_, nameTable, errorReports);
        return;
    }

    bin_op* lunaBinOp = dynamic_cast<bin_op*>(ASTexpr);
    if (lunaBinOp != NULL){

        // deduction of binary operation type
        std::string op = (lunaBinOp->to_string()).substr(lunaBinOp->left_->to_string().size(), 
            lunaBinOp->to_string().size() - lunaBinOp->left_->to_string().size() - lunaBinOp->right_->to_string().size());
        while (op[0] != ' '){
            op = op.substr(1, op.size() - 1);
        }
        switch(op[0]){
            case '+':  this->type = addNode; break;
            case '-': this->type = subtractNode; break;
            case '*': this->type = multiplyNode; break;
            case '/': this->type = divideNode; break;
            default: std::cout << "INTERNAL ERROR: UNKNOWN OPERATION IN EXPRESSION CONSTRUCTOR AT LINE " << ASTexpr->line_ << std::endl;
        }

        this->leftExpr = new Expression(lunaBinOp->left_, nameTable, errorReports);
        this->rightExpr = new Expression(lunaBinOp->right_, nameTable, errorReports);
        return;
    }

    // name found
    id* df = dynamic_cast<id*>(ASTexpr);
    if (df != NULL){

        this->type = identifierNode;
        //this->identifier;

        simple_id* simpleDF = dynamic_cast<simple_id*>(ASTexpr);
        // this also has for iterators, and not only them perhaps TODO
        if (simpleDF != NULL){
            std::string simpleDFName = *(simpleDF->value_->value_);
            auto base = nameTable.find(simpleDFName);
            if (base != nameTable.end()){

                switch(base->second->getType()){
                    case baseDFName://todo also maybe subargname (but not mainargname)
                        this->identifier = new IndexedDFName(simpleDFName, base->second, {}, ASTexpr->line_);
                        break;
                    default:
                        //todo what types?
                        break;
                }
            } else {
                std::string report = "ERROR: no name \"" + simpleDFName + "\" found!" + "\n";
                errorReports->push_back(report);
            }

            return;
        }

        complex_id* complexDF = dynamic_cast<complex_id*>(ASTexpr);
        if (complexDF != NULL){
            // find out how many indices there are
            //TODO redo this with one function that
            // parses indexed DFs and creates IndexedDFName with Expression objects as indices

            IndexedDFName* temp = parseIndexedDFExpression(complexDF, nameTable, ASTexpr->line_, errorReports);
            if (temp != nullptr){
                //result.insert(temp);
            }

            return;
        }

        std::string report = "ERROR: at dynamic_cast to id at line " + std::to_string(ASTexpr->line_) + "\n";
        errorReports->push_back(report);
    }

    std::cout << "> getNamesFromExpression finished with errors\n\n";
    return;
}

void Expression::markAsUse(){//todo maybe return string vector of reports?

}

void Expression::markAsDef(){

}
