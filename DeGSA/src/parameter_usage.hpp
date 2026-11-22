#pragma once

#include "../../parser/ast.hpp"
#include <algorithm>

// Look for references to a declaration within its lexical scope.
class NameUsage {
    const std::string& name;

    bool references(const expr* expression) const {
        if (auto id = dynamic_cast<const simple_id*>(expression))
            return id->value_->to_string() == name;
        if (auto indexed = dynamic_cast<const complex_id*>(expression))
            return references(indexed->id_) || references(indexed->expr_);
        if (auto binary = dynamic_cast<const bin_op*>(expression))
            return references(binary->left_) || references(binary->right_);
        if (auto cast = dynamic_cast<const luna_cast*>(expression))
            return references(cast->expr_);
        return false;
    }

    bool references(const opt_exprs* args) const {
        if (!args || !args->exprs_seq_) return false;
        for (auto arg : *args->exprs_seq_->expr_)
            if (references(arg)) return true;
        return false;
    }

    bool contains(const name_seq* names) const {
        return std::any_of(names->names_->begin(), names->names_->end(),
            [&](const luna_string* entry) { return entry->to_string() == name; });
    }

    bool references(const opt_behavior* behavior) const {
        if (!behavior || !behavior->seq_) return false;
        for (auto pragma : *behavior->seq_->behv_pragma_) {
            if (auto eq = dynamic_cast<behv_pragma_eq*>(pragma)) {
                if (references(eq->id_) || references(eq->expr_)) return true;
            } else if (auto eqg = dynamic_cast<behv_pragma_eqg*>(pragma)) {
                if (references(eqg->id_) || references(eqg->expr_)) return true;
            } else if (auto ids = dynamic_cast<behv_pragma_id_seq*>(pragma)) {
                for (auto id : *ids->id_seq_->seq_)
                    if (references(id)) return true;
            } else if (auto value = dynamic_cast<behv_pragma_expr*>(pragma)) {
                if (references(value->expr_)) return true;
            } else if (auto names = dynamic_cast<behv_pragma_seq*>(pragma)) {
                if (contains(names->name_seq_)) return true;
            }
        }
        return false;
    }

    static bool hasPlacementExpressions(const control_pragma* placement) {
        // The parser stores the address of a temporary vector for these clauses.
        // Its operands cannot be inspected reliably, so usage is inconclusive.
        return placement && placement->expr_;
    }

    bool references(const statement* stmt) const {
        if (auto call = dynamic_cast<const cf_statement*>(stmt)) {
            return references(call->opt_exprs_) || references(call->opt_label_->id_)
                || references(call->opt_rules_->opt_exprs_) || references(call->opt_setdf_rules_->opt_exprs_)
                || references(call->opt_behavior_);
        }
        if (auto branch = dynamic_cast<const if_statement*>(stmt))
            return references(branch->expr_) || references(branch->block_);
        if (auto loop = dynamic_cast<const for_statement*>(stmt))
            return hasPlacementExpressions(loop->control_pragma_) || references(loop->expr_1_) || references(loop->expr_2_)
                || (loop->name_->to_string() != name && references(loop->block_));
        if (auto loop = dynamic_cast<const while_statement*>(stmt))
            return hasPlacementExpressions(loop->control_pragma_) || references(loop->right_) || references(loop->id_)
                || (loop->left_->to_string() != name && (references(loop->expr_) || references(loop->block_)));
        if (auto let = dynamic_cast<const let_statement*>(stmt)) {
            bool shadowed = false;
            for (auto assignment : *let->assign_seq_->assign_seq_) {
                if (references(assignment->expr_)) return true;
                if (assignment->name_->to_string() == name) shadowed = true;
            }
            return !shadowed && references(let->block_);
        }
        return false;
    }

    bool references(const block* body) const {
        auto locals = body->opt_dfdecls_->dfdecls_;
        if (locals && contains(locals->name_seq_)) return false;
        for (auto stmt : *body->statement_seq_->statements_)
            if (references(stmt)) return true;
        return references(body->opt_behavior_);
    }

public:
    explicit NameUsage(const std::string& identifierName) : name(identifierName) {}

    bool referencedIn(const for_statement& loop) const {
        return hasPlacementExpressions(loop.control_pragma_) || references(loop.block_);
    }

    bool referencedIn(const while_statement& loop) const {
        return hasPlacementExpressions(loop.control_pragma_) || references(loop.expr_) || references(loop.block_);
    }

    bool referencedIn(const luna_sub_def& sub) const {
        return hasPlacementExpressions(sub.control_pragma_) || references(sub.block_);
    }
};
