import {Context, Statement, TStatement, TContext} from '../statement';
import {BranchableNode} from '../../luna-ast';
import {TMetaInfo, updateNodeCreator} from '../../meta-info';
import {BranchCondition, combineCondition, TCondition} from '../../condition';
import {BodyStatement} from '../body-statement';
import {TContextInfo, isConstant, isNotConstant, updateRegistry} from '../../df/context-info';
import {
    combineLtls, FormulaStatement, IfNode, UseNodeState, TFormulaStatement, getFormulaName, processNodes
} from '../../promela-node/node-creator';
import {LtlSem5, LtlSem6} from '../../ltl/ltl';
import { pipe } from 'fp-ts/lib/function';
import * as E from 'fp-ts/Either';
import * as math from 'mathjs';

export const BranchableStatement = (node: BranchableNode, context: TContext, getCondition: (branchableNode: BranchableNode) => TCondition): TStatement => {
    const condition = getCondition(node);
    return getStatementByBranchCondition(condition.branchCondition)
        (() => Statement(
            Context(
                context.childInfo,
                withLtlSem5(context)(FormulaStatement(condition.formula, condition.prettyFormula, '0'))
            ),
            []
        ))
        (() => {
            const statement = BodyStatement(node.body, context);
            const contextInfo = updateRegistry(context.childInfo)(statement.context.childInfo);
            return Statement(
                Context(
                    contextInfo,
                    withLtlSem5(
                        statement.context
                    )(
                        FormulaStatement(condition.formula, condition.prettyFormula, '1')
                    )
                ),
                statement.promelaNodes
            )
        })
        (() => handleIDK(context)(node)(getCondition));
};

export const handleIDK = (context: TContext) =>
                         (node: BranchableNode) =>
                         (getCondition: (branchableNode: BranchableNode) => TCondition): TStatement => {
    const metaInfo = context.metaInfo;
    const childInfo = context.childInfo;
    const condition = getCondition(node);
    if (!metaInfo.nodeCreator.formulas.has(condition.prettyFormula)) {
        metaInfo.nodeCreator.formulas.set(condition.prettyFormula, getFormulaName());
    }
    const formula = FormulaStatement(condition.formula, condition.prettyFormula, metaInfo.nodeCreator.formulas.get(condition.prettyFormula));
    const iteratorStatement = processNodes(node)(UseNodeState)(condition.lunaDfs.filter(isNotConstant(childInfo)))(context);
    const bodyStatement = BodyStatement(node.body, iteratorStatement.context);
    const promelaBodyNodes = bodyStatement.promelaNodes;
    const func = () => Statement(
        Context(iteratorStatement.context.childInfo, withLtlSem5(bodyStatement.context)(formula)),
        iteratorStatement.promelaNodes
    );
    if (promelaBodyNodes.length > 0 || condition.lunaDfs.find(isConstant(childInfo))) {
        const m2 = withLtlSem5(bodyStatement.context)(formula);
        const m3 = withLtlSem6(Context(bodyStatement.context.childInfo, m2))(formula);
        return Statement(Context(iteratorStatement.context.childInfo, m3),
            [...iteratorStatement.promelaNodes, IfNode(promelaBodyNodes, iteratorStatement.context.childInfo, formula)]
        );
    }
    return func();
};

export const getStatementByBranchCondition = (branchCondition: BranchCondition) =>
                                             (f1: () => TStatement) =>
                                             (f2: () => TStatement) =>
                                             (f3: () => TStatement): TStatement => {
    switch (branchCondition) {
        case BranchCondition.FALSE: return f1();
        case BranchCondition.TRUE: return f2();
        case BranchCondition.IDK: return f3();
        default:
            throw new Error('Unreadable branch condition');
    }
};

const replaceLogicalOperators = (expr: string): string => {
    expr = expr.replace(/\b[\w\[\]\.]+\b/g, (match) =>
        match
    )
        .replace(/\|\|/g, ' or ')
        .replace(/\&\&/g, ' and ')
    return expr;
};


export const getCond = (formula: TFormulaStatement, constants: Map<string, TCondition> = new Map()) => {
    let variables = {};
    for (const [key, value] of constants) {
        variables = {
            ...variables,
            [key]: value.formula
        };
    }
    const rules = [
        ...math.simplify.rules,
        'n1 < n2 or n1 >= n2 -> 1',
        'n1 > n2 or n1 <= n2 -> 1',

        'n1 < n2 and n1 >= n2 -> 0',
        'n1 > n2 and n1 <= n2 -> 0',

        'n1 == n2 -> n2 == n1',
        'n1 != n2 -> n2 != n1',

        'n1 == n1 -> 1',
        'n1 != n1 -> 0',

        'n1 > n2 -> n2 < n1',
        'n1 >= n2 -> n2 <= n1',

        'n1 <= n2 -> n1 < n2 or n1 == n2',

        'n1 < n2 -> n1 - n2 < 0',
        'n1 < n2 or n2 < n1 -> n1 != n2',

        'n1 == n2 or n1 != n2 -> 1',
        'n1 == n2 or n2 != n1 -> 1',

        'n1 == n1 + 1 -> 0',

        'n1 == n2 and n1 != n2 -> 0',
        'n1 == n2 and n2 != n1 -> 0'
    ];
    return pipe(
        E.tryCatch(
            () => {
                const f1 = replaceLogicalOperators(formula.lunaFormula.trim());
                const f2 = math.simplify(f1, variables).toString().replace(/"/g, '');
                const ast = math.parse(f2);
                const simplified = math.simplify(ast, rules).toString();
                return {'1': 'true', '0': 'false'}[simplified] ?? formula.promelaFormula;
            },
            () => formula.promelaFormula
        ),
        E.getOrElse(() => formula.promelaFormula)
    );
};

const withLtlSem5 = (context: TContext) => (formula: TFormulaStatement): TMetaInfo => {
    const metaInfo = context.metaInfo;
    const contextInfo = context.childInfo;
    const cond = getCond(formula, contextInfo.constants);

    let variables = {};
    for (const [key, value] of contextInfo.constants) {
        variables = {
            ...variables,
            [key]: value.formula
        };
    }
    const f1 = replaceLogicalOperators(formula.lunaFormula.trim());
    if (cond === 'true' || cond === 'false') {
        if (math.simplify(f1).toString().replace(/"/g, '').toString() !== math.simplify(f1, variables).toString().replace(/"/g, '')) {
            return metaInfo;
        }
        const ltlSem5 = {
            ...LtlSem5(formula.promelaFormula, cond),
            bool: cond as 'true' | 'false',
            condition: formula.prettyFormula
        };
        return updateNodeCreator(
            metaInfo
        )(
            {
                ...metaInfo.nodeCreator,
                ltls: combineLtls(metaInfo.nodeCreator)([ltlSem5])
            }
        );
    }
    return metaInfo;
};

const withLtlSem6 = (context: TContext) => (formula: TFormulaStatement) => {
    const metaInfo = context.metaInfo;
    const contextInfo = context.childInfo;
    const cond = getCond(formula, contextInfo.constants);

    let variables = {};
    for (const [key, value] of contextInfo.constants) {
        variables = {
            ...variables,
            [key]: value.formula
        };
    }
    const f1 = replaceLogicalOperators(formula.lunaFormula.trim());
    if (cond === 'true' || cond === 'false') {
        if (math.simplify(f1).toString().replace(/"/g, '').toString() === math.simplify(f1, variables).toString().replace(/"/g, '')) {
            return metaInfo;
        }
        const ltlSem6 = {
            ...LtlSem6(formula.promelaFormula, cond),
            bool: cond as 'true' | 'false',
            condition: formula.prettyFormula
        };


        return updateNodeCreator(
            metaInfo
        )(
            {
                ...metaInfo.nodeCreator,
                ltls: combineLtls(metaInfo.nodeCreator)([ltlSem6])
            }
        );
    }
    return metaInfo;
};
