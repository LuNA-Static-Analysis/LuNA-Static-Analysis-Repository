import {TBiOperatorCond, TCondNode, TernaryOperatorCond} from './luna-ast';
import {LunaDf, TLunaDf} from './df/luna-df';
import {flatMapArray, join, mapArray} from './utils';
import {Expr, LunaDfIndex, TLunaDfIndex} from "./df/luna-df-index";
import {TContextInfo} from "./df/context-info";

export enum BranchCondition { FALSE, TRUE, IDK }

export type TCondition = {
    readonly value: number | undefined;
    readonly  formula: string;
    readonly prettyFormula: string;
    readonly branchCondition: BranchCondition;
    readonly lunaDfs: readonly TLunaDf[];
};

export const parseCondNode = (condition: TCondNode): TCondition => {
    if (condition.type in binary) {
        return parseBiCond(condition as TBiOperatorCond)(binary[condition.type]);
    }
    if (condition.type === '?') {
        return parseTernaryCond(condition);
    }
    if (condition.type === 'iconst' || condition.type === 'rconst') {
        return Condition(condition.value, condition, BranchCondition.TRUE, []);
    }
    if (condition.type === 'id') {
        return Condition(undefined, condition, BranchCondition.IDK, [LunaDf(condition)]);
    }
    if (condition.type as any === 'icast' || condition.type as any === 'rcast') {
        return parseCondNode((condition as any).expr);
    }
    return Condition(undefined, condition, BranchCondition.IDK, []);
};

export const getBaseLunaDf = (cond: TCondition) =>
    cond.lunaDfs[0];

const binary = {
    '+': (a: number, b: number) => a + b,
    '-': (a: number, b: number) => a - b,
    '*': (a: number, b: number) => a * b,
    '/': (a: number, b: number) => (a - a % b) / b,
    '%': (a: number, b: number) => a % b,
    '<': (a: number, b: number) => a < b ? 1 : 0,
    '<=':(a: number, b: number) => a <= b ? 1 : 0,
    '>': (a: number, b: number) => a > b ? 1 : 0,
    '>=': (a: number, b: number) => a >= b ? 1 : 0,
    '==': (a: number, b: number) => a === b ? 1 : 0,
    '!=': (a: number, b: number) => a !== b ? 1 : 0,
    '&&': (a: number, b: number) => a && b ? 1 : 0,
    '||': (a: number, b: number) => a || b ? 1 : 0
};

const Condition = (value: number | undefined,
                   condNode: TCondNode,
                   branchCondition: BranchCondition = BranchCondition.IDK,
                   lunaDfs: readonly TLunaDf[]): TCondition =>
    ({ value, branchCondition, lunaDfs, formula: `${Expr(condNode)}`, prettyFormula: `${LunaDfIndex(condNode).name}` });

const parseBiCond = (operator: TBiOperatorCond) =>
                    (func: (a: number, b: number) => number): TCondition => {
    const operands = mapArray(parseCondNode)(operator.operands);
    const lunaDfs: readonly TLunaDf[] = flatMapArray((op: TCondition) => op.lunaDfs)(operands);
    const isComputable = operands.some(op => op.value === undefined);
    if (isComputable) {
        return Condition(undefined, operator, BranchCondition.IDK, lunaDfs);
    }
    const value = func(operands[0].value, operands[1].value);
    const branchCondition = value === 0 ? BranchCondition.FALSE : BranchCondition.TRUE;
    return Condition(value, operator, branchCondition, lunaDfs);
};

const parseTernaryCond = (operator: TernaryOperatorCond): TCondition => {
    const operands = mapArray(parseCondNode)(operator.operands);
    const lunaDfs: readonly TLunaDf[] = flatMapArray((op: TCondition) => op.lunaDfs)(operands);
    const [cond, trueBranch, falseBranch] = operands;
    if (cond.branchCondition === BranchCondition.IDK) {
        return Condition(undefined, operator, BranchCondition.IDK, lunaDfs);
    }
    const branchCondition = cond.value === 0 ? BranchCondition.FALSE : BranchCondition.TRUE;
    const branch = branchCondition === BranchCondition.TRUE ? trueBranch : falseBranch;
    return Condition(branch.value, operator, branch.branchCondition, lunaDfs);
};

export const combineCondition = (
    condition: TCondition,
    contextInfo: TContextInfo
): TCondition => {
    const updatedFormula = condition.formula.replace(/\b[a-zA-Z_]\w*\b/g, (token) => {
        const replacement = contextInfo.constants.get(token);
        return replacement ? `(${replacement.formula})` : token;
    });

    const updatedPrettyFormula = condition.prettyFormula.replace(/\b[a-zA-Z_]\w*\b/g, (token) => {
        const replacement = contextInfo.constants.get(token);
        return replacement ? `(${replacement.prettyFormula})` : token;
    });
    const aliasDfs = Array.from(contextInfo.constants.values()).flatMap(c => c.lunaDfs);
    const allLunaDfs = [...condition.lunaDfs, ...aliasDfs];

    return {
        ...condition,
        formula: updatedFormula,
        prettyFormula: updatedPrettyFormula,
        lunaDfs: allLunaDfs
    };
};

