import {LunaDf, TLunaDf} from './df/luna-df';
import {mapArray} from './utils';
import {execSync} from 'child_process';
import {mkdtempSync, readFileSync, rmSync, writeFileSync} from 'fs';
import path from 'path';
import {tmpdir} from 'os';

export type ArgType = 'real' | 'int' | 'name';
export type LiteralType = 'iconst' | 'rconst';
export type BiOperator = '+' | '-' | '*' | '/' | '%' | '<' | '<=' | '>' | '>=' | '==' | '!=' | '&&' | '||';
export type TernaryOperator = '?';
export type LiteralCond = { readonly type: LiteralType, readonly value: number };
export type BeginContained = { readonly begin: number };
export type BodyContained = BeginContained & { readonly body: BodyNode };
export type ExternArg = { readonly type: ArgType };
export type StructArg = { readonly type: ArgType, readonly id: string };
export type ExternNode = { readonly type: 'extern', readonly args: readonly ExternArg[] };
export type StructNode = BeginContained & BodyContained & { readonly type: 'struct', readonly args: readonly StructArg[] };
export type ASTNode = ExternNode | StructNode;
export type LunaAST = { [alias: string]: ASTNode };
export type IfNode = BodyContained & { readonly type: 'if', readonly cond: TCondNode };
export type WhileNode = BodyContained & { readonly type: 'while', readonly cond: TCondNode, readonly start: TCondNode, readonly wout: TDfCond };
export type ForNode = BodyContained & { readonly type: 'for', readonly var: string, readonly first: TCondNode, readonly last: TCondNode };
export type ExecNode = BeginContained & { readonly type: 'exec', readonly code: string, readonly args: readonly TCondNode[], readonly rules: readonly RuleNode[] };
export type BranchableNode = (IfNode | WhileNode | ForNode);
export type StatementNode = (BranchableNode | ExecNode);
export type BodyNode = readonly [DfNode, ...StatementNode[]];
export type TBiOperatorCond = { readonly type: BiOperator, readonly operands: readonly [TCondNode, TCondNode] };
export type TernaryOperatorCond = { readonly type: TernaryOperator, readonly operands: readonly [TCondNode, TCondNode, TCondNode] };
export type TCondNode = TDfCond | LiteralCond | TBiOperatorCond | TernaryOperatorCond;
export type RuleNode = BeginContained & { readonly ruletype: string, readonly type: string, readonly property : string, readonly items : readonly [string, ...TCondNode[]][] };
export type TDfCond = { readonly type: 'id'; readonly ref: readonly [string, ...readonly TCondNode[]]; readonly begin: number; };
export type DfNode = { readonly names: string[]; readonly begin: number; };
export type NestedStringArray = string | readonly NestedStringArray[];

export const DfCond = (ref: readonly [string, ...TCondNode[]], begin: number): TDfCond =>
    ({ type: 'id', ref, begin });


export const CondNode = (begin: number) => (value: string | readonly [string, ...readonly NestedStringArray[]]): TCondNode => {
    const intRegExp = /^\d*$/;
    const floatRegExp = /^\d*\.\d*$/;
    const operatorRegExp = /[=+\-*/<%>!&|?:]/;
    if (typeof value !== 'string') {
        return {
            type: 'id',
            ref: [value[0], ...value.slice(1).map(CondNode(begin))],
            begin: begin
        };
    }
    if (operatorRegExp.exec(value)) {
        const workDir = mkdtempSync(path.join(tmpdir(), 'TEMP_DIR_PREFIX_'));
        try {
            const workFile = path.join(workDir, 'main.fa');
            const jsonFile = path.join(workDir, 'a.json');
            writeFileSync(workFile, `sub main() { foo(${value}); }`);
            execSync(`parser -o ${jsonFile} ${workFile}`);
            return JSON.parse(readFileSync(jsonFile, 'utf8'))['main'].body[1].args[0];
        } finally {
            rmSync(workDir, {recursive: true, force: true});
        }
    }
    if (intRegExp.exec(value[0])) {
        return {type: 'iconst', value: Number.parseInt(value, 10)};
    }
    if (floatRegExp.exec(value[0])) {
        return {type: 'rconst', value: Number.parseFloat(value)};
    }
    return undefined;
};

export const getNames = (dfNode: DfNode): readonly TLunaDf[] =>
    mapArray((name: string) => LunaDf((DfCond([name], dfNode.begin))))(dfNode.names);

export const BiOperatorCond = (type: BiOperator, operands: readonly [TCondNode, TCondNode]): TBiOperatorCond =>
    ({ type, operands });
