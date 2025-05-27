import {TPromelaDf} from '../../df/promela-df';
import {getEmptyLtlCode, TLtl, ltlsToPromelaDfs} from '../../ltl/ltl';
import {substitute} from '../../utils';
import {TPromelaASTNode} from '../promela-ast-node/promela-ast-node';
import {filter} from "fp-ts/Array";

export type TProctypeBodyNode = TPromelaASTNode & {
    readonly promelaDfs: readonly TPromelaDf[];
    readonly begins: readonly number[]
    readonly ltls: readonly TLtl[];
};

export type DependsNode = TProctypeBodyNode & {
    readonly initDependsNodes: TPromelaASTNode;
};

export const ProctypeBodyNode = (template: string, begins: number[], ltls: readonly TLtl[]): TProctypeBodyNode => {
    const promelaDfs = ltlsToPromelaDfs(ltls);
    return ({
            content: substitute(template)(promelaDfs),
            ltls: filter((ltl: TLtl) => getEmptyLtlCode() !== ltl.code)([...ltls]),
            promelaDfs,
            begins
    });
};

export const equalsDependsNodeContent = (dependsNode: DependsNode) =>
                                        (node: TPromelaASTNode): boolean =>
    node.content === dependsNode.initDependsNodes.content;

export const extractPromelaDfsFromNode = (node: TProctypeBodyNode) =>
    node.promelaDfs;

export const getNodeBegins = (node: TProctypeBodyNode): readonly number[] =>
    node.begins;
