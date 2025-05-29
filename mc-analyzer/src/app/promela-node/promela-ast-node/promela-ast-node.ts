import {TPromelaDf} from '../../df/promela-df';
import {substitute} from '../../utils';

export type TPromelaASTNode = {
    readonly content: string;
};

export const PromelaAstNode = (promelaDfs: TPromelaDf[], template: string): TPromelaASTNode =>
    ({ content: substitute(template)(promelaDfs) });

export const createInitDependsNode = (dfUse: TPromelaDf) =>
                                     (dfInit: TPromelaDf): TPromelaASTNode =>
    PromelaAstNode([dfUse, dfInit], 'bool depends_on_{0}_{1}=false;');

export const getNodeContent = (node: TPromelaASTNode) =>
    node.content;
