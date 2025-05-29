import {hasImport, hasSub, TMetaInfo} from '../meta-info';
import {TContextInfo} from '../df/context-info';
import {TProctypeBodyNode} from '../promela-node/proctype-body-node/proctype-body-node';
import {IfStatement} from './branchable-statement/if-statement';
import {WhileStatement} from './branchable-statement/while-statement';
import {ForStatement} from './branchable-statement/for-statement';
import {ExecImportStatement} from './exec-statement/exec-import-statement';
import {ExecSubStatement} from './exec-statement/exec-sub-statement';

export type TContext = {
    readonly childInfo: TContextInfo;
    readonly metaInfo: TMetaInfo;
}

export const Context = (childInfo: TContextInfo, metaInfo: TMetaInfo): TContext =>
    ({ childInfo , metaInfo });

export type TStatement = {
    readonly context: TContext;
    readonly promelaNodes: readonly TProctypeBodyNode[];
};

export const Statement = (context: TContext, promelaNodes: readonly TProctypeBodyNode[]): TStatement =>
    ({ context: context, promelaNodes });

export const parseStatement = (node: any) =>
                              (context: TContext): TStatement => {
    const metaInfo = context.metaInfo;
    return ({
        'if': () => IfStatement(node, context),
        'while': () => WhileStatement(node, context),
        'for': () => ForStatement(node, context),
        'exec': () => hasImport(metaInfo)(node.code)
            ? ExecImportStatement(node, context)
            : hasSub(metaInfo)(node.code)
                ? ExecSubStatement(node, context)
                : undefined
    })[node.type]()
};
