import {TStatement, Statement} from '../statement';
import {TCondition, parseCondNode} from '../../condition';
import {ExecNode} from '../../luna-ast';
import {TMetaInfo} from '../../meta-info';
import {TContextInfo} from '../../df/context-info';
import {TProctypeBodyNode} from '../../promela-node/proctype-body-node/proctype-body-node';
import {mapArray} from '../../utils';

export type TExecStatement = TStatement & {
    readonly args: readonly TCondition[];
};

export const getArgs = (execNode: ExecNode) =>
    mapArray(parseCondNode)(execNode.args);

export const ExecStatement = (statement: TStatement, execNode: ExecNode) =>
    ({
        ...statement,
        args: getArgs(execNode)
    });
