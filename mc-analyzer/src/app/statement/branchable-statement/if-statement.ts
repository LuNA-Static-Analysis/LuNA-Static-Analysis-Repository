import {BranchableStatement} from './branchable-statement';
import {IfNode} from '../../luna-ast';
import {parseCondNode} from '../../condition';
import {TContext, TStatement} from '../statement';

export const IfStatement = (ifNode: IfNode, context: TContext): TStatement =>
    BranchableStatement(ifNode, context, (node: IfNode) => parseCondNode(node.cond));
