import {BranchableStatement} from './branchable-statement';
import {WhileNode} from '../../luna-ast';
import {TMetaInfo} from '../../meta-info';
import {parseCondNode} from '../../condition';
import {TContextInfo} from '../../df/context-info';
import {TContext, TStatement} from '../statement';

export const WhileStatement = (whileNode: WhileNode, context: TContext): TStatement =>
    BranchableStatement(whileNode, context, (node: WhileNode) => parseCondNode(node.cond));
