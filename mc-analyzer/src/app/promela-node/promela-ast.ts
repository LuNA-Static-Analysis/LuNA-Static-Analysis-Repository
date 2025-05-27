import {TPositionTable} from './position-table';
import {getEmptyLtlCode, TLtl} from '../ltl/ltl';
import {LunaAST} from '../luna-ast';
import {MetaInfo, TMetaInfo} from '../meta-info';
import {Context, parseStatement} from '../statement/statement';
import {EmptyContextInfo} from '../df/context-info';
import {addProctype, getPositionTable, getPromelaCode} from './node-creator';

export type TPromelaAST = {
    readonly promelaCode: string;
    readonly positionTable: TPositionTable;
    readonly ltls: readonly TLtl[];
    readonly lunaSourceLines: readonly string[];
};



export const PromelaAST = (lunaSourceLines: readonly string[], ast: LunaAST): TPromelaAST => {
    const metaInfo: TMetaInfo = MetaInfo(ast);
    const mainStatement = parseStatement(metaInfo.mainNode)(Context(EmptyContextInfo(), MetaInfo(ast)));
    const nodeCreator = addProctype(mainStatement.context.metaInfo.nodeCreator)(mainStatement.promelaNodes)(metaInfo.mainNode.code);
    console.error(getPositionTable(nodeCreator));
    return {
        promelaCode: getPromelaCode(nodeCreator),
        positionTable: getPositionTable(nodeCreator),
        ltls: nodeCreator.ltls,
        lunaSourceLines
    };
};
