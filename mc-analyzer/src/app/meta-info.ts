import {ExecNode, ExternNode, LunaAST, StructNode} from './luna-ast';
import {NodeCreator, TNodeCreator} from './promela-node/node-creator';
import {Import, TImport} from './import';
import {Sub, TSub} from './sub';
import {filter, mapArray} from './utils';
import {ExecImportStatement} from "./statement/exec-statement/exec-import-statement";

export type TMetaInfo = {
    readonly imports: Map<string, TImport>;
    readonly subs: Map<string, TSub>;
    readonly nodeCreator: TNodeCreator;
    readonly mainNode: ExecNode;
};

export const MetaInfo = (ast: LunaAST): TMetaInfo => {
    const mainStructName = 'main';
    const keys = Object.keys(ast);
    const subs = new Map<string, TSub>(
        mapArray((alias: string): [string, TSub] => [alias, Sub(ast[alias] as StructNode)])(
            filter((alias: string) => ast[alias].type === 'struct')(keys)
        )
    );
    const imports = new Map(
        mapArray((alias: string): [string, TImport] => [alias, Import(ast[alias] as ExternNode)])(
            filter((alias: string) => ast[alias].type === 'extern' || ast[alias].type === 'foreign_cpp')(keys)
        ));
    return {
        imports,
        subs,
        nodeCreator: NodeCreator(mainStructName),
        mainNode: {
            type: 'exec',
            begin: getSubBegin(subs)(mainStructName),
            code: mainStructName,
            args: [],
            rules: []
        }
    };
};

export const updateNodeCreator = (metaInfo: TMetaInfo) =>
    (nodeCreator: TNodeCreator): TMetaInfo =>
        ({ ...metaInfo, nodeCreator });

export const hasImport = (metaInfo: TMetaInfo) =>
                         (name: string): boolean =>
    metaInfo.imports.has(name);

export const hasSub = (metaInfo: TMetaInfo) =>
                      (name: string): boolean =>
    metaInfo.subs.has(name);

export const getImport = (metaInfo: TMetaInfo) =>
                        (name: string): TImport =>
    metaInfo.imports.get(name);

export const getSub = (metaInfo: TMetaInfo) =>
                      (name: string): TSub =>
    metaInfo.subs.get(name);

const getSubBegin = (subs: Map<string, TSub>) =>
                 (name: string) =>
    subs.get(name).begin;
