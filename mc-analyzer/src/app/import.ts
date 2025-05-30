import {ArgType, ExternArg, ExternNode} from './luna-ast';
import {mapArray} from './utils';

export type TImport = {
    readonly args: readonly ArgType[];
};

export const Import = (externNode: ExternNode): TImport =>
    ({ args: mapArray((arg: ExternArg) => arg.type)(externNode.args) });
