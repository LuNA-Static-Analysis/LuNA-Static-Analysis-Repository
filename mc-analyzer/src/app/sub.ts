import {ArgType, BodyNode, StructArg, StructNode} from './luna-ast';
import {mapArray} from './utils';

export type TSub = {
    readonly args: Map<string, ArgType>;
    readonly bodyNode: BodyNode;
    readonly begin: number;
    readonly get: (index: number) => string;
};

export const Sub = (structNode: StructNode): TSub => {
    const args = new Map(mapArray((arg: StructArg): [string, ArgType] => [arg.id, arg.type])(structNode.args));
    const argsKeys: string[] = Array.from(args.keys());
    return {
        args,
        bodyNode: structNode.body,
        begin: structNode.begin,
        get: (index: number): string => argsKeys[index]
    };
};
