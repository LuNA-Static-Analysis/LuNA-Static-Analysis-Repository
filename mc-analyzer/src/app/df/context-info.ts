import {PromelaDf, getPromelaDfLunaName, TPromelaDf} from './promela-df';
import {LunaDf, TLunaDf} from './luna-df';
import {filter, mapArray} from '../utils';
import {TCondNode, CondNode, TDfCond, NestedStringArray} from '../luna-ast';
import {TCondition} from "../condition";

export type TContextInfo = {
    readonly dfRegistry: Map<string, TPromelaDf>;
    readonly aliases: Map<string, string>;
    readonly constants: Map<string, TCondition>;
};

export const ContextInfo = (dfRegistry: Map<string, TPromelaDf>,
                            aliases: Map<string, string>,
                            constants: Map<string, TCondition>): TContextInfo =>
    ({
        dfRegistry: new Map<string, TPromelaDf>(dfRegistry),
        aliases: new Map<string, string>(aliases),
        constants: new Map<string, TCondition>(constants)
    });

export const EmptyContextInfo = () =>
    ContextInfo(null, null, null);

export const getCanonicalLunaDf = (contextInfo: TContextInfo) =>
                                  (lunaDf: TLunaDf): TLunaDf => {
    const newName = lunaDf.fullName; //replaceChars(lunaDf)(contextInfo.aliases);

    // console.error({
    //     // contextInfo,
    //     // aliases: JSON.stringify(contextInfo.aliases),
    //     lunaDf,
    //     caninical: contextInfo.dfRegistry.get(contextInfo.aliases.get(lunaDf.fullName))?.lunaDf ||
    //         contextInfo.dfRegistry.get(newName)?.lunaDf ||
    //         LunaDf(CondNode(parseIndices(newName)) as TDfCond)
    // });
    return contextInfo.dfRegistry.get(contextInfo.aliases.get(lunaDf.fullName))?.lunaDf ||
        contextInfo.dfRegistry.get(newName)?.lunaDf ||
        LunaDf(CondNode(lunaDf.begin)(parseIndices(newName)) as TDfCond);
};

export const updateRegistry = (contextInfo: TContextInfo) =>
                              (parentInfo: TContextInfo): TContextInfo => {
    const dfsInThisRegistry = mapArray(getPromelaDfLunaName)([...contextInfo.dfRegistry.values()]);
    return addEntryDfs(parentInfo)(
        filter(
            (promelaDf: TPromelaDf) => dfsInThisRegistry.includes(promelaDf.lunaDf.lunaName)
        )([...parentInfo.dfRegistry.values()]));
};

export const isConstant = (contextInfo: TContextInfo) =>
                          (lunaDf: TLunaDf): boolean =>
    contextInfo.constants.has(getCanonicalLunaDf(contextInfo)(lunaDf).fullName);

export const getConstant = (contextInfo: TContextInfo) =>
                           (lunaDf: TLunaDf) =>
    contextInfo.constants.get(getCanonicalLunaDf(contextInfo)(lunaDf).fullName);

export const isNotConstant = (contextInfo: TContextInfo) =>
                             (lunaDf: TLunaDf): boolean =>
    !isConstant(contextInfo)(lunaDf);

export const getEntryDf = (contextInfo: TContextInfo) =>
                          (lunaDf: TLunaDf) => {
    const canonicalLunaDf =  getCanonicalLunaDf(contextInfo)(lunaDf);
    if (isNotConstant(contextInfo)(canonicalLunaDf)) {
        return contextInfo.dfRegistry.get(canonicalLunaDf.fullName);
    }
    return undefined//createPromelaDf(canonicalLunaDf)('constant');
}

export const addEntryDfs = (contextInfo: TContextInfo) =>
                           (promelaDfs: readonly TPromelaDf[]): TContextInfo => {
    const dfRegistry = new Map([
        ...contextInfo.dfRegistry,
        ...promelaDfs
            .filter(df => isNotConstant(contextInfo)(df.lunaDf))
            .map((df): [string, TPromelaDf] => [df.lunaDf.fullName, df])
    ]);

    return ContextInfo(dfRegistry, contextInfo.aliases, contextInfo.constants);
};

export const addAlias = (contextInfo: TContextInfo) =>
                        (lunaName: string) =>
                        (lunaDf: TLunaDf): TContextInfo => {
    const aliases = new Map([
        ...contextInfo.aliases,
        [lunaName, lunaDf.fullName]
    ]);
    return ContextInfo(contextInfo.dfRegistry, aliases, contextInfo.constants);
};

export const addConstant = (contextInfo: TContextInfo) =>
                           (lunaDf: TLunaDf) =>
                           (condition: TCondition): TContextInfo => {
            const constants = new Map([
                ...contextInfo.constants,
                [getCanonicalLunaDf(contextInfo)(lunaDf).fullName, condition]
            ]);
            return ContextInfo(contextInfo.dfRegistry, contextInfo.aliases, constants);
        };

const replaceChars = (lunaDf: TLunaDf) =>
                     (aliases: Map<string, string>) => {
    let lunaName = lunaDf.fullName;
    const keys = [...aliases.keys()].sort((a, b) => b.length - a.length);
    function replaceOnce(str: string) {
        for (const key of keys) {
            if (str.includes(key)) {
                return str.replace(key, aliases.get(key));
            }
        }
        return str;
    }
    let prev: string;
    do {
        prev = lunaName;
        lunaName = replaceOnce(lunaName);
    } while (prev !== lunaName && lunaName.length < 10);
    return lunaName;
};

const parseIndices = (s: string): readonly [string, ...readonly NestedStringArray[]] => {
    let variable = '';
    const indices: string[] = [];
    let i = 0;
    while (i < s.length && s[i] !== '[') {
        variable += s[i];
        i++;
    }
    let depth = 0;
    let buffer = '';
    for (; i < s.length; i++) {
        const char = s[i];
        if (char === '[') {
            depth++;
            if (depth > 1) {
                buffer += '[';
            }
        } else if (char === ']') {
            depth--;
            if (depth === 0) {
                indices.push(buffer);
                buffer = '';
            } else if (depth > 0) {
                buffer += ']';
            }
        } else {
            buffer += char;
        }
    }
    const df: [string, ...NestedStringArray[]] = [variable];
    for (const ind of indices) {
        if (ind.includes('[')) {
            df.push(parseIndices(ind));
        } else {
            df.push(ind);
        }
    }
    return df;
};
