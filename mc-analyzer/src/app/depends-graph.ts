import {TDfPull, getAllPromelaDfs} from './df/df-pull';
import {TPromelaDf} from './df/promela-df';
import {join, mapArray, reduce, sort} from './utils';

export type TDependsGraph = {
    readonly dependencies: Map<string, Set<string>>
    readonly dfPull: TDfPull;
};

export const DependsGraph = (dependencies: Map<string, Set<string>>, dfPull: TDfPull) =>
    ({ dependencies: new Map(dependencies),  dfPull});

export const addDependency = (dependsGraph: TDependsGraph) =>
                             (dfUseEntry: TPromelaDf) =>
                             (dfInitEntry: TPromelaDf): TDependsGraph => {
    const dependencies: Map<string, Set<string>> = new Map(dependsGraph.dependencies);
    if (dependencies.has(dfInitEntry.promelaName)) {
        dependencies.get(dfInitEntry.promelaName).add(dfUseEntry.promelaName);
    } else {
        dependencies.set(dfInitEntry.promelaName, new Set<string>([dfUseEntry.promelaName]));
    }
    return DependsGraph(dependencies, dependsGraph.dfPull);
};

export const findAllCycles = (dependsGraph: TDependsGraph): readonly (readonly TPromelaDf[])[] => {
    const depthFirstSearch = (node: string, path: string[] = []): string[][] => {
        visited.add(node);
        path = [node].concat(path);
        return reduce([...dependsGraph.dependencies.get(node) || []])
                     ([])
                     ((previousCycles: string[][]) => (neighbor: string) =>
                         path.includes(neighbor)
                             ? [...previousCycles, path.slice(0, path.indexOf(neighbor) + 1)]
                             : [...previousCycles, ...depthFirstSearch(neighbor, path)]
                     );
    };
    const visited = new Set<string>();
    const cycles = reduce([...(dependsGraph.dependencies.keys())])
                         ([])
                         ((previousCycles: string[][]) => (node: string) =>
                             visited.has(node)
                                 ? previousCycles
                                 : [...previousCycles, ...depthFirstSearch(node)]);
    const uniqueCycles = toUniqueCycles(cycles);
    return mapArray(getAllPromelaDfs(dependsGraph.dfPull))(uniqueCycles);
};

const toUniqueCycles = (arrays: readonly (readonly string[])[]) => {
    const unique = new Map<string, readonly string[]>();
    for (const elem of arrays) {
        unique.set(
            join('->')(sort(elem)),
            elem
        );
    }
    return [...unique.values()];
};

// TODO
// type Dependencies = Map<string, Set<string>>
//
// export const depthFirstSearch = (): State<Dependencies, string[][]> => {
//     const state: State<Dependencies, string[][]> = bind(get<Dependencies>())((state: Dependencies) => {
//         const visited = new Set<string>(node)
//         visited.add(node);
//         path = [node].concat(path);
//         return [...dependencies.get(node) || []]
//             .reduce((cycles: string[][], neighbor: string) =>
//                 cycles.concat(path.includes(neighbor) ? [path.slice(0, path.indexOf(neighbor) + 1)] : depthFirstSearch(neighbor, path)), []);
//         return of(findAllCycles)
//     });
//     return put<Dependencies>(state)(() => of([[]] as string[][]))
// }

