import {mkdtempSync, readFileSync, writeFileSync} from 'fs';
import path from 'path';
import { execSync } from 'child_process';
import { TLtl } from './ltl/ltl';
import { tmpdir } from 'os';
import { PromelaAST, TPromelaAST } from './promela-node/promela-ast';
import { getBegin } from './promela-node/position-table';
import {includes, mapArray} from './utils';
import minimist, {ParsedArgs} from 'minimist';
import {pipe} from 'fp-ts/function';
import {Option, getOrElse, fromNullable, match, tryCatch, fromPredicate} from 'fp-ts/Option';
import {concat, reduce} from 'fp-ts/Array';
import {LunaAST} from './luna-ast';

type McAnalyzeConfig = {
    lunaSourcePath: string;
    errorsFile: Option<string>;
};

type DeclaredLines = {
    file: string,
    line: number,
    name: string,
    charIndex: number,
    foundLine: number
};

type AdaptError = {
    details: {
        df: {
            name: string;
            declared: DeclaredLines[];
            initialized: any[][];
            used: any[];
        },
        type: 'true' | 'false',
        condition: string,
        used: {
            true: any
        }
    }
    error_code: string;
    foundLine?: string;
    file?: string;
    line?: number;
    name?: 'main';
    charIndex?: number;
};

const exec = (command: string) =>
             (workDir: string): Buffer | string =>
    execSync(command, {cwd: workDir});

const at = (index: number) =>
           (arr: readonly unknown[]) =>
    arr.at(index);


const verify = (lunaSourcePath: string) =>
               (prettyAST: TPromelaAST) =>
    pipe(
       prettyAST.ltls,
       reduce([], (previousErrors: AdaptError[], ltl: TLtl) =>
           pipe(
               verifyModel(ltl.name),
               fromPredicate(Boolean),
               match(
                   () => [],
                   () => pipe(
                       getTrace(),
                       (spinOutput: string) => [...spinOutput.matchAll(/verification\.pml:(\d+)/g)],
                       mapArray((expression: RegExpExecArray)  => Number(expression[1])),
                       at(-1),
                       getBegin(prettyAST.positionTable),
                       (promelaLine: number) => [{
                           error_code: ltl.code,
                           details: {
                               type: ltl.bool,
                               condition: ltl.condition,
                               where: {
                                   name: "name",
                                   type: "type",
                                   file: lunaSourcePath,
                                   line: 1
                               },
                               identifier: {
                                   name: ltl.promelaDfs[0].lunaDf.fullName,
                                   declared: getDeclaredPosition(promelaLine)(prettyAST.lunaSourceLines)(lunaSourcePath),
                               },
                               cf: {
                                   name: "cfName",
                                   type: "extern",
                                   file: lunaSourcePath,
                                   line: 1
                               },
                               call_stack_entry: [{
                                   file: lunaSourcePath,
                                   line: 1,
                                   name: "main"
                               }],
                               used: {
                                   true: ltl.promelaDfs[0].lunaDf.fullName,
                                   local: ltl.promelaDfs[0].lunaDf.fullName,
                                   conditions: ltl.promelaDfs[0].lunaDf.fullName,
                                   where: {

                                   },
                                   df: {
                                       declared: [[getDeclaredPosition(promelaLine)(prettyAST.lunaSourceLines)(lunaSourcePath)]],
                                       name: ltl.promelaDfs[0].lunaDf.fullName,
                                       true: ltl.promelaDfs[0].lunaDf.fullName,
                                       local: ltl.promelaDfs[0].lunaDf.fullName
                                   }
                               },
                               initialized: {
                                   true: ltl.promelaDfs[0].lunaDf.fullName,
                                   local: ltl.promelaDfs[0].lunaDf.fullName,
                                   conditions: ltl.promelaDfs[0].lunaDf.fullName,
                                   where: {

                                   },
                                   df: {
                                       declared: [[getDeclaredPosition(promelaLine)(prettyAST.lunaSourceLines)(lunaSourcePath)]],
                                       name: ltl.promelaDfs[0].lunaDf.fullName,
                                       true: ltl.promelaDfs[0].lunaDf.fullName,
                                       local: ltl.promelaDfs[0].lunaDf.fullName
                                   }
                               }
                           }

                       }])
               ),
               concat(previousErrors)
           )
       )
    );

const getDeclaredPosition = (index: number) =>
                            (lunaSourceLines: readonly string[]) =>
                            (lunaSourcePath: string) => {
    let charCount = 0;
    let lineNumber = 0;
    let charIndex = 0;
    let foundLine = lunaSourceLines[0];
    for (const line of lunaSourceLines) {
        ++lineNumber;
        charCount += line.length + 1;
        if (charCount >= index) {
            charIndex = index - (charCount - line.length) + 2;
            foundLine = line;
            break;
        }
    }
    return {
        file: lunaSourcePath,
        line: lineNumber,
        name: 'main',
        charIndex,
        foundLine
    };
};

const readFile = (path: string): string =>
    readFileSync(path, 'utf8');

const writeText = (path: string) =>
                  (buffer: string) =>
    writeFileSync(path, buffer);

const timer = (timerName: string) => (func: () => unknown) => {
    console.time(timerName);
    const res = func();
    console.timeEnd(timerName);
    return res;
};

const config = pipe(
    minimist(process.argv.slice(2), {string: ['project-dir', 'errors-file', 'output-dir'] }),
    (parsedArgs: ParsedArgs) => ({
        lunaSourcePath: path.resolve(parsedArgs._[0]),
        errorsFile: fromNullable(parsedArgs['errors-file'] && path.resolve(parsedArgs['errors-file'])),
        workDir: parsedArgs['output-dir'] ?? mkdtempSync(path.join(tmpdir(), 'lpv-'))
    })
);

const createPromelaBin = (promelaAST: TPromelaAST) => {
    // console.log(promelaAST.promelaCode);
    writeText(path.join(config.workDir, 'verification.pml'))(promelaAST.promelaCode);
    exec(`spin -a verification.pml`)(config.workDir);
    exec(`gcc -w -o ./pan ./pan.c`)(config.workDir);
    return promelaAST;
}

const parseLunaFile = (): LunaAST => {
    exec(`parser -o ast.json ${config.lunaSourcePath}`)(config.workDir);
    return pipe(
        path.join(config.workDir, 'ast.json'),
        readFile,
        JSON.parse
    );
};

const getLunaLines = () =>
    readFile(config.lunaSourcePath).split('\n');

const verifyModel = (ltlName: string) => includes(
    exec(`./pan -a  -N ${ltlName}`)(config.workDir).toString()
)(`wrote verification.pml`);

const getTrace = () => {
    try {
        return exec(`spin -t  -p verification.pml`)(config.workDir).toString();
    } catch (e) {
        return '';
    }
};

const getPromelaAST = (config: McAnalyzeConfig): TPromelaAST =>
    PromelaAST(getLunaLines(), parseLunaFile())

const handleErrors = (mcAnalyserErrors: AdaptError[]) =>
    match(
        () => undefined, //console.log(JSON.stringify(mcAnalyserErrors)),
        (file: string) =>
            pipe(
                tryCatch(() => JSON.parse(readFile(file))),
                getOrElse(() => []),
                concat(mcAnalyserErrors),
                JSON.stringify,
                writeText(file)
            )
    )(config.errorsFile);

pipe(
    timer('parseLunaFile')(parseLunaFile),
    (f: LunaAST) => timer('createPromelaAST')(() => PromelaAST(getLunaLines(), f)),
    (f: TPromelaAST) => timer('createPromelaBin')(() => createPromelaBin(f)),
    (f: TPromelaAST) => timer('verify')(() => verify(config.lunaSourcePath)(f)),
    (f: AdaptError[]) => timer('handleErrors')(() => handleErrors(f))
);
