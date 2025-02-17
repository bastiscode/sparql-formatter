/* tslint:disable */
/* eslint-disable */
/**
* @returns {any}
*/
export function commonSPARQLPrefixes(): any;
/**
*/
export class SPARQLFormatter {
  free(): void;
/**
*/
  constructor();
/**
* @param {string} query
* @param {any} prefixes
* @param {number} indent
* @returns {string}
*/
  format(query: string, prefixes: any, indent: number): string;
}

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly __wbg_sparqlformatter_free: (a: number) => void;
  readonly sparqlformatter_new_js: (a: number) => void;
  readonly sparqlformatter_format: (a: number, b: number, c: number, d: number, e: number, f: number) => void;
  readonly commonSPARQLPrefixes: (a: number) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_add_to_stack_pointer: (a: number) => number;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __wbindgen_exn_store: (a: number) => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;
/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {SyncInitInput} module
*
* @returns {InitOutput}
*/
export function initSync(module: SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {InitInput | Promise<InitInput>} module_or_path
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: InitInput | Promise<InitInput>): Promise<InitOutput>;
