declare class Blake2b {
  update(input: Uint8Array): this
  digest(out: 'hex'): string
  digest(out: 'binary' | Uint8Array): Uint8Array
}
type Ctx = {
  b: Uint8Array;
  h: Uint32Array;
  t: number; // input count
  c: number; // pointer within buffer
  outlen: number; // output length in bytes
};

declare module 'blake2b' {
  export default function createHash(
    outlen?: number,
    key?: Uint8Array | undefined,
    salt?: Uint8Array | undefined,
    personal?: Uint8Array | undefined,
    noAssert?: boolean
  ): Blake2b
}
