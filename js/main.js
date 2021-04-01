'use strict';

const { WASI } = require('wasi');
const fs = require('fs');
const process = require("process")

if (process.stdout._handle)
    process.stdout._handle.setBlocking(true);
if (process.stderr._handle)
    process.stderr._handle.setBlocking(true);

class OCamlExn extends Error {
}

function wasicaml_try(session) {
    return function(f, ctx) {
        try {
            console.log("try");
            session.instance.exports.wasicaml_call(f, ctx);
            console.log("try ok");
            return 0;
        } catch (e) {
            console.log("catch");
            if (e instanceof OCamlExn) {
                return 1;
            } else
                throw e;
        }
    }
}

function wasicaml_throw(session) {
    return function() {
        console.log("throw");
        throw new OCamlExn("An OCaml exception")
    }
}

let syscalls =
    [ "args_get",
      "args_sizes_get",
      "environ_get",
      "environ_sizes_get",
      "clock_res_get",
      "clock_time_get",
      "fd_advise",
      "fd_allocate",
      "fd_close",
      "fd_datasync",
      "fd_fdstat_get",
      "fd_fdstat_set_flags",
      "fd_fdstat_set_rights",
      "fd_filestat_get",
      "fd_filestat_set_size",
      "fd_filestat_set_times",
      "fd_pread",
      "fd_prestat_get",
      "fd_prestat_dir_name",
      "fd_pwrite",
      "fd_read",
      "fd_readdir",
      "fd_renumber",
      "fd_seek",
      "fd_sync",
      "fd_tell",
      "fd_write",
      "path_create_directory",
      "path_filestat_get",
      "path_filestat_set_times",
      "path_link",
      "path_open",
      "path_readlink",
      "path_remove_directory",
      "path_rename",
      "path_symlink",
      "path_unlink_file",
      "poll_oneoff",
      "proc_exit",
      "proc_raise",
      "sched_yield",
      "random_get",
      "sock_recv",
      "sock_send",
      "sock_shutdown"
    ]

async function instantiate(wasm_mod, args) {
    const wasi = new WASI({
        args: args,
        env: process.env,
        preopens: { "/": "/" },
    });
    let session =
        { instance: null };
    let realimport = {};
    for (let n of syscalls) {
        let f = wasi.wasiImport[n];
        realimport[n] = ((...args) => {
            console.log("WASI syscall: " + n);
            if (n == "fd_readdir") {
                let r = f.apply(undefined, args);
                let [ fd, buf, buf_len, cookie, bufused ] = args;
                let mem32 = new Uint32Array(session.instance.exports.memory.buffer);
                console.log("errno=", r);
                console.log("args=", args);
                console.log("bufused=", mem32[bufused >> 2]);
                return r;
            };
            return f.apply(undefined, args)
        });
    };
    const importObject =
          { wasi_snapshot_preview1: realimport,
            wasicaml: { "try": wasicaml_try(session),
                        "throw": wasicaml_throw(session)
                      }
          };
    session.instance = await WebAssembly.instantiate(wasm_mod, importObject);
    wasi.start(session.instance);
}

(async () => {
    // argv[0]: node
    // argv[1]: the path of this script
    // argv[2]: the file with the wasm code
    // argv[3]...: the args to pass to the sandbox
    try {
        const wasm_code_filename = process.argv[2];
        const args = process.argv.slice(3);
        const wasm_code_buf = fs.readFileSync(wasm_code_filename);
        const wasm_code_u8 = new Uint8Array(wasm_code_buf);
        const start = wasm_code_u8.indexOf(0);
        if (wasm_code_u8[start+1] != 0x61 || wasm_code_u8[start+2] != 0x73 || wasm_code_u8[start+3] != 0x6d) {
            throw new Error("cannot find wasm start in file " + wasm_code_filename);
        };
        const wasm_code = wasm_code_u8.subarray(start);
        const wasm_mod = await WebAssembly.compile(wasm_code);
        await instantiate(wasm_mod, args);
    } catch (e) {
        console.error("WASM error: " + e);
        console.error(e.stack);
        process.exit(2);
    }
})()
