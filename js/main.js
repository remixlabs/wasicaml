'use strict';

const { WASI } = require("@wasmer/wasi");
const wasiBindings = require("@wasmer/wasi/lib/bindings/node");
const fs = require('fs');
const util = require("util");
const process = require("process")
const { execSync } = require("child_process");

if (process.stdin._handle)
    process.stdin._handle.setBlocking(true);
if (process.stdout._handle)
    process.stdout._handle.setBlocking(true);
if (process.stderr._handle)
    process.stderr._handle.setBlocking(true);

class OCamlExn extends Error {
}

function wasicaml_try(session) {
    return function(f, ctx) {
        try {
            session.instance.exports.wasicaml_call(f, ctx);
            return 0;
        } catch (e) {
            if (e instanceof OCamlExn) {
                return 1;
            } else
                throw e;
        }
    }
}

function wasicaml_try4(session) {
    return function(f, ctx1, ctx2, ctx3, ctx4) {
        try {
            session.instance.exports.wasicaml_call4(f, ctx1, ctx2, ctx3, ctx4);
            return 0;
        } catch (e) {
            if (e instanceof OCamlExn) {
                return 1;
            } else
                throw e;
        }
    }
}

function wasicaml_throw(session) {
    return function() {
        throw new OCamlExn("An OCaml exception")
    }
}

function get_string(session, ptr) {
    let mem = new Uint8Array(session.instance.exports.memory.buffer);
    let n = 0;
    while (mem[ptr+n] != 0) n++;
    let buf = new Uint8Array(n);
    buf.set(mem.subarray(ptr, ptr+n));
    // this is not quite correct, but node does not give us anything else:
    return new TextDecoder().decode(buf);
}

function wasicaml_system(session) {
    return function(cmd_ptr) {
        let cmd = get_string(session, cmd_ptr);
        try {
            execSync(cmd, { stdio: "inherit" });
            return 0;
        } catch (e) {
            console.error("SYSTEM: ", cmd);
            console.error(e);
            return 1;  // FIXME
        }
    }
}

function wasicaml_rename(session) {
    return function(old_ptr, new_ptr) {
        let old_name = get_string(session, old_ptr);
        let new_name = get_string(session, new_ptr);
        try {
            fs.renameSync(old_name, new_name);
            return 0;
        } catch (e) {
            // the most important codes:
            let code = util.getSystemErrorName(e.errno);
            console.error("RENAME ", old_name, "->", new_name, ": ", code);
            switch (code) {
            case "EACCES": return 2;
            case "EEXIST": return 20;
            case "EINVAL": return 28;
            case "EISDIR": return 31;
            case "ELOOP": return 32;
            case "ENOENT": return 44;
            case "ENOTDIR": return 54;
            case "ENOTEMPTY": return 55;
            case "EPERM": return 63;
            default: return 63;  // also EPERM for anything else
            }
        }
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
        ...wasiBindings,
        args: args,
        env: process.env,
        preopenDirectories: { "/": "/" },
    });
    let wasi_imports = wasi.getImports(wasm_mod).wasi_snapshot_preview1;
    let session =
        { instance: null };
    let realimport = {};
    for (let n of syscalls) {
        let f = wasi_imports[n];
        realimport[n] = ((...args) => {
            //console.error("WASI syscall: " + n);
            switch (n) {
            case "path_open":
                let open_path = get_string(session, args[2]);
                // console.error("  ", open_path);
                break;
            case "path_rename":
                let old_path = get_string(session, args[1]);
                let new_path = get_string(session, args[4]);
                // console.error("  ", old_path, "->", new_path);
                break;
            case "path_filestat_get":
                let stat_path = get_string(session, args[2]);
                // console.error("  ", stat_path);
                break;
            };
            let r = f.apply(undefined, args);
            //if (r != 0)
            //    console.error("  code: ", r);
            return r;
        });
    };
    const importObject =
          { wasi_snapshot_preview1: realimport,
            wasicaml: { "wasicaml_try": wasicaml_try(session),
                        "wasicaml_try4": wasicaml_try4(session),
                        "wasicaml_throw": wasicaml_throw(session),
                        "wasicaml_system": wasicaml_system(session),
                        "wasicaml_rename": wasicaml_rename(session)
                      }
          };
    session.instance = await WebAssembly.instantiate(wasm_mod, importObject);
    wasi.start(session.instance);
}

function bufferFindElements(buf, start, elements) {
    outer:
    for (let k = start; k < buf.length; k++) {
        if (buf[k] == elements[0]) {
            for (let j = 0; j < elements.length; j++) {
                if (buf[k+j] != elements[j]) continue outer;
            }
            return k;
        }
    }
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
        let search = 0;
        let idx = bufferFindElements(wasm_code_u8, search, [ 0, 0x4c, 0x45, 0x4e ]);
        let len = 0;
        if (idx >= 0) {
            const eol = wasm_code_u8.subarray(idx+4).indexOf(10) + idx+4;
            const len_buf = wasm_code_u8.subarray(idx+4, eol);
            const len_str = new TextDecoder().decode(len_buf);
            len = parseInt(len_str);
            search = eol+1;
        };
        idx = bufferFindElements(wasm_code_u8, search, [ 0, 0x61, 0x73, 0x6d ]);
        if (idx < 0) {
            throw new Error("cannot find wasm start in file " + wasm_code_filename);
        };
        let wasm_code;
        if (len > 0)
            wasm_code = wasm_code_u8.subarray(idx, idx+len);
        else
            wasm_code = wasm_code_u8.subarray(idx);
        const wasm_mod = await WebAssembly.compile(wasm_code);
        await instantiate(wasm_mod, args);
    } catch (e) {
        console.error("WASM error: " + e);
        console.error(e.stack);
        process.exit(2);
    }
})()
