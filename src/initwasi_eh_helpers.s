        .text
        .file "initwasi_eh_helpers.s"

        .tagtype ocaml_exception
        .globl ocaml_exception
ocaml_exception:

        .functype wasicaml_try (i32, i32) -> (i32)
        .functype wasicaml_try2 (i32, i32, i32) -> (i32)
        .functype wasicaml_try3 (i32, i32, i32, i32) -> (i32)
        .functype wasicaml_try4 (i32, i32, i32, i32, i32) -> (i32)
        .functype wasicaml_try5 (i32, i32, i32, i32, i32, i32) -> (i32)
        .functype wasicaml_throw () -> ()

        .section .text.wasicaml_try,"",@
        .globl wasicaml_try
        .type wasicaml_try,@function
wasicaml_try:
        .functype wasicaml_try (i32, i32) -> (i32)
        try
        local.get 1
        local.get 0
        call_indirect (i32) -> (i32)
        return
        catch ocaml_exception
        i32.const 0
        return
        end_try
        unreachable
        end_function

        .section .text.wasicaml_try2,"",@
        .globl wasicaml_try2
        .type wasicaml_try2,@function
wasicaml_try2:
        .functype wasicaml_try2 (i32, i32, i32) -> (i32)
        try
        local.get 1
        local.get 2
        local.get 0
        call_indirect (i32, i32) -> (i32)
        return
        catch ocaml_exception
        i32.const 0
        return
        end_try
        unreachable
        end_function

        .section .text.wasicaml_try3,"",@
        .globl wasicaml_try3
        .type wasicaml_try3,@function
wasicaml_try3:
        .functype wasicaml_try3 (i32, i32, i32, i32) -> (i32)
        try
        local.get 1
        local.get 2
        local.get 3
        local.get 0
        call_indirect (i32, i32, i32) -> (i32)
        return
        catch ocaml_exception
        i32.const 0
        return
        end_try
        unreachable
        end_function

        .section .text.wasicaml_try4,"",@
        .globl wasicaml_try4
        .type wasicaml_try4,@function
wasicaml_try4:
        .functype wasicaml_try4 (i32, i32, i32, i32, i32) -> (i32)
        try
        local.get 1
        local.get 2
        local.get 3
        local.get 4
        local.get 0
        call_indirect (i32, i32, i32, i32) -> (i32)
        return
        catch ocaml_exception
        i32.const 0
        return
        end_try
        unreachable
        end_function

        .section .text.wasicaml_try5,"",@
        .globl wasicaml_try5
        .type wasicaml_try5,@function
wasicaml_try5:
        .functype wasicaml_try5 (i32, i32, i32, i32, i32, i32) -> (i32)
        try
        local.get 1
        local.get 2
        local.get 3
        local.get 4
        local.get 5
        local.get 0
        call_indirect (i32, i32, i32, i32, i32) -> (i32)
        return
        catch ocaml_exception
        i32.const 0
        return
        end_try
        unreachable
        end_function

        .section .text.wasicaml_throw,"",@
        .globl wasicaml_throw
        .type wasicaml_throw,@function
wasicaml_throw:
        .functype wasicaml_throw () -> ()
        throw ocaml_exception
        unreachable
        end_function
