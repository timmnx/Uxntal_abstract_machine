open Compiler
open Compile
open Machine

let () =
  let file_machine_h = open_out "tal/machine_h.tal" in
  let file_machine = open_out "tal/machine.tal" in
  let repr =
    m
    |> AstNormalize.normalize
    |> IrFromAstNormalized.translate_to_ir
    |> UxnFromIr.from_program
  in
  Compile.compile_header (Format.formatter_of_out_channel file_machine_h) repr;
  Compile.compile (Format.formatter_of_out_channel file_machine) repr;
  close_out file_machine_h;
  close_out file_machine;
;;
