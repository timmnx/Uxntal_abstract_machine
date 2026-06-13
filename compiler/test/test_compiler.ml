open Compiler

(* let space () =
  print_newline ();
  "[*" ^ String.make 64 '-' ^ "*]" |> print_endline;
  "[*" ^ String.make 64 '-' ^ "*]" |> print_endline;
  "[*" ^ String.make 64 '-' ^ "*]" |> print_endline;
  print_newline ()
;;

let test1 =
  let mDesugared = BnfNormalize.Desugar.desugar Machine1.m in
  let mMerged = BnfNormalize.Merge.merge mDesugared in
  let irM = IrFromBnfNormalized.translate_to_ir mMerged in
  BnfPrinting.print_machine Format.std_formatter Machine1.m;
  space ();
  BnfPrinting.print_machine Format.std_formatter mDesugared;
  space ();
  BnfPrinting.print_machine Format.std_formatter mMerged;
  space ();
  print_endline (Ir.show_program irM);
  space ();
  IrPrinting.program_pp Format.std_formatter irM;
  ()
;; *)

(* let%test "fail" = false *)
