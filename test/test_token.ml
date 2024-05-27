open Caju
open Token

let token = Alcotest.testable pp eq

let test_parse_int () =
  let buffer = String.to_seq "123" in
  let actual, _= parse_int buffer in
  Alcotest.(check token) "Buffer with digits only" (Digit 123) actual

let test_parse_complex_buffer () =
  let buffer = String.to_seq "12 feijao com arroz" in
  let actual, _ = parse_int buffer in
  Alcotest.(check token) "Buffer starting with numbers" (Digit 12) actual

let test_parse_int_stop_on_whitespace () =
  let buffer = String.to_seq "1 2 feijao com arroz" in
  let actual, _ = parse_int buffer in
  Alcotest.(check token) "Stop on whitespace" (Digit 1) actual

(* let test_parse_int_noop_on_fail () = *)
(*   let buffer = String.to_seq "feijao com arroz" in *)
(*   Alcotest.(check token) *)
(*     "Noop on buffer without numbers" Let (parse_int buffer) *)

let () =
  let open Alcotest in
  Alcotest.run "Token tests"
    [
      ( "lex digits",
        [
          test_case "simple-case" `Quick test_parse_int;
          test_case "complex-case" `Quick test_parse_complex_buffer;
          test_case "stop-on-whitespace" `Quick
            test_parse_int_stop_on_whitespace;
          (* test_case "noop-on-fail" `Quick test_parse_int_noop_on_fail; *)
        ] );
    ]
