open Caju.Lexer

let token = Alcotest.testable pp_token eq_token

let test_parse_int () =
  let buffer = String.to_seq "123" in
  let actual = Option.get @@ parse_int buffer in
  Alcotest.(check token) "Buffer with digits only" (Digit 123) actual
  
let test_parse_complex_buffer () =
  let buffer = String.to_seq "12 feijao com arroz" in
  let actual = Option.get @@ parse_int buffer in
  Alcotest.(check token) "Buffer starting with numbers" (Digit 12) actual

let test_parse_int_stop_on_whitespace () =
  let buffer = String.to_seq "1 2 feijao com arroz" in
  let actual = Option.get @@ parse_int buffer in
  Alcotest.(check token) "Stop on whitespace" (Digit 1) actual

let test_parse_int_noop_on_fail () =
  let buffer = String.to_seq "feijao com arroz" in
  Alcotest.(check @@ option token) "Noop on buffer without numbers" None (parse_int buffer)

let () =
  let open Alcotest in
  Alcotest.run "Lexer tests" [
  "lex digits", [
    test_case "simple-case" `Quick test_parse_int;
    test_case "complex-case" `Quick test_parse_complex_buffer;
    test_case "stop-on-whitespace" `Quick test_parse_int_stop_on_whitespace;
    test_case "noop-on-fail" `Quick test_parse_int_noop_on_fail;
  ]
]
