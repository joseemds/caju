open Caju

let token = Alcotest.testable Token.pp Token.eq

let test_tokenize_all_tokens () =
  let buf = "abc<>+-abc" in
  let lexer = Lexer.of_string buf in
  let actual = Lexer.tokenize lexer in
  let expected =
    Token.
      [
        Digit 123;
        Ident "abc";
        LeftChevron;
        RightChevron;
        LeftParen;
        RightParen;
        Equal;
        Plus;
        Minus;
        If;
        Else;
        Then;
        Let;
        Or;
        And;
        True;
        False;
        EOF;
      ]
  in
  Alcotest.(check (list token)) "tokens are equal" expected actual

let () =
  let open Alcotest in
  run "Lexer tests"
    [
      ( "tokenize",
        [
          test_case "tokenize consume all tokens" `Quick test_tokenize_all_tokens;
        ] );
    ]
