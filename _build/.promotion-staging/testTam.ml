
open Compilateur

(* Changer le chemin d'accès du jar. *)
(*let runtamcmde = "java -jar ../../runtam.jar"*)
let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" 

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)
let%expect_test "testprintint" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintint.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 26, characters 2-62
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testprintbool" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintbool.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 30, characters 2-63
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testprintrat" =
   runtam "../../fichiersRat/src-rat-tam-test/testprintrat.rat";
   [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 34, characters 3-63
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testaddint" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddint.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 38, characters 2-60
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testaddrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddrat.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 42, characters 2-60
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testmultint" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultint.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 46, characters 2-61
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testmultrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultrat.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 50, characters 2-61
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testnum" =
  runtam "../../fichiersRat/src-rat-tam-test/testnum.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 54, characters 2-57
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testdenom" =
  runtam "../../fichiersRat/src-rat-tam-test/testdenom.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 58, characters 2-59
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testwhile1" =
  runtam "../../fichiersRat/src-rat-tam-test/testwhile1.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 62, characters 2-60
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testif1" =
  runtam "../../fichiersRat/src-rat-tam-test/testif1.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 66, characters 2-57
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "testif2" =
  runtam "../../fichiersRat/src-rat-tam-test/testif2.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 70, characters 2-57
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factiter" =
  runtam "../../fichiersRat/src-rat-tam-test/factiter.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 74, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "complique" =
  runtam "../../fichiersRat/src-rat-tam-test/complique.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 78, characters 2-59
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factfun1" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun1.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 82, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factfun2" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun2.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 86, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factfun3" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun3.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 90, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factfun4" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun4.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 94, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factfun5" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun5.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 98, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factfun6" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun6.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 102, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factfuns" =
  runtam "../../fichiersRat/src-rat-tam-test/testfuns.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 106, characters 2-58
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]

let%expect_test "factrec" =
  runtam "../../fichiersRat/src-rat-tam-test/factrec.rat";
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  End_of_file
  Raised at Stdlib.input_line.scan in file "stdlib.ml", line 450, characters 14-31
  Called from Rat__TestTam.runtamcode in file "testTam.ml", line 15, characters 16-29
  Called from Rat__TestTam.runtam in file "testTam.ml" (inlined), line 22, characters 15-46
  Called from Rat__TestTam.(fun) in file "testTam.ml", line 110, characters 2-57
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  Error: Unable to access jarfile /mnt/n7fs/.../tools/runtam/runtam.jar |}]


