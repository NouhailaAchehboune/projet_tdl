
open Compilateur

(* Changer le chemin d'accès du jar. *)
(*let runtamcmde = "java -jar ../../runtam.jar"*)
let runtamcmde = "java -jar /Users/nouhailaachehboune/Desktop/ENSEEIHT/prog_fonct/Projet/runtam.jar" 

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
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testprintbool" =
  runtam "../../fichiersRat/src-rat-tam-test/testprintbool.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testprintrat" =
   runtam "../../fichiersRat/src-rat-tam-test/testprintrat.rat";
   [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testaddint" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddint.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testaddrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testaddrat.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testmultint" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultint.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testmultrat" =
  runtam "../../fichiersRat/src-rat-tam-test/testmultrat.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testnum" =
  runtam "../../fichiersRat/src-rat-tam-test/testnum.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testdenom" =
  runtam "../../fichiersRat/src-rat-tam-test/testdenom.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testwhile1" =
  runtam "../../fichiersRat/src-rat-tam-test/testwhile1.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testif1" =
  runtam "../../fichiersRat/src-rat-tam-test/testif1.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "testif2" =
  runtam "../../fichiersRat/src-rat-tam-test/testif2.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factiter" =
  runtam "../../fichiersRat/src-rat-tam-test/factiter.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "complique" =
  runtam "../../fichiersRat/src-rat-tam-test/complique.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 2 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factfun1" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun1.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 6 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factfun2" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun2.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 6 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factfun3" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun3.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 8 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factfun4" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun4.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 8 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factfun5" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun5.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 7 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factfun6" =
  runtam "../../fichiersRat/src-rat-tam-test/testfun6.rat";
  [%expect{|Semantic error: asm.SemanticError: Ligne 78 : double déclaration de l'étiquette 'main'.|}]

let%expect_test "factfuns" =
  runtam "../../fichiersRat/src-rat-tam-test/testfuns.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 43 : double déclaration de l'étiquette 'main'. |}]

let%expect_test "factrec" =
  runtam "../../fichiersRat/src-rat-tam-test/factrec.rat";
  [%expect{| Semantic error: asm.SemanticError: Ligne 28 : double déclaration de l'étiquette 'main'. |}]


