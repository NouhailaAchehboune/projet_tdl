
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TRUE
    | STRUCT
    | SLASH
    | RETURN
    | RAT
    | PV
    | PRINT
    | POINT
    | PO
    | PLUSEQ
    | PLUS
    | PF
    | NUM
    | NULL
    | NEW
    | MULT
    | INT
    | INF
    | IF
    | ID of (
# 11 "parser.mly"
       (string)
# 31 "parser.ml"
  )
    | FALSE
    | EQUAL
    | EOF
    | ENTIER of (
# 10 "parser.mly"
       (int)
# 39 "parser.ml"
  )
    | ELSE
    | DENOM
    | CONST
    | CO
    | CF
    | CALL
    | BOOL
    | AO
    | AF
    | ADDR
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState116
  | MenhirState110
  | MenhirState107
  | MenhirState101
  | MenhirState98
  | MenhirState93
  | MenhirState83
  | MenhirState80
  | MenhirState78
  | MenhirState77
  | MenhirState74
  | MenhirState71
  | MenhirState69
  | MenhirState65
  | MenhirState62
  | MenhirState59
  | MenhirState55
  | MenhirState47
  | MenhirState46
  | MenhirState43
  | MenhirState38
  | MenhirState30
  | MenhirState27
  | MenhirState26
  | MenhirState23
  | MenhirState22
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState13
  | MenhirState12
  | MenhirState8
  | MenhirState2
  | MenhirState0

# 3 "parser.mly"
  

open Type
open Ast.AstSyntax

# 108 "parser.ml"

let rec _menhir_run49 : _menhir_env -> ('ttv_tail * _menhir_state) * _menhir_state * (Ast.AstSyntax.affectable) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (a1 : (Ast.AstSyntax.affectable))), (n : (
# 11 "parser.mly"
       (string)
# 128 "parser.ml"
            ))) = _menhir_stack in
            let _v : (Ast.AstSyntax.affectable) = 
# 90 "parser.mly"
                        (Acces (a1,n))
# 133 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.AstSyntax.affectable) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (a1 : (Ast.AstSyntax.affectable))) = _menhir_stack in
    let _v : (Ast.AstSyntax.expression) = 
# 106 "parser.mly"
                          (Affectable a1)
# 155 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_prog : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.programme) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (lfi : (Ast.AstSyntax.programme))) = _menhir_stack in
            let _v : (Ast.AstSyntax.programme) = 
# 63 "parser.mly"
                          (lfi)
# 175 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.AstSyntax.programme)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (lf : (Ast.AstSyntax.fonction))), _, (lfi : (Ast.AstSyntax.programme))) = _menhir_stack in
        let _v : (Ast.AstSyntax.programme) = 
# 66 "parser.mly"
                          (let (Programme (lf1,li))=lfi in (Programme (lf::lf1,li)))
# 194 "parser.ml"
         in
        _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_a : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.affectable) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState101 | MenhirState93 | MenhirState83 | MenhirState77 | MenhirState74 | MenhirState71 | MenhirState14 | MenhirState65 | MenhirState62 | MenhirState59 | MenhirState55 | MenhirState17 | MenhirState22 | MenhirState23 | MenhirState38 | MenhirState26 | MenhirState27 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POINT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (a1 : (Ast.AstSyntax.affectable))) = _menhir_stack in
            let _v : (Ast.AstSyntax.affectable) = 
# 89 "parser.mly"
                          (Deref a1)
# 233 "parser.ml"
             in
            _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | POINT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL | INF | MULT | PF | PLUS ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 | MenhirState13 | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDR ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | AO ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | CALL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | CO ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | DENOM ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | ENTIER _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | NUM ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | PO ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | TRUE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState101
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_cp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.AstSyntax.expression))), _, (le : (Ast.AstSyntax.expression list))) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression list) = 
# 124 "parser.mly"
                (e1::le)
# 315 "parser.ml"
         in
        _menhir_goto_cp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (c : (Ast.AstSyntax.expression list))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 120 "parser.mly"
                          (Creation(c))
# 331 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (n : (
# 11 "parser.mly"
       (string)
# 352 "parser.ml"
            ))), _, (lp : (Ast.AstSyntax.expression list))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 104 "parser.mly"
                          (AppelFonction (n,lp))
# 357 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_is : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.bloc) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
            let _v : (Ast.AstSyntax.bloc) = 
# 71 "parser.mly"
                          (li)
# 386 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState69 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _v : (Ast.AstSyntax.instruction) = 
# 84 "parser.mly"
                                    (TantQue (exp,li))
# 397 "parser.ml"
                 in
                _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
            | MenhirState78 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | AO ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState80
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState80 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))), _, (li1 : (Ast.AstSyntax.bloc))), _, (li2 : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _v : (Ast.AstSyntax.instruction) = 
# 83 "parser.mly"
                                    (Conditionnelle (exp,li1,li2))
# 429 "parser.ml"
                 in
                _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
            | MenhirState12 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
                let _v : (Ast.AstSyntax.programme) = 
# 67 "parser.mly"
                          (Programme ([],li))
# 439 "parser.ml"
                 in
                _menhir_goto_prog _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i1 : (Ast.AstSyntax.instruction))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
        let _v : (Ast.AstSyntax.bloc) = 
# 75 "parser.mly"
                          (i1::li)
# 457 "parser.ml"
         in
        _menhir_goto_is _menhir_env _menhir_stack _menhir_s _v
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 472 "parser.ml"
            ))), _, (p : ((Type.typ * string) list))), _, (li : (Ast.AstSyntax.bloc))) = _menhir_stack in
            let _v : (Ast.AstSyntax.fonction) = 
# 69 "parser.mly"
                                         (Fonction(t,n,p,li))
# 477 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | ID _v ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
            | INT ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | RAT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | STRUCT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState116
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | PO ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_reduce1 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 11 "parser.mly"
       (string)
# 525 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (n : (
# 11 "parser.mly"
       (string)
# 531 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.AstSyntax.affectable) = 
# 88 "parser.mly"
                          (Ident n)
# 536 "parser.ml"
     in
    _menhir_goto_a _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_i : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | CONST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | ID _v ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
    | IF ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | INT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | PO ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | PRINT ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | RAT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | RETURN ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | WHILE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | AF ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState98
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.AstSyntax.expression list) = 
# 123 "parser.mly"
                ([])
# 581 "parser.ml"
     in
    _menhir_goto_cp _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_dp : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Type.typ * string) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 595 "parser.ml"
        ))), _, (lp : ((Type.typ * string) list))) = _menhir_stack in
        let _v : ((Type.typ * string) list) = 
# 94 "parser.mly"
                          ((t,n)::lp)
# 600 "parser.ml"
         in
        _menhir_goto_dp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (le : ((Type.typ * string) list))) = _menhir_stack in
            let _v : (Type.typ) = 
# 97 "parser.mly"
                           (Enre le )
# 616 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | CONST ->
                    _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | ID _v ->
                    _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                | IF ->
                    _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | INT ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | PO ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | PRINT ->
                    _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | RAT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | RETURN ->
                    _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | STRUCT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | WHILE ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | AF ->
                    _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.AstSyntax.bloc) = 
# 74 "parser.mly"
                          ([])
# 688 "parser.ml"
     in
    _menhir_goto_is _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | MULT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PO ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run77 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 852 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | PLUSEQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | AO ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | CALL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | CO ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DENOM ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ENTIER _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | FALSE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | ID _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
        | NULL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NUM ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PO ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | TRUE ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
    | EQUAL ->
        _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run86 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ENTIER _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | PV ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), (n : (
# 11 "parser.mly"
       (string)
# 931 "parser.ml"
                    ))), (e : (
# 10 "parser.mly"
       (int)
# 935 "parser.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.AstSyntax.instruction) = 
# 81 "parser.mly"
                                    (Constante (n,e))
# 940 "parser.ml"
                     in
                    _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.AstSyntax.expression) = 
# 107 "parser.mly"
                          (Booleen true)
# 980 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
    | MULT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NEW ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState16 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | INT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | RAT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | STRUCT ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState16
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.AstSyntax.expression) = 
# 119 "parser.mly"
                          (Null)
# 1082 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 1089 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce1 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.AstSyntax.expression) = 
# 108 "parser.mly"
                          (Booleen false)
# 1103 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (int)
# 1110 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (
# 10 "parser.mly"
       (int)
# 1118 "parser.ml"
    )) = _v in
    let _v : (Ast.AstSyntax.expression) = 
# 109 "parser.mly"
                          (Entier e)
# 1123 "parser.ml"
     in
    _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDR ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | AO ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | CALL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | CO ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | DENOM ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | ENTIER _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | NUM ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | PO ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | TRUE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | PF ->
                _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDR ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | AO ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | CALL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | CO ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | DENOM ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ENTIER _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | FALSE ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | ID _v ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NUM ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | PO ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | TRUE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | AF ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (n : (
# 11 "parser.mly"
       (string)
# 1307 "parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression) = 
# 117 "parser.mly"
                          (Adresse n)
# 1313 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_e : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.AstSyntax.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState26 | MenhirState30 | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADDR ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | AO ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | CALL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | CO ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DENOM ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | ENTIER _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | FALSE ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | ID _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | NULL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | NUM ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | PO ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | TRUE ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | AF | PF ->
            _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDR ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | AO ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | CALL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | CO ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | DENOM ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ENTIER _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NUM ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PO ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | TRUE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 105 "parser.mly"
                          (Binaire(Fraction,e1,e2))
# 1419 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression) = 
# 111 "parser.mly"
                          (Unaire(Denominateur,e1))
# 1435 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
        let _v : (Ast.AstSyntax.expression) = 
# 110 "parser.mly"
                          (Unaire(Numerateur,e1))
# 1445 "parser.ml"
         in
        _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDR ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | AO ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | CALL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | CO ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | DENOM ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ENTIER _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | NUM ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | PO ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | TRUE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
        | INF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDR ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | AO ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | CALL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | CO ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | DENOM ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | ENTIER _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | NUM ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | PO ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | TRUE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
        | MULT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDR ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | AO ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | CALL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | CO ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | DENOM ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | ENTIER _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | NUM ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | PO ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | TRUE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59)
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 116 "parser.mly"
                          (exp)
# 1560 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | PLUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ADDR ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | AO ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CALL ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | CO ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | DENOM ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | ENTIER _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | FALSE ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | ID _v ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NUM ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | PO ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | TRUE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 112 "parser.mly"
                          (Binaire (Plus,e1,e2))
# 1615 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 113 "parser.mly"
                          (Binaire (Mult,e1,e2))
# 1637 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 115 "parser.mly"
                          (Binaire (Inf,e1,e2))
# 1659 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))), _, (e2 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 114 "parser.mly"
                          (Binaire (Equ,e1,e2))
# 1681 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AO ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (exp : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 85 "parser.mly"
                                    (Retour (exp))
# 1714 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 82 "parser.mly"
                                    (Affichage (e1))
# 1736 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AO ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (n : (
# 11 "parser.mly"
       (string)
# 1768 "parser.ml"
            ))), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 80 "parser.mly"
                                    (Ajout (n,e1))
# 1773 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (t : (Type.typ))), (n : (
# 11 "parser.mly"
       (string)
# 1794 "parser.ml"
            ))), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 78 "parser.mly"
                                    (Declaration (t,n,e1))
# 1799 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | PV ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (a1 : (Ast.AstSyntax.affectable))), _, (e1 : (Ast.AstSyntax.expression))) = _menhir_stack in
            let _v : (Ast.AstSyntax.instruction) = 
# 79 "parser.mly"
                                    (Affectation (a1,e1))
# 1821 "parser.ml"
             in
            _menhir_goto_i _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run7 : _menhir_env -> 'ttv_tail * _menhir_state * (Type.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_menhir_stack, _menhir_s, (t : (Type.typ))) = _menhir_stack in
    let _v : (Type.typ) = 
# 101 "parser.mly"
             (Pointeur t)
# 1841 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : ((Type.typ * string) list) = 
# 93 "parser.mly"
                          ([])
# 1850 "parser.ml"
     in
    _menhir_goto_dp _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | CONST ->
        _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ID _v ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | IF ->
        _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PO ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PRINT ->
        _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | RAT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | RETURN ->
        _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | WHILE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | AF ->
        _menhir_reduce35 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Type.typ) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState107 | MenhirState8 | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BOOL ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | INT ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | RAT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | STRUCT ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | AF | PF ->
                _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
        | MULT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MULT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
        | PF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _, (t : (Type.typ))) = _menhir_stack in
            let _v : (Ast.AstSyntax.expression) = 
# 118 "parser.mly"
                          (New t)
# 1941 "parser.ml"
             in
            _menhir_goto_e _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState110 | MenhirState98 | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ADDR ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | AO ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | CALL ->
                    _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | CO ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | DENOM ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | ENTIER _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | FALSE ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | ID _v ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
                | NULL ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | NUM ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | PO ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | TRUE ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState93
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MULT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | PO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BOOL ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | INT ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | RAT ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | STRUCT ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | PF ->
                    _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MULT ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState2 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AO ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BOOL ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | INT ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | RAT ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | STRUCT ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | AF ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState2
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState2)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Type.typ) = 
# 100 "parser.mly"
          (Rat)
# 2233 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Type.typ) = 
# 99 "parser.mly"
          (Int)
# 2244 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 11 "parser.mly"
       (string)
# 2251 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AO ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Type.typ) = 
# 98 "parser.mly"
          (Bool)
# 2272 "parser.ml"
     in
    _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.AstSyntax.programme) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BOOL ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RAT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2318 "parser.ml"
