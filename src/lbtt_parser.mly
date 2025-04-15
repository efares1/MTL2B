(*http://www.tcs.hut.fi/Software/lbtt/doc/html/Format-for-automata.html*)
%{
    open Lbtt
%}
%token <int> INT
%token <string> ST
%token SPC
%token NOT AND OR IMP EQV EXC TRUE FALSE
%token <string> EVENT
%token <string> RESET
%token <string> UNCH
%token <string*string*int> CLK
%token END EOF EOL
%start <automaton> automaton
%%

automaton: ns=INT SPC c=cond_specifier EOL sl=state_list EOF { (ns,c,sl) }
;
cond_specifier: st=ST {st} | {""};

state_list:  l=state_list  st=state {st::l}
  | {[]}
;
state :  stateId=INT SPC init=INT EOL tl=transition_list {(stateId, init=1, tl)}
;

cond_list :  SPC acc=INT cl=cond_list { acc::cl }
          |  SPC END  { [] }
;

transition_list : tr=transition tl=transition_list { tr::tl }
               |  END EOL { [] }
;
transition: stateId=INT cl=cond_list SPC gf=guard_formula EOL { (stateId,cl,gf,[])}
;

guard_formula:
    TRUE  { True }
  | FALSE  { False} 
  | p=EVENT { Atom p }
  | p=RESET { Reset p }
  | p=UNCH { Unch p }
  | p=CLK { let (x,o,d) = p in Clk (x,op_of_string o,d) }
  | NOT SPC gf=guard_formula { mk_not gf }
  | AND SPC gf1=guard_formula SPC gf2=guard_formula { And(gf1,gf2) }
  | OR  SPC gf1=guard_formula SPC gf2=guard_formula { Or(gf1,gf2) }
  | IMP SPC gf1=guard_formula SPC gf2=guard_formula { mk_imp gf1 gf2 }
  | EQV SPC gf1=guard_formula SPC gf2=guard_formula { mk_eqv gf1 gf2 }
  | EXC SPC gf1=guard_formula SPC gf2=guard_formula { mk_excl gf1 gf2 }
;
