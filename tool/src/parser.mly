%{
    open Mtl
%}
%token <int> INT
%token <string> LABEL
%token NEXT
%token UNTIL
%token XUNTIL
%token RELEASE
%token XRELEASE
%token <string> EVENT
%token NOT
%token AND
%token OR IMP EQV
%token LE LT GE GT
%token LPAR
%token RPAR
%token BOX DIAM XBOX XDIAM
%token LBRACK RBRACK
%token TRUE FALSE
%token EOF

%left UNTIL RELEASE XUNTIL XRELEASE
%left OR
%left AND
%right IMP
%nonassoc EQV
%nonassoc NEXT NOT BOX XBOX DIAM XDIAM

%start <mtl> form
%start <(string*mtl)list> forms
%%

forms:
  l=LABEL m=mtl EOF { [l,m] }
| l=LABEL m=mtl f=forms { (l,m)::f }
;

form: m=mtl EOF {m}
;
mtl:
  | e=EVENT  { Event e }
  | TRUE { True }
  | FALSE { False }
  | NOT m=mtl  { mtl_not m } 
  | BOX c=ctr m=mtl  { mtl_box c m } 
  | XBOX c=ctr m=mtl  { mtl_xbox c m } 
  | DIAM c=ctr m=mtl  { mtl_diam c m } 
  | XDIAM c=ctr m=mtl  { mtl_xdiam c m } 
  | NEXT p=mtl { Next p }
  | p1=mtl AND p2=mtl  { And(p1,p2) } 
  | p1=mtl IMP p2=mtl  { Or(mtl_not p1,p2) } 
  | p1=mtl EQV p2=mtl  { And(Or(mtl_not p1,p2),Or(mtl_not p2,p1)) }
  | p1=mtl OR p2=mtl  { Or(p1,p2) }
  | p1=mtl UNTIL c=ctr p2=mtl  { Until(c,p1,p2) }
  | p1=mtl RELEASE c=ctr p2=mtl  { Release(c,p1,p2) }
  | p1=mtl XUNTIL c=ctr p2=mtl  { XUntil(c,p1,p2) }
  | p1=mtl XRELEASE c=ctr p2=mtl  { XRelease(c,p1,p2) }
  | LPAR p=mtl RPAR { p }
;

ctr:
  | LBRACK LE d=INT RBRACK { LE d }
  | LBRACK LT d=INT RBRACK { LT d }
  | LBRACK GE d=INT RBRACK { GE d }
  | LBRACK GT d=INT RBRACK { GT d }
  |  { Untimed }
;
