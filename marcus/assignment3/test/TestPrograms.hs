module TestPrograms (
  p
, pComment
, p1
, p4
, pPretty
) where

p = ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

pComment = ("\
\read k;\
\read n;\
\-- Hi, Hej\n\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      -- Skip same as just comment\n\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

pPretty = ""

p1 = ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

p4 = ("\
\read a;\
\read b;\
\-- a comment\n\
\s := 3;\
\while a do\
\  begin\
\    c := a^s;\
\    d := 2^a;\
\    write c;\
\    write d;\
\    a := a-1;\
\  end\
\write a;")
