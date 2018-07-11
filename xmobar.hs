Config { font = "xft:Terminus"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 100
       , commands = [ Run Weather "LHBP" ["-t"," <tempC>C","-L","64","-H","77","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 20
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%a %b %_d %H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory%    <fc=#ee9a00>%date%</fc> | %LHBP%"
       }