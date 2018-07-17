Config { font = "xft:Terminus"
       , bgColor = "black"
       , fgColor = "grey"
       , position = TopW L 95
       , lowerOnStart = True
       , commands = [ Run BatteryP ["BAT0"] ["-t", "bat: <acstatus> <left>%", "-L", "10", "-H", "80", "-p", "3","--", "-O", "<fc=green>On</fc> - ", "-i", "","-L", "-15", "-H", "-5", "-l", "red", "-m", "blue", "-h", "green"] 600
                    , Run Weather "LHBP" ["-t"," <tempC>C","-L","64","-H","77","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 20
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%a %b %_d %H:%M" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %battery% | %cpu% | %memory%    <fc=#ee9a00>%date%</fc> | %LHBP%"
       }