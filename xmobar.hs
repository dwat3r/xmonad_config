Config { font = "xft:xos4 Terminus"
       , bgColor = "black"
       , fgColor = "#777777"
       , position = TopW L 98
       , lowerOnStart = True
       , hideOnStart  = False
       , persistent   = True
       , commands = [ Run Battery [ 
                            "-t" , "bat: <acstatus> <left>%"
                            , "--Low"      , "10"
                            , "--High"     , "80"
                            , "--low"      , "darkred"
                            , "--normal"   , "darkorange"
                            , "--high"     , "darkgreen"
                            , "--" -- battery specific options
                                       -- discharging status
                                       , "-o" , "<fc=darkred>-</fc>(<timeleft>)"
                                       -- AC "on" status
                                       , "-O" , "<fc=darkorange>+</fc>(<timeleft>)"
                                       -- charged status
                                       , "-i" , ""
                             ] 50
                    , Run Cpu ["-t","cpu: <total>%", "-L","3","-H","50","--normal","darkgreen","--high","red"] 30
                    , Run CoreTemp [ "-t" , "<core0>°C", "--Low", "70", "--High", "80", "--low", "darkgreen", "--normal", "darkorange", "--high", "darkred" ] 50
                    , Run Memory ["-t","mem: <usedratio>%"] 10
                    , Run Date "%a %b %_d %H:%M" "date" 10
                    , Run Wireless "wlp3s0" [ "-t", "<essid> <qualityvbar>" ] 20
                    , Run MPD ["-t", "<state>: <file>"] 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %mpd% | %wlp3s0wi% | %battery% | %cpu% %coretemp% | %memory% | <fc=darkorange>%date%</fc>"
       }