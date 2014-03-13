Config {
    font = "xft:Fixed-8",
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = TopP 0 112,
    lowerOnStart = True,
    commands = [
        Run MultiCpu ["-t","Cpu: <total0> <total1> <total2> <total3>","-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 30,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 30,
        Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 30,
        Run Network "wlan0" ["-t","Net: <rx>, <tx>","-H","200","-L","10","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 30,
        Run Date "%a %b %_d %l:%M" "date" 30,
        Run BatteryP ["BAT0"] ["-t", "<acstatus><watts> (<left>%)", "-L", "10", "-H", "80", "-p", "3",
                               "--", "-O", "<fc=green>On</fc> - ", "-i", "", "-L", "-15", "-H", "-5",
                               "-l", "red", "-m", "blue", "-h", "green"] 600,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %multicpu%   %memory%   %swap%   %wlan0%   <fc=#FFFFCC>%date%</fc>   %battery%"
}
