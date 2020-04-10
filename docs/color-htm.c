#ifdef POSITIVE
#   define BGCLR   "#ffffff"
#   define TXTCLR  "#000000"
#   define LNKCLR  "#770000"
#   define ALNKCLR "#0000ff"
#   define VLNKCLR "#007700"
#else
# ifdef GREY
#   define BGCLR   "#7f7f7f"
#   define TXTCLR  "#ffffff"
#   define LNKCLR  "#ff5f7f"
#   define ALNKCLR "#ffffff"
#   define VLNKCLR "#2fcf5f"
# else
#  ifdef BLUE
#   define BGCLR   "#00007f"
#   define TXTCLR  "#ffffff"
#   define LNKCLR  "#ff5f7f"
#   define ALNKCLR "#ffffff"
#   define VLNKCLR "#2fcf5f"
#  else
#   define BGCLR   "#000000"
#   define TXTCLR  "#ffffff"
#   define LNKCLR  "#ff5f7f"
#   define ALNKCLR "#ffffff"
#   define VLNKCLR "#2fcf5f"
#  endif
# endif
#endif
<body bgcolor=BGCLR text=TXTCLR link=LNKCLR alink=ALNKCLR vlink=VLNKCLR>
