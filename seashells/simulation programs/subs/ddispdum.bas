DECLARE SUB switchscreen (inz%, iactive%, igraph%, kd%, programname$, fdelay)
DECLARE SUB rwparameterdisk (RW$, ifilenumber%)
DECLARE SUB whatprmfiles (s$(), programnameloc$, progrtmp$, exten$, drive$, irfilename$, ilineparam%)
DECLARE SUB screencheck (igraph%, lastline%, lastrow%, ivgashift%, ifree%)
DECLARE SUB disp (kdisplay%, il%, ax!(), ico%, iwi%, ff!, xo!, yo!, kax%, kx%, kay%, ky%, tvv$)
DECLARE SUB backfront (inz%, x0!, x1!, y4!, y1!, s0r!, s1r!, s0l!, s1l!, ico%, iwil%, px!, p4!, pyy!)
DECLARE SUB cmessage (itype%, iline%, irow%, ilettercol%, iletterback%, ibackc%, text$, resp$)
DECLARE SUB growthxy (iinsert%, js%, imx%, kn%, lax%(), lx%(), lay%(), ly%(), ax!())
DECLARE SUB leftright (x1!, x2!, x3!, y1!, y2!, y3!, s1r!, s2r!, s3r!, s1l!, s2l!, s3l!, ico%, iwil%, px!, p4!)
DECLARE SUB linep (ihp%, x1!, y1!, x2!, y2!, ipat%, iwidth%, ico%, ityp%)
DECLARE SUB perpendicular (x0!, y0!, ybase0!, s0l!, s0r!, ico%, iwil%)
DECLARE SUB rl (inz%, x2!, y2!, s2!, x1!, yto!, s1!, y3!, isi%, iy%, jyto%, ico%, iwil%, px!)
DECLARE SUB zeingabe (igt%, iquest%, inz%, i%, f!, labels$, text$)

DEFINT I-N
' $INCLUDE: 'scommon.bi'
' $INCLUDE: '..\subs\errlist.bas'

SUB disp (kdisplay, il, axy(), icol, iwil, ff, xo, yo, kax, kx, kay, ky, tvv$) STATIC
'this is a dummy
END SUB

'DEFBL A-G, O-Z
SUB manipulateXY (mwasnun$, iinsert) STATIC
'this is a dummy
END SUB

SUB showdisplay (kdispl, displaywastmp$) STATIC
'this is a dummy
END SUB

