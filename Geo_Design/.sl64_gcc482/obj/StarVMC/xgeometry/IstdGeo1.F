C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE ISTDGEO1                                                 1
C ISTDGEO1 IS THE THE STAR IST GEOMETRY MODELING IN AGML                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'ISTDGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE THE STAR IST GEOMETRY MODELING IN AGML'/      1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 27 NOVEMBER 2013                                                1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'27 NOVEMBER 2013'/                           2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR YAPING WANG (UNIVERSITY OF ILLINOIS AT CHICAGO)                  2
      CHARACTER*40 AUTHOR /'YAPING WANG (UNIVERSITY OF ILLINOIS AT CHICA  3
     *GO)'/                                                               3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C - Advanced Geant interface                                              4
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      4
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       4
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         4
     *AG_OPTION,AG_ORT,AG_MARK                                            4
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         4
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     4
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     4
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    4
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          4
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   4
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     4
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        4
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   4
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 4
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     4
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       4
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        4
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       4
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       4
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       4
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         4
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     4
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       4
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   4
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       4
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     4
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          4
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         4
     *-696969,AG_LDETU=250)                                               4
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    4
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  4
     *AG_IMFLD,AG_ISIMU                                                   4
C Inherited variables a: saved during internal calls                      4
C b: reset at the entry in a block and then saved                         4
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        4
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       4
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         4
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     4
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       4
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         4
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           4
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        4
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        4
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            4
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           4
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    4
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      4
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           4
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          4
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                4
C local variables valid inside same block                                 4
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       4
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  4
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         4
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    4
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          4
     *AG_OPTION                                                           4
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        4
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             4
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              4
C temporarly                                                              4
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           4
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    4
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       4
     *AG_HY)                                                              4
      REAL MIL_P,INCH_P                                                   4
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               4
C - GEANT description of logical units                                    4
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
      REAL *4 AANGLE,RADDEG,LADDER_THK,LADDER_WIDTH,LADDER_LENGTH,X, Y,   5
     *XP, YP, XPOS, YPOS,ZPOS,XLOCAL, YLOCAL, MODULERADIUS,TUBEANGLE,     5
     *XTUBEARC,YTUBEARC,ZTUBEARC                                          5
      REAL *4 BANGLE,XCTJPOS(12),YCTJPOS(12)                              9
      REAL *4 RADIUS_A,RADIUS_B,RADIUS_C,RADIUS_T,TILT_A,TILT_B,TILT_C,   12
     *TILT_T,ANGLE_A, ANGLE_B,ANGLE_C,ANGLE_T,ANGLEPIT_A,ANGLEPIT_B,      12
     *ANGLEPIT_C,ANGLEPIT_T                                               12
      INTEGER *4 NL,NCTJ,NCABLESET,LY,NU,NE,NES,NS,NSC,NUM,NSEG,DEBUG     14
      INTEGER BLOCK_ICCT                                                  105
      INTEGER BLOCK_ICCA                                                  105
      INTEGER BLOCK_ICCL                                                  105
      INTEGER BLOCK_ICLP                                                  105
      INTEGER BLOCK_IBMO                                                  105
      INTEGER BLOCK_IBAM                                                  105
      INTEGER BLOCK_ILCC                                                  105
      INTEGER BLOCK_ICCU                                                  105
      INTEGER BLOCK_ICCD                                                  105
      INTEGER BLOCK_IBCF                                                  105
      INTEGER BLOCK_ICFC                                                  105
      INTEGER BLOCK_ICFD                                                  105
      INTEGER BLOCK_ICFB                                                  105
      INTEGER BLOCK_ICFA                                                  105
      INTEGER BLOCK_IBAT                                                  105
      INTEGER BLOCK_ICTC                                                  105
      INTEGER BLOCK_ICTE                                                  105
      INTEGER BLOCK_ICTW                                                  105
      INTEGER BLOCK_IBCW                                                  105
      INTEGER BLOCK_ILST                                                  105
      INTEGER BLOCK_ISTC                                                  105
      INTEGER BLOCK_ISTH                                                  105
      INTEGER BLOCK_ILSB                                                  105
      INTEGER BLOCK_ISBC                                                  105
      INTEGER BLOCK_ISBH                                                  105
      INTEGER BLOCK_IBHT                                                  105
      INTEGER BLOCK_IHTC                                                  105
      INTEGER BLOCK_IHTH                                                  105
      INTEGER BLOCK_IBHB                                                  105
      INTEGER BLOCK_IHBC                                                  105
      INTEGER BLOCK_IHBH                                                  105
      INTEGER BLOCK_IBHC                                                  105
      INTEGER BLOCK_IHCA                                                  105
      INTEGER BLOCK_IBLM                                                  105
      INTEGER BLOCK_IBSS                                                  105
      INTEGER BLOCK_IBTE                                                  105
      INTEGER BLOCK_IBBE                                                  105
      INTEGER BLOCK_IBLE                                                  105
      INTEGER BLOCK_IBRE                                                  105
      INTEGER BLOCK_IBAP                                                  105
      INTEGER BLOCK_IECW                                                  105
      INTEGER BLOCK_ICWT                                                  105
      INTEGER BLOCK_ICWB                                                  105
      INTEGER BLOCK_IABW                                                  105
      INTEGER BLOCK_IAAW                                                  105
      INTEGER BLOCK_ITNW                                                  105
      INTEGER BLOCK_ITSW                                                  105
      INTEGER BLOCK_IESB                                                  105
      INTEGER BLOCK_IESA                                                  105
      INTEGER BLOCK_IESC                                                  105
      INTEGER BLOCK_IECE                                                  105
      INTEGER BLOCK_ICET                                                  105
      INTEGER BLOCK_ICEB                                                  105
      INTEGER BLOCK_IABE                                                  105
      INTEGER BLOCK_IAAE                                                  105
      INTEGER BLOCK_ITNE                                                  105
      INTEGER BLOCK_ITSE                                                  105
      INTEGER BLOCK_IESE                                                  105
      INTEGER BLOCK_IESS                                                  105
      INTEGER BLOCK_IBRB                                                  105
      INTEGER BLOCK_IRSA                                                  105
      INTEGER BLOCK_IRSB                                                  105
      INTEGER BLOCK_IRSC                                                  105
      INTEGER BLOCK_IBSR                                                  105
      INTEGER BLOCK_ISRH                                                  105
      INTEGER BLOCK_ISRR                                                  105
      INTEGER BLOCK_ISRB                                                  105
      INTEGER BLOCK_IRBA                                                  105
      INTEGER BLOCK_ISRL                                                  105
      INTEGER BLOCK_ISRC                                                  105
      INTEGER BLOCK_ISRA                                                  105
      INTEGER BLOCK_ISRO                                                  105
      INTEGER BLOCK_IROA                                                  105
      INTEGER BLOCK_ISRS                                                  105
      INTEGER BLOCK_ISCM                                                  105
      INTEGER BLOCK_ISWS                                                  105
      INTEGER BLOCK_ISCH                                                  105
      INTEGER BLOCK_ISCS                                                  105
      INTEGER BLOCK_ISCR                                                  105
      INTEGER BLOCK_IBTS                                                  105
      INTEGER BLOCK_IBCJ                                                  105
      INTEGER BLOCK_ICJR                                                  105
      INTEGER BLOCK_ICJS                                                  105
      INTEGER BLOCK_ICJA                                                  105
      INTEGER BLOCK_ICJU                                                  105
      INTEGER BLOCK_ICLN                                                  105
      INTEGER BLOCK_ISCA                                                  105
      INTEGER BLOCK_ISCB                                                  105
      INTEGER BLOCK_ISCC                                                  105
      INTEGER BLOCK_ICLE                                                  105
      INTEGER BLOCK_ISCT                                                  105
      INTEGER BLOCK_ICTJ                                                  105
      INTEGER BLOCK_ICTA                                                  105
      INTEGER BLOCK_ICTB                                                  105
      REAL *4 HALFLADWDT/3.6559/                                          106
      REAL *4 HALFLADTHK/1.8957/                                          108
      REAL *4 HALFLADLEN/40.0877/                                         110
      REAL *4 HALFCOMBWDT/1.800/                                          112
      REAL *4 HALFCOMBTHK/0.2900/                                         114
      REAL *4 HALFCOMBLEN/27.94/                                          116
      REAL *4 HALFCOMBTHKCONNECT/0.28315/                                 118
      REAL *4 HALFCOMBLENCONNECT/4.4125/                                  120
      REAL *4 HALFFOAMWDT/1.250/                                          122
      REAL *4 HALFFOAMTHK/0.2900/                                         124
      REAL *4 HALFFOAMLEN/27.94/                                          126
      REAL *4 HALFFOAMTHKCONNECT/0.28315/                                 128
      REAL *4 HALFFOAMLENCONNECT/4.4125/                                  130
      REAL *4 DISTFOAMTOLADDER/1.0618/                                    132
      REAL *4 TUBERMIN/0.18225/                                           134
      REAL *4 TUBERMAX/0.238125/                                          136
      REAL *4 TUBECONEOUTERRMIN/0.31625/                                  138
      REAL *4 TUBECONEOUTERRMAX/0.3721/                                   140
      REAL *4 HALFTUBELEN/40.0877/                                        142
      REAL *4 HALFTUBECONELEN/0.0889/                                     144
      REAL *4 DISTTUBETOPSKIN/0.2922/                                     146
      REAL *4 LIQUIDINTUBER/0.18225/                                      148
      REAL *4 LIQUIDINTUBELEN/40.0877/                                    150
      REAL *4 HALFSKINWDT/3.05/                                           152
      REAL *4 HALFSKINTHK/0.0075/                                         154
      REAL *4 HALFSKINLEN/28.440/                                         156
      REAL *4 HALFTHKSKINMOM/0.010925/                                    158
      REAL *4 HALFLENSKINCONNECT/4.5725/                                  160
      REAL *4 HALFHBDWDTTB/3.05/                                          162
      REAL *4 HALFHBDTHKTB/0.008125/                                      164
      REAL *4 HALFHBDLENTB/28.440/                                        166
      REAL *4 HALFHBDTHKTBCONNECT/0.01155/                                168
      REAL *4 HALFHBDLENTBCONNECT/4.565/                                  170
      REAL *4 HALFTHKAIRINHBD/0.003425/                                   172
      REAL *4 HBDARCRMIN/0.29815/                                         174
      REAL *4 HBDARCRMAX/0.32125/                                         176
      REAL *4 HALFHBDARCLEN/28.44/                                        178
      REAL *4 AIRINHBDARCRMAX/0.305/                                      180
      REAL *4 AIRINHBDHALFLEN/23.875/                                     182
      REAL *4 HALFAPVWDT/0.41/                                            184
      REAL *4 HALFAPVTHK/0.015/                                           186
      REAL *4 HALFAPVLEN/0.35875/                                         188
      REAL *4 APVGAPSIZE/0.5375/                                          190
      REAL *4 APVSENSORGAPSIZE/0.1019/                                    192
      REAL *4 HALFSENSORWDT/1.9008/                                       194
      REAL *4 HALFSENSORTHK/0.015/                                        196
      REAL *4 HALFSENSORLEN/3.765/                                        198
      REAL *4 HALFSENSOREDGELR/0.04/                                      200
      REAL *4 HALFSENSOREDGET/0.0364/                                     202
      REAL *4 HALFSENSOREDGEB/0.0628/                                     204
      REAL *4 SENSORGAPSIZE/0.04/                                         206
      REAL *4 HALFSENMOMWDT/2.46095/                                      208
      REAL *4 HALFSENMOMTHK/0.015/                                        210
      REAL *4 HALFSENMOMLEN/3.865/                                        212
      REAL *4 DISTLADDERTOIBLM/21.3906/                                   214
      REAL *4 DISTSENSORTOLADDER/0.1/                                     216
      REAL *4 HALFENDCAPMOMWDT/3.05/                                      218
      REAL *4 HALFENDCAPMOMTHK/0.305/                                     220
      REAL *4 HALFENDCAPMOMLEN/1.125/                                     222
      REAL *4 HALFOVERLAPENDCAPSKINTHK/0.2775/                            224
      REAL *4 CONNECTENDCAPSKINLEN/0.6/                                   226
      REAL *4 OVERLAPENDCAPSKINLEN/0.5/                                   228
      REAL *4 HALFTBOARDWDT/1.9355/                                       230
      REAL *4 HALFTBOARDTHK/0.0823/                                       232
      REAL *4 HALFTBOARDLEN/5.830/                                        234
      REAL *4 OUTSIDETUBEWESTLEN/10.1548/                                 236
      REAL *4 OUTSIDETUBEEASTLEN/9.6406/                                  238
      REAL *4 OUTSIDETBOARDLEN/0.874/                                     240
      REAL *4 HALFTBSLOTWDT/0.25/                                         242
      REAL *4 HALFTBSLOTTHK/0.288075/                                     244
      REAL *4 HALFTBSLOTLEN/1.650/                                        246
      REAL *4 TBSLOTGAPSIZE/0.150/                                        248
      REAL *4 DISTSLOTCTOFOAM/0.1582/                                     250
      REAL *4 OVERLAPSLOTSLEN/0.4/                                        252
      REAL *4 HALFSRMOMWDT/1.77875/                                       254
      REAL *4 HALFSRMOMTHK/1.5696/                                        256
      REAL *4 HALFSRMOMLEN/0.635/                                         258
      REAL *4 DISTSRMOMTOECEDGE/0.2738/                                   260
      REAL *4 DISTSRMOMTOHBDEDGE/0.2063/                                  262
      REAL *4 MODULEINNERRADIUS/11.9253/                                  264
      REAL *4 CABLEDIAMETER/0.372/                                        266
      REAL *4 IBMOINNERRADIUS/11.84/                                      268
      REAL *4 IBMOOUTERRADIUS/18.5/                                       270
      INTEGER *4 NLADDER/24/                                              272
      INTEGER *4 NUNIT/6/                                                 274
      INTEGER *4 NAPVUNIT/6/                                              276
      REAL *4 TILT/19.1042/                                               278
      REAL *4 ANGLEOFFSET/1.1661/                                         280
C -                                                                       282
      INTEGER PARO_VERSION                                                282
      EQUIVALENCE (BANK_PARO(3),PARO_VERSION,RE_PARO_VERSION),            282
     *(NAMES_PARO(1),NPARO_VERSION)                                       282
      EQUIVALENCE (COMMEN_PARO(1),CPARO_VERSION),(MAP_PARO(1,1),          282
     *MPARO_VERSION)                                                      282
      CHARACTER CPARO_VERSION*40/' '/                                     282
      CHARACTER NPARO_VERSION*12/'I.VERSION'/                             282
      INTEGER MPARO_VERSION(2)/1,1/                                       282
      REAL RE_PARO_VERSION                                                282
C -                                                                       282
      REAL PARO_DX                                                        282
      EQUIVALENCE (BANK_PARO(4),PARO_DX,RE_PARO_DX),(NAMES_PARO(2),       282
     *NPARO_DX)                                                           282
      EQUIVALENCE (COMMEN_PARO(2),CPARO_DX),(MAP_PARO(1,2),MPARO_DX)      282
      CHARACTER CPARO_DX*40/' '/                                          282
      CHARACTER NPARO_DX*12/'R.DX'/                                       282
      INTEGER MPARO_DX(2)/1,1/                                            282
      REAL RE_PARO_DX                                                     282
C -                                                                       282
      REAL PARO_DY                                                        282
      EQUIVALENCE (BANK_PARO(5),PARO_DY,RE_PARO_DY),(NAMES_PARO(3),       282
     *NPARO_DY)                                                           282
      EQUIVALENCE (COMMEN_PARO(3),CPARO_DY),(MAP_PARO(1,3),MPARO_DY)      282
      CHARACTER CPARO_DY*40/' '/                                          282
      CHARACTER NPARO_DY*12/'R.DY'/                                       282
      INTEGER MPARO_DY(2)/1,1/                                            282
      REAL RE_PARO_DY                                                     282
C -                                                                       282
      REAL PARO_DZ                                                        282
      EQUIVALENCE (BANK_PARO(6),PARO_DZ,RE_PARO_DZ),(NAMES_PARO(4),       282
     *NPARO_DZ)                                                           282
      EQUIVALENCE (COMMEN_PARO(4),CPARO_DZ),(MAP_PARO(1,4),MPARO_DZ)      282
      CHARACTER CPARO_DZ*40/' '/                                          282
      CHARACTER NPARO_DZ*12/'R.DZ'/                                       282
      INTEGER MPARO_DZ(2)/1,1/                                            282
      REAL RE_PARO_DZ                                                     282
C -                                                                       282
      REAL PARO_MULT                                                      282
      EQUIVALENCE (BANK_PARO(7),PARO_MULT,RE_PARO_MULT),(NAMES_PARO(5),   282
     *NPARO_MULT)                                                         282
      EQUIVALENCE (COMMEN_PARO(5),CPARO_MULT),(MAP_PARO(1,5),             282
     *MPARO_MULT)                                                         282
      CHARACTER CPARO_MULT*40/' '/                                        282
      CHARACTER NPARO_MULT*12/'R.MULT'/                                   282
      INTEGER MPARO_MULT(2)/1,1/                                          282
      REAL RE_PARO_MULT                                                   282
C -                                                                       282
      INTEGER PARO_LAD(24)                                                282
      EQUIVALENCE (BANK_PARO(8),PARO_LAD,RE_PARO_LAD),(NAMES_PARO(6),     282
     *NPARO_LAD)                                                          282
      EQUIVALENCE (COMMEN_PARO(6),CPARO_LAD),(MAP_PARO(1,6),MPARO_LAD)    282
      CHARACTER CPARO_LAD*40/' '/                                         282
      CHARACTER NPARO_LAD*12/'I.LAD'/                                     282
      INTEGER MPARO_LAD(2)/24,1/                                          282
      REAL RE_PARO_LAD                                                    282
C -                                                                       282
      INTEGER LEN_PARO,LLEN_PARO,LINK_PARO                                282
      PARAMETER (LEN_PARO=31)                                             282
      PARAMETER (LLEN_PARO=6)                                             282
      INTEGER FLAG_PARO/0/,MAP_PARO(2,LLEN_PARO),IDNUM_PARO(10)           282
      CHARACTER NAMES_PARO(LLEN_PARO)*12,COMMEN_PARO(LLEN_PARO)*40        282
      DATA IDNUM_PARO/10*0/                                               282
      REAL BANK_PARO(LEN_PARO)                                            282
      CHARACTER*60 BPATH_PARO/' '/                                        282
      SAVE FLAG_PARO,MAP_PARO,IDNUM_PARO,NAMES_PARO,COMMEN_PARO,          282
     *BPATH_PARO                                                          282
      DATA LINK_PARO/0/,BANK_PARO/LEN_PARO*0/                             282
C *                                                                       282
C -                                                                       283
      INTEGER PARA_VERSION                                                283
      EQUIVALENCE (BANK_PARA(3),PARA_VERSION,RE_PARA_VERSION),            283
     *(NAMES_PARA(1),NPARA_VERSION)                                       283
      EQUIVALENCE (COMMEN_PARA(1),CPARA_VERSION),(MAP_PARA(1,1),          283
     *MPARA_VERSION)                                                      283
      CHARACTER CPARA_VERSION*40/' '/                                     283
      CHARACTER NPARA_VERSION*12/'I.VERSION'/                             283
      INTEGER MPARA_VERSION(2)/1,1/                                       283
      REAL RE_PARA_VERSION                                                283
C -                                                                       283
      REAL PARA_DA                                                        283
      EQUIVALENCE (BANK_PARA(4),PARA_DA,RE_PARA_DA),(NAMES_PARA(2),       283
     *NPARA_DA)                                                           283
      EQUIVALENCE (COMMEN_PARA(2),CPARA_DA),(MAP_PARA(1,2),MPARA_DA)      283
      CHARACTER CPARA_DA*40/' '/                                          283
      CHARACTER NPARA_DA*12/'R.DA'/                                       283
      INTEGER MPARA_DA(2)/1,1/                                            283
      REAL RE_PARA_DA                                                     283
C -                                                                       283
      REAL PARA_DB                                                        283
      EQUIVALENCE (BANK_PARA(5),PARA_DB,RE_PARA_DB),(NAMES_PARA(3),       283
     *NPARA_DB)                                                           283
      EQUIVALENCE (COMMEN_PARA(3),CPARA_DB),(MAP_PARA(1,3),MPARA_DB)      283
      CHARACTER CPARA_DB*40/' '/                                          283
      CHARACTER NPARA_DB*12/'R.DB'/                                       283
      INTEGER MPARA_DB(2)/1,1/                                            283
      REAL RE_PARA_DB                                                     283
C -                                                                       283
      REAL PARA_DG                                                        283
      EQUIVALENCE (BANK_PARA(6),PARA_DG,RE_PARA_DG),(NAMES_PARA(4),       283
     *NPARA_DG)                                                           283
      EQUIVALENCE (COMMEN_PARA(4),CPARA_DG),(MAP_PARA(1,4),MPARA_DG)      283
      CHARACTER CPARA_DG*40/' '/                                          283
      CHARACTER NPARA_DG*12/'R.DG'/                                       283
      INTEGER MPARA_DG(2)/1,1/                                            283
      REAL RE_PARA_DG                                                     283
C -                                                                       283
      REAL PARA_MULT                                                      283
      EQUIVALENCE (BANK_PARA(7),PARA_MULT,RE_PARA_MULT),(NAMES_PARA(5),   283
     *NPARA_MULT)                                                         283
      EQUIVALENCE (COMMEN_PARA(5),CPARA_MULT),(MAP_PARA(1,5),             283
     *MPARA_MULT)                                                         283
      CHARACTER CPARA_MULT*40/' '/                                        283
      CHARACTER NPARA_MULT*12/'R.MULT'/                                   283
      INTEGER MPARA_MULT(2)/1,1/                                          283
      REAL RE_PARA_MULT                                                   283
C -                                                                       283
      INTEGER PARA_LAD(24)                                                283
      EQUIVALENCE (BANK_PARA(8),PARA_LAD,RE_PARA_LAD),(NAMES_PARA(6),     283
     *NPARA_LAD)                                                          283
      EQUIVALENCE (COMMEN_PARA(6),CPARA_LAD),(MAP_PARA(1,6),MPARA_LAD)    283
      CHARACTER CPARA_LAD*40/' '/                                         283
      CHARACTER NPARA_LAD*12/'I.LAD'/                                     283
      INTEGER MPARA_LAD(2)/24,1/                                          283
      REAL RE_PARA_LAD                                                    283
C -                                                                       283
      INTEGER LEN_PARA,LLEN_PARA,LINK_PARA                                283
      PARAMETER (LEN_PARA=31)                                             283
      PARAMETER (LLEN_PARA=6)                                             283
      INTEGER FLAG_PARA/0/,MAP_PARA(2,LLEN_PARA),IDNUM_PARA(10)           283
      CHARACTER NAMES_PARA(LLEN_PARA)*12,COMMEN_PARA(LLEN_PARA)*40        283
      DATA IDNUM_PARA/10*0/                                               283
      REAL BANK_PARA(LEN_PARA)                                            283
      CHARACTER*60 BPATH_PARA/' '/                                        283
      SAVE FLAG_PARA,MAP_PARA,IDNUM_PARA,NAMES_PARA,COMMEN_PARA,          283
     *BPATH_PARA                                                          283
      DATA LINK_PARA/0/,BANK_PARA/LEN_PARA*0/                             283
C *                                                                       283
C *                                                                       284
      CALL AGSBEGM(MODULE,IPRIN)                                          284
      IF (IPRIN.LT.0) RETURN                                              284
C Filling bank PARO - DEFAULT                                             284
      BANK_TITLE='DEFAULT'                                                284
      BPATH_PARO='PARO*'                                                  284
      IDNUM_PARO(1)=0                                                     285
C NONE                                                                    285
      PARO_VERSION=0                                                      285
      CPARO_VERSION='NONE'                                                285
C DEFAULT : NO SHIFT                                                      286
      PARO_DX=0                                                           286
      CPARO_DX='DEFAULT : NO SHIFT'                                       286
C DEFAULT : NO SHIFT                                                      287
      PARO_DY=0                                                           287
      CPARO_DY='DEFAULT : NO SHIFT'                                       287
C DEFAULT : NO SHIFT                                                      288
      PARO_DZ=0                                                           288
      CPARO_DZ='DEFAULT : NO SHIFT'                                       288
C DEFAULT : NO SHIFT                                                      289
      PARO_MULT=0                                                         289
      CPARO_MULT='DEFAULT : NO SHIFT'                                     289
C 1..24                                                                   291
      PARO_LAD(1)=0                                                       291
      PARO_LAD(2)=0                                                       291
      PARO_LAD(3)=0                                                       291
      PARO_LAD(4)=0                                                       291
      PARO_LAD(5)=0                                                       291
      PARO_LAD(6)=0                                                       291
      PARO_LAD(7)=0                                                       291
      PARO_LAD(8)=0                                                       291
      PARO_LAD(9)=0                                                       291
      PARO_LAD(10)=0                                                      291
      PARO_LAD(11)=0                                                      291
      PARO_LAD(12)=0                                                      291
      PARO_LAD(13)=0                                                      291
      PARO_LAD(14)=0                                                      291
      PARO_LAD(15)=0                                                      291
      PARO_LAD(16)=0                                                      291
      PARO_LAD(17)=0                                                      291
      PARO_LAD(18)=0                                                      291
      PARO_LAD(19)=0                                                      291
      PARO_LAD(20)=0                                                      291
      PARO_LAD(21)=0                                                      291
      PARO_LAD(22)=0                                                      291
      PARO_LAD(23)=0                                                      291
      PARO_LAD(24)=0                                                      291
      CPARO_LAD='1..24'                                                   291
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    292
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           292
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   292
C *                                                                       292
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    292
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           292
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   292
C *                                                                       293
      CALL AGSBEGM(MODULE,IPRIN)                                          293
      IF (IPRIN.LT.0) RETURN                                              293
C Filling bank PARO - SHIFT                                               293
      BANK_TITLE='SHIFT'                                                  293
      BPATH_PARO='PARO*'                                                  293
      IDNUM_PARO(1)=0                                                     294
C NONE                                                                    294
      PARO_VERSION=1                                                      294
      CPARO_VERSION='NONE'                                                294
C SHIFT FOR ALL LADDERS                                                   295
      PARO_DX=0                                                           295
      CPARO_DX='SHIFT FOR ALL LADDERS'                                    295
C DEFAULT = NO SHIFT                                                      296
      PARO_DY=0                                                           296
      CPARO_DY='DEFAULT = NO SHIFT'                                       296
C DEFAULT = NO SHIFT                                                      297
      PARO_DZ=1                                                           297
      CPARO_DZ='DEFAULT = NO SHIFT'                                       297
C MAX IS 1 CM                                                             298
      PARO_MULT=5                                                         298
      CPARO_MULT='MAX IS 1 CM'                                            298
C 1..24                                                                   300
      PARO_LAD(1)=1                                                       300
      PARO_LAD(2)=1                                                       300
      PARO_LAD(3)=1                                                       300
      PARO_LAD(4)=1                                                       300
      PARO_LAD(5)=1                                                       300
      PARO_LAD(6)=1                                                       300
      PARO_LAD(7)=1                                                       300
      PARO_LAD(8)=1                                                       300
      PARO_LAD(9)=1                                                       300
      PARO_LAD(10)=1                                                      300
      PARO_LAD(11)=1                                                      300
      PARO_LAD(12)=1                                                      300
      PARO_LAD(13)=1                                                      300
      PARO_LAD(14)=1                                                      300
      PARO_LAD(15)=1                                                      300
      PARO_LAD(16)=1                                                      300
      PARO_LAD(17)=1                                                      300
      PARO_LAD(18)=1                                                      300
      PARO_LAD(19)=1                                                      300
      PARO_LAD(20)=1                                                      300
      PARO_LAD(21)=1                                                      300
      PARO_LAD(22)=1                                                      300
      PARO_LAD(23)=1                                                      300
      PARO_LAD(24)=1                                                      300
      CPARO_LAD='1..24'                                                   300
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    301
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           301
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   301
C *                                                                       301
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    301
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           301
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   301
C *                                                                       302
      CALL AGSBEGM(MODULE,IPRIN)                                          302
      IF (IPRIN.LT.0) RETURN                                              302
C Filling bank PARA - DEFAULT                                             302
      BANK_TITLE='DEFAULT'                                                302
      BPATH_PARA='PARA*'                                                  302
      IDNUM_PARA(1)=0                                                     303
C NONE                                                                    303
      PARA_VERSION=0                                                      303
      CPARA_VERSION='NONE'                                                303
C DEFAULT = NO ROT                                                        304
      PARA_DA=0                                                           304
      CPARA_DA='DEFAULT = NO ROT'                                         304
C DEFAULT = NO ROT.                                                       305
      PARA_DB=0                                                           305
      CPARA_DB='DEFAULT = NO ROT.'                                        305
C DEFAULT = NO ROT.                                                       306
      PARA_DG=0                                                           306
      CPARA_DG='DEFAULT = NO ROT.'                                        306
C DEFAULT = NO ROT.                                                       307
      PARA_MULT=0                                                         307
      CPARA_MULT='DEFAULT = NO ROT.'                                      307
C 1..24                                                                   309
      PARA_LAD(1)=0                                                       309
      PARA_LAD(2)=0                                                       309
      PARA_LAD(3)=0                                                       309
      PARA_LAD(4)=0                                                       309
      PARA_LAD(5)=0                                                       309
      PARA_LAD(6)=0                                                       309
      PARA_LAD(7)=0                                                       309
      PARA_LAD(8)=0                                                       309
      PARA_LAD(9)=0                                                       309
      PARA_LAD(10)=0                                                      309
      PARA_LAD(11)=0                                                      309
      PARA_LAD(12)=0                                                      309
      PARA_LAD(13)=0                                                      309
      PARA_LAD(14)=0                                                      309
      PARA_LAD(15)=0                                                      309
      PARA_LAD(16)=0                                                      309
      PARA_LAD(17)=0                                                      309
      PARA_LAD(18)=0                                                      309
      PARA_LAD(19)=0                                                      309
      PARA_LAD(20)=0                                                      309
      PARA_LAD(21)=0                                                      309
      PARA_LAD(22)=0                                                      309
      PARA_LAD(23)=0                                                      309
      PARA_LAD(24)=0                                                      309
      CPARA_LAD='1..24'                                                   309
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    310
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           310
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   310
C *                                                                       310
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    310
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           310
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   310
C *                                                                       311
      CALL AGSBEGM(MODULE,IPRIN)                                          311
      IF (IPRIN.LT.0) RETURN                                              311
C Filling bank PARA - ROTATION GAMMA                                      311
      BANK_TITLE='ROTATION GAMMA'                                         311
      BPATH_PARA='PARA*'                                                  311
      IDNUM_PARA(1)=0                                                     312
C NONE                                                                    312
      PARA_VERSION=1                                                      312
      CPARA_VERSION='NONE'                                                312
C NO ROT.                                                                 313
      PARA_DA=0                                                           313
      CPARA_DA='NO ROT.'                                                  313
C NO ROT.                                                                 314
      PARA_DB=0                                                           314
      CPARA_DB='NO ROT.'                                                  314
C ROT. FOR ALL LADDERS                                                    315
      PARA_DG=1                                                           315
      CPARA_DG='ROT. FOR ALL LADDERS'                                     315
C MAX IS 1 DEGREES                                                        316
      PARA_MULT=1                                                         316
      CPARA_MULT='MAX IS 1 DEGREES'                                       316
C 1..24                                                                   318
      PARA_LAD(1)=1                                                       318
      PARA_LAD(2)=1                                                       318
      PARA_LAD(3)=1                                                       318
      PARA_LAD(4)=1                                                       318
      PARA_LAD(5)=1                                                       318
      PARA_LAD(6)=1                                                       318
      PARA_LAD(7)=1                                                       318
      PARA_LAD(8)=1                                                       318
      PARA_LAD(9)=1                                                       318
      PARA_LAD(10)=1                                                      318
      PARA_LAD(11)=1                                                      318
      PARA_LAD(12)=1                                                      318
      PARA_LAD(13)=1                                                      318
      PARA_LAD(14)=1                                                      318
      PARA_LAD(15)=1                                                      318
      PARA_LAD(16)=1                                                      318
      PARA_LAD(17)=1                                                      318
      PARA_LAD(18)=1                                                      318
      PARA_LAD(19)=1                                                      318
      PARA_LAD(20)=1                                                      318
      PARA_LAD(21)=1                                                      318
      PARA_LAD(22)=1                                                      318
      PARA_LAD(23)=1                                                      318
      PARA_LAD(24)=1                                                      318
      CPARA_LAD='1..24'                                                   318
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    319
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           319
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   319
C *                                                                       319
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    319
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           319
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   319
C use PARO bank                                                           320
      BPATH_PARO='PARO*'                                                  320
      IDNUM_PARO(1)=0                                                     320
      PARO_VERSION=0                                                      320
      CALL AGDGETP('ISTDGEO1','*',-999,'PARO',NPARO_VERSION,              320
     *RE_PARO_VERSION,BPATH_PARO,IDNUM_PARO,1,MAP_PARO,NAMES_PARO,        320
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   320
C use PARA bank                                                           321
      BPATH_PARA='PARA*'                                                  321
      IDNUM_PARA(1)=0                                                     321
      PARA_VERSION=0                                                      321
      CALL AGDGETP('ISTDGEO1','*',-999,'PARA',NPARA_VERSION,              321
     *RE_PARA_VERSION,BPATH_PARA,IDNUM_PARA,1,MAP_PARA,NAMES_PARA,        321
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   321
      XCTJPOS(1)= 1.4941                                                  322
      XCTJPOS(2)= 8.7384                                                  323
      XCTJPOS(3)=13.6413                                                  324
      XCTJPOS(4)=14.8890                                                  325
      XCTJPOS(5)=12.1472                                                  326
      XCTJPOS(6)= 6.1506                                                  327
      XCTJPOS(7)=-1.4941                                                  328
      XCTJPOS(8)=-8.7384                                                  329
      XCTJPOS(9)=-13.6413                                                 330
      XCTJPOS(10)=-14.889                                                 331
      XCTJPOS(11)=-12.1472                                                332
      XCTJPOS(12)=-6.1506                                                 333
      YCTJPOS(1)= 14.8890                                                 334
      YCTJPOS(2)= 12.1472                                                 335
      YCTJPOS(3)= 6.1506                                                  336
      YCTJPOS(4)= -1.4941                                                 337
      YCTJPOS(5)= -8.7384                                                 338
      YCTJPOS(6)= -13.6413                                                339
      YCTJPOS(7)= -14.8890                                                340
      YCTJPOS(8)= -12.1472                                                341
      YCTJPOS(9)= -6.1506                                                 342
      YCTJPOS(10)= 1.4941                                                 343
      YCTJPOS(11)= 8.7384                                                 344
      YCTJPOS(12)= 13.6413                                                345
C Geant MATERIAL : CARBONFOAM ===> A=12 Z=6 DENS=0.09 RADL=42.7 <===      346
      CALL AGSRESET                                                       346
      AG_A = 12                                                           346
      AG_Z = 6                                                            346
      AG_DENS = 0.09                                                      346
      AG_RADL = 42.7                                                      346
      AG_TITLE = 'CARBONFOAM'                                             346
      AG_EXNAME = 'MATERIAL'                                              346
      AG_PARLIST = 'A_Z_DENS_RADL'                                        346
      CALL AXMATERIAL                                                     346
C *                                                                       346
C Geant COMPONENT : O ===> A=16 Z=8 W=0.074 <===                          346
      CALL AGSRESET                                                       347
      AG_A = 16                                                           347
      AG_Z = 8                                                            347
      AG_W = 0.074                                                        347
      AG_TITLE = 'O'                                                      347
      AG_EXNAME = 'COMPONENT'                                             347
      AG_PARLIST = 'A_Z_W'                                                347
      CALL AXCOMPONENT                                                    347
C *                                                                       347
C Geant COMPONENT : C ===> A=12 Z=6 W=0.870 <===                          347
      CALL AGSRESET                                                       348
      AG_A = 12                                                           348
      AG_Z = 6                                                            348
      AG_W = 0.870                                                        348
      AG_TITLE = 'C'                                                      348
      AG_EXNAME = 'COMPONENT'                                             348
      AG_PARLIST = 'A_Z_W'                                                348
      CALL AXCOMPONENT                                                    348
C *                                                                       348
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           348
      CALL AGSRESET                                                       349
      AG_A = 1                                                            349
      AG_Z = 1                                                            349
      AG_W = 0.023                                                        349
      AG_TITLE = 'H'                                                      349
      AG_EXNAME = 'COMPONENT'                                             349
      AG_PARLIST = 'A_Z_W'                                                349
      CALL AXCOMPONENT                                                    349
C *                                                                       349
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.033 <===                      349
      CALL AGSRESET                                                       350
      AG_A = 35.5                                                         350
      AG_Z = 17                                                           350
      AG_W = 0.033                                                        350
      AG_TITLE = 'CL'                                                     350
      AG_EXNAME = 'COMPONENT'                                             350
      AG_PARLIST = 'A_Z_W'                                                350
      CALL AXCOMPONENT                                                    350
C *                                                                       350
C Geant MIXTURE : CFRPMIX ===> DENS=0.03 <===                             350
      CALL AGSRESET                                                       351
      AG_DENS = 0.03                                                      351
      AG_TITLE = 'CFRPMIX'                                                351
      AG_EXNAME = 'MIXTURE'                                               351
      AG_PARLIST = 'DENS'                                                 351
      CALL AXMIXTURE                                                      351
C *                                                                       351
C Geant COMPONENT : O ===> A=16 Z=8 W=0.074 <===                          351
      CALL AGSRESET                                                       352
      AG_A = 16                                                           352
      AG_Z = 8                                                            352
      AG_W = 0.074                                                        352
      AG_TITLE = 'O'                                                      352
      AG_EXNAME = 'COMPONENT'                                             352
      AG_PARLIST = 'A_Z_W'                                                352
      CALL AXCOMPONENT                                                    352
C *                                                                       352
C Geant COMPONENT : C ===> A=12 Z=6 W=0.870 <===                          352
      CALL AGSRESET                                                       353
      AG_A = 12                                                           353
      AG_Z = 6                                                            353
      AG_W = 0.870                                                        353
      AG_TITLE = 'C'                                                      353
      AG_EXNAME = 'COMPONENT'                                             353
      AG_PARLIST = 'A_Z_W'                                                353
      CALL AXCOMPONENT                                                    353
C *                                                                       353
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           353
      CALL AGSRESET                                                       354
      AG_A = 1                                                            354
      AG_Z = 1                                                            354
      AG_W = 0.023                                                        354
      AG_TITLE = 'H'                                                      354
      AG_EXNAME = 'COMPONENT'                                             354
      AG_PARLIST = 'A_Z_W'                                                354
      CALL AXCOMPONENT                                                    354
C *                                                                       354
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.033 <===                      354
      CALL AGSRESET                                                       355
      AG_A = 35.5                                                         355
      AG_Z = 17                                                           355
      AG_W = 0.033                                                        355
      AG_TITLE = 'CL'                                                     355
      AG_EXNAME = 'COMPONENT'                                             355
      AG_PARLIST = 'A_Z_W'                                                355
      CALL AXCOMPONENT                                                    355
C *                                                                       355
C Geant MIXTURE : CFRPMIX2 ===> DENS=1.60 <===                            355
      CALL AGSRESET                                                       356
      AG_DENS = 1.60                                                      356
      AG_TITLE = 'CFRPMIX2'                                               356
      AG_EXNAME = 'MIXTURE'                                               356
      AG_PARLIST = 'DENS'                                                 356
      CALL AXMIXTURE                                                      356
C *                                                                       356
C Geant COMPONENT : O ===> A=16 Z=8 W=0.117 <===                          356
      CALL AGSRESET                                                       357
      AG_A = 16                                                           357
      AG_Z = 8                                                            357
      AG_W = 0.117                                                        357
      AG_TITLE = 'O'                                                      357
      AG_EXNAME = 'COMPONENT'                                             357
      AG_PARLIST = 'A_Z_W'                                                357
      CALL AXCOMPONENT                                                    357
C *                                                                       357
C Geant COMPONENT : C ===> A=12 Z=6 W=0.854 <===                          357
      CALL AGSRESET                                                       358
      AG_A = 12                                                           358
      AG_Z = 6                                                            358
      AG_W = 0.854                                                        358
      AG_TITLE = 'C'                                                      358
      AG_EXNAME = 'COMPONENT'                                             358
      AG_PARLIST = 'A_Z_W'                                                358
      CALL AXCOMPONENT                                                    358
C *                                                                       358
C Geant COMPONENT : H ===> A=1 Z=1 W=0.029 <===                           358
      CALL AGSRESET                                                       359
      AG_A = 1                                                            359
      AG_Z = 1                                                            359
      AG_W = 0.029                                                        359
      AG_TITLE = 'H'                                                      359
      AG_EXNAME = 'COMPONENT'                                             359
      AG_PARLIST = 'A_Z_W'                                                359
      CALL AXCOMPONENT                                                    359
C *                                                                       359
C Geant MIXTURE : PEEKCF30 ===> DENS=1.41 <===                            359
      CALL AGSRESET                                                       360
      AG_DENS = 1.41                                                      360
      AG_TITLE = 'PEEKCF30'                                               360
      AG_EXNAME = 'MIXTURE'                                               360
      AG_PARLIST = 'DENS'                                                 360
      CALL AXMIXTURE                                                      360
C *                                                                       360
C Geant COMPONENT : O ===> A=16 Z=8 W=0.210 <===                          360
      CALL AGSRESET                                                       361
      AG_A = 16                                                           361
      AG_Z = 8                                                            361
      AG_W = 0.210                                                        361
      AG_TITLE = 'O'                                                      361
      AG_EXNAME = 'COMPONENT'                                             361
      AG_PARLIST = 'A_Z_W'                                                361
      CALL AXCOMPONENT                                                    361
C *                                                                       361
C Geant COMPONENT : C ===> A=12 Z=6 W=0.691 <===                          361
      CALL AGSRESET                                                       362
      AG_A = 12                                                           362
      AG_Z = 6                                                            362
      AG_W = 0.691                                                        362
      AG_TITLE = 'C'                                                      362
      AG_EXNAME = 'COMPONENT'                                             362
      AG_PARLIST = 'A_Z_W'                                                362
      CALL AXCOMPONENT                                                    362
C *                                                                       362
C Geant COMPONENT : H ===> A=1 Z=1 W=0.026 <===                           362
      CALL AGSRESET                                                       363
      AG_A = 1                                                            363
      AG_Z = 1                                                            363
      AG_W = 0.026                                                        363
      AG_TITLE = 'H'                                                      363
      AG_EXNAME = 'COMPONENT'                                             363
      AG_PARLIST = 'A_Z_W'                                                363
      CALL AXCOMPONENT                                                    363
C *                                                                       363
C Geant COMPONENT : N ===> A=14 Z=7 W=0.073 <===                          363
      CALL AGSRESET                                                       364
      AG_A = 14                                                           364
      AG_Z = 7                                                            364
      AG_W = 0.073                                                        364
      AG_TITLE = 'N'                                                      364
      AG_EXNAME = 'COMPONENT'                                             364
      AG_PARLIST = 'A_Z_W'                                                364
      CALL AXCOMPONENT                                                    364
C *                                                                       364
C Geant MIXTURE : KAPTON ===> DENS=1.42 <===                              364
      CALL AGSRESET                                                       365
      AG_DENS = 1.42                                                      365
      AG_TITLE = 'KAPTON'                                                 365
      AG_EXNAME = 'MIXTURE'                                               365
      AG_PARLIST = 'DENS'                                                 365
      CALL AXMIXTURE                                                      365
C *                                                                       365
C Geant COMPONENT : O ===> A=16 Z=8 W=0.061 <===                          365
      CALL AGSRESET                                                       366
      AG_A = 16                                                           366
      AG_Z = 8                                                            366
      AG_W = 0.061                                                        366
      AG_TITLE = 'O'                                                      366
      AG_EXNAME = 'COMPONENT'                                             366
      AG_PARLIST = 'A_Z_W'                                                366
      CALL AXCOMPONENT                                                    366
C *                                                                       366
C Geant COMPONENT : H ===> A=1 Z=1 W=0.019 <===                           366
      CALL AGSRESET                                                       367
      AG_A = 1                                                            367
      AG_Z = 1                                                            367
      AG_W = 0.019                                                        367
      AG_TITLE = 'H'                                                      367
      AG_EXNAME = 'COMPONENT'                                             367
      AG_PARLIST = 'A_Z_W'                                                367
      CALL AXCOMPONENT                                                    367
C *                                                                       367
C Geant COMPONENT : C ===> A=12 Z=6 W=0.272 <===                          367
      CALL AGSRESET                                                       368
      AG_A = 12                                                           368
      AG_Z = 6                                                            368
      AG_W = 0.272                                                        368
      AG_TITLE = 'C'                                                      368
      AG_EXNAME = 'COMPONENT'                                             368
      AG_PARLIST = 'A_Z_W'                                                368
      CALL AXCOMPONENT                                                    368
C *                                                                       368
C Geant COMPONENT : F ===> A=9 Z=19 W=0.648 <===                          368
      CALL AGSRESET                                                       369
      AG_A = 9                                                            369
      AG_Z = 19                                                           369
      AG_W = 0.648                                                        369
      AG_TITLE = 'F'                                                      369
      AG_EXNAME = 'COMPONENT'                                             369
      AG_PARLIST = 'A_Z_W'                                                369
      CALL AXCOMPONENT                                                    369
C *                                                                       369
C Geant MIXTURE : NOVEC7200 ===> DENS=1.44 <===                           369
      CALL AGSRESET                                                       370
      AG_DENS = 1.44                                                      370
      AG_TITLE = 'NOVEC7200'                                              370
      AG_EXNAME = 'MIXTURE'                                               370
      AG_PARLIST = 'DENS'                                                 370
      CALL AXMIXTURE                                                      370
C *                                                                       370
C Geant COMPONENT : O ===> A=16 Z=8 W=0.3*2*16/60.+0.7*3*16/288. <===     370
      CALL AGSRESET                                                       371
      AG_A = 16                                                           371
      AG_Z = 8                                                            371
      AG_W = 0.3*2*16/60.+0.7*3*16/288.                                   371
      AG_TITLE = 'O'                                                      371
      AG_EXNAME = 'COMPONENT'                                             371
      AG_PARLIST = 'A_Z_W'                                                371
      CALL AXCOMPONENT                                                    371
C *                                                                       371
C Geant COMPONENT : C ===> A=12 Z=6 W=0.7*19*12/288. <===                 371
      CALL AGSRESET                                                       372
      AG_A = 12                                                           372
      AG_Z = 6                                                            372
      AG_W = 0.7*19*12/288.                                               372
      AG_TITLE = 'C'                                                      372
      AG_EXNAME = 'COMPONENT'                                             372
      AG_PARLIST = 'A_Z_W'                                                372
      CALL AXCOMPONENT                                                    372
C *                                                                       372
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.3*1*28/60. <===              372
      CALL AGSRESET                                                       373
      AG_A = 28.08                                                        373
      AG_Z = 14                                                           373
      AG_W = 0.3*1*28/60.                                                 373
      AG_TITLE = 'SI'                                                     373
      AG_EXNAME = 'COMPONENT'                                             373
      AG_PARLIST = 'A_Z_W'                                                373
      CALL AXCOMPONENT                                                    373
C *                                                                       373
C Geant COMPONENT : H ===> A=1 Z=1 W=0.7*12*1/288. <===                   373
      CALL AGSRESET                                                       374
      AG_A = 1                                                            374
      AG_Z = 1                                                            374
      AG_W = 0.7*12*1/288.                                                374
      AG_TITLE = 'H'                                                      374
      AG_EXNAME = 'COMPONENT'                                             374
      AG_PARLIST = 'A_Z_W'                                                374
      CALL AXCOMPONENT                                                    374
C *                                                                       374
C Geant MIXTURE : PEEKGF30 ===> DENS=1.49 <===                            374
      CALL AGSRESET                                                       375
      AG_DENS = 1.49                                                      375
      AG_TITLE = 'PEEKGF30'                                               375
      AG_EXNAME = 'MIXTURE'                                               375
      AG_PARLIST = 'DENS'                                                 375
      CALL AXMIXTURE                                                      375
C *                                                                       375
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             375
      CALL AGSRESET                                                       376
      AG_A = 28.08                                                        376
      AG_Z = 14                                                           376
      AG_W = 0.6*1*28./60.                                                376
      AG_TITLE = 'SI'                                                     376
      AG_EXNAME = 'COMPONENT'                                             376
      AG_PARLIST = 'A_Z_W'                                                376
      CALL AXCOMPONENT                                                    376
C *                                                                       376
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60.+0.4*4*16./174. <===   376
      CALL AGSRESET                                                       377
      AG_A = 16                                                           377
      AG_Z = 8                                                            377
      AG_W = 0.6*2*16./60.+0.4*4*16./174.                                 377
      AG_TITLE = 'O'                                                      377
      AG_EXNAME = 'COMPONENT'                                             377
      AG_PARLIST = 'A_Z_W'                                                377
      CALL AXCOMPONENT                                                    377
C *                                                                       377
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 377
      CALL AGSRESET                                                       378
      AG_A = 12                                                           378
      AG_Z = 6                                                            378
      AG_W = 0.4*8*12./174.                                               378
      AG_TITLE = 'C'                                                      378
      AG_EXNAME = 'COMPONENT'                                             378
      AG_PARLIST = 'A_Z_W'                                                378
      CALL AXCOMPONENT                                                    378
C *                                                                       378
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  378
      CALL AGSRESET                                                       379
      AG_A = 1                                                            379
      AG_Z = 1                                                            379
      AG_W = 0.4*14*1./174.                                               379
      AG_TITLE = 'H'                                                      379
      AG_EXNAME = 'COMPONENT'                                             379
      AG_PARLIST = 'A_Z_W'                                                379
      CALL AXCOMPONENT                                                    379
C *                                                                       379
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  379
      CALL AGSRESET                                                       380
      AG_DENS = 1.7                                                       380
      AG_TITLE = 'G10'                                                    380
      AG_EXNAME = 'MIXTURE'                                               380
      AG_PARLIST = 'DENS'                                                 380
      CALL AXMIXTURE                                                      380
C *                                                                       380
C Geant COMPONENT : O ===> A=16 Z=8 W=2*16./100. <===                     380
      CALL AGSRESET                                                       381
      AG_A = 16                                                           381
      AG_Z = 8                                                            381
      AG_W = 2*16./100.                                                   381
      AG_TITLE = 'O'                                                      381
      AG_EXNAME = 'COMPONENT'                                             381
      AG_PARLIST = 'A_Z_W'                                                381
      CALL AXCOMPONENT                                                    381
C *                                                                       381
C Geant COMPONENT : C ===> A=12 Z=6 W=5*12./100. <===                     381
      CALL AGSRESET                                                       382
      AG_A = 12                                                           382
      AG_Z = 6                                                            382
      AG_W = 5*12./100.                                                   382
      AG_TITLE = 'C'                                                      382
      AG_EXNAME = 'COMPONENT'                                             382
      AG_PARLIST = 'A_Z_W'                                                382
      CALL AXCOMPONENT                                                    382
C *                                                                       382
C Geant COMPONENT : H ===> A=1 Z=1 W=8*1./100. <===                       382
      CALL AGSRESET                                                       383
      AG_A = 1                                                            383
      AG_Z = 1                                                            383
      AG_W = 8*1./100.                                                    383
      AG_TITLE = 'H'                                                      383
      AG_EXNAME = 'COMPONENT'                                             383
      AG_PARLIST = 'A_Z_W'                                                383
      CALL AXCOMPONENT                                                    383
C *                                                                       383
C Geant MIXTURE : DELRIN ===> DENS=1.2 <===                               383
      CALL AGSRESET                                                       384
      AG_DENS = 1.2                                                       384
      AG_TITLE = 'DELRIN'                                                 384
      AG_EXNAME = 'MIXTURE'                                               384
      AG_PARLIST = 'DENS'                                                 384
      CALL AXMIXTURE                                                      384
C *                                                                       384
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.191 <===                      384
      CALL AGSRESET                                                       385
      AG_A = 28.1                                                         385
      AG_Z = 14                                                           385
      AG_W = 0.191                                                        385
      AG_TITLE = 'SI'                                                     385
      AG_EXNAME = 'COMPONENT'                                             385
      AG_PARLIST = 'A_Z_W'                                                385
      CALL AXCOMPONENT                                                    385
C *                                                                       385
C Geant COMPONENT : O ===> A=16 Z=8 W=0.339 <===                          385
      CALL AGSRESET                                                       386
      AG_A = 16                                                           386
      AG_Z = 8                                                            386
      AG_W = 0.339                                                        386
      AG_TITLE = 'O'                                                      386
      AG_EXNAME = 'COMPONENT'                                             386
      AG_PARLIST = 'A_Z_W'                                                386
      CALL AXCOMPONENT                                                    386
C *                                                                       386
C Geant COMPONENT : C ===> A=12 Z=6 W=0.213 <===                          386
      CALL AGSRESET                                                       387
      AG_A = 12                                                           387
      AG_Z = 6                                                            387
      AG_W = 0.213                                                        387
      AG_TITLE = 'C'                                                      387
      AG_EXNAME = 'COMPONENT'                                             387
      AG_PARLIST = 'A_Z_W'                                                387
      CALL AXCOMPONENT                                                    387
C *                                                                       387
C Geant COMPONENT : H ===> A=1 Z=1 W=0.028 <===                           387
      CALL AGSRESET                                                       388
      AG_A = 1                                                            388
      AG_Z = 1                                                            388
      AG_W = 0.028                                                        388
      AG_TITLE = 'H'                                                      388
      AG_EXNAME = 'COMPONENT'                                             388
      AG_PARLIST = 'A_Z_W'                                                388
      CALL AXCOMPONENT                                                    388
C *                                                                       388
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.009 <===                      388
      CALL AGSRESET                                                       389
      AG_A = 35.5                                                         389
      AG_Z = 17                                                           389
      AG_W = 0.009                                                        389
      AG_TITLE = 'CL'                                                     389
      AG_EXNAME = 'COMPONENT'                                             389
      AG_PARLIST = 'A_Z_W'                                                389
      CALL AXCOMPONENT                                                    389
C *                                                                       389
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.122 <===                      389
      CALL AGSRESET                                                       390
      AG_A = 63.5                                                         390
      AG_Z = 29                                                           390
      AG_W = 0.122                                                        390
      AG_TITLE = 'CU'                                                     390
      AG_EXNAME = 'COMPONENT'                                             390
      AG_PARLIST = 'A_Z_W'                                                390
      CALL AXCOMPONENT                                                    390
C *                                                                       390
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.039 <===                       390
      CALL AGSRESET                                                       391
      AG_A = 207                                                          391
      AG_Z = 82                                                           391
      AG_W = 0.039                                                        391
      AG_TITLE = 'PB'                                                     391
      AG_EXNAME = 'COMPONENT'                                             391
      AG_PARLIST = 'A_Z_W'                                                391
      CALL AXCOMPONENT                                                    391
C *                                                                       391
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.059 <===                     391
      CALL AGSRESET                                                       392
      AG_A = 118.7                                                        392
      AG_Z = 50                                                           392
      AG_W = 0.059                                                        392
      AG_TITLE = 'SN'                                                     392
      AG_EXNAME = 'COMPONENT'                                             392
      AG_PARLIST = 'A_Z_W'                                                392
      CALL AXCOMPONENT                                                    392
C *                                                                       392
C Geant MIXTURE : APVMIX ===> DENS=2.535 <===                             392
      CALL AGSRESET                                                       393
      AG_DENS = 2.535                                                     393
      AG_TITLE = 'APVMIX'                                                 393
      AG_EXNAME = 'MIXTURE'                                               393
      AG_PARLIST = 'DENS'                                                 393
      CALL AXMIXTURE                                                      393
C *                                                                       393
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.856 <===                      393
      CALL AGSRESET                                                       394
      AG_A = 63.5                                                         394
      AG_Z = 29                                                           394
      AG_W = 0.856                                                        394
      AG_TITLE = 'CU'                                                     394
      AG_EXNAME = 'COMPONENT'                                             394
      AG_PARLIST = 'A_Z_W'                                                394
      CALL AXCOMPONENT                                                    394
C *                                                                       394
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.063 <===                        394
      CALL AGSRESET                                                       395
      AG_A = 27                                                           395
      AG_Z = 13                                                           395
      AG_W = 0.063                                                        395
      AG_TITLE = 'AL'                                                     395
      AG_EXNAME = 'COMPONENT'                                             395
      AG_PARLIST = 'A_Z_W'                                                395
      CALL AXCOMPONENT                                                    395
C *                                                                       395
C Geant COMPONENT : O ===> A=16 Z=8 W=0.081*0.061 <===                    395
      CALL AGSRESET                                                       396
      AG_A = 16                                                           396
      AG_Z = 8                                                            396
      AG_W = 0.081*0.061                                                  396
      AG_TITLE = 'O'                                                      396
      AG_EXNAME = 'COMPONENT'                                             396
      AG_PARLIST = 'A_Z_W'                                                396
      CALL AXCOMPONENT                                                    396
C *                                                                       396
C Geant COMPONENT : H ===> A=1 Z=1 W=0.081*0.019 <===                     396
      CALL AGSRESET                                                       397
      AG_A = 1                                                            397
      AG_Z = 1                                                            397
      AG_W = 0.081*0.019                                                  397
      AG_TITLE = 'H'                                                      397
      AG_EXNAME = 'COMPONENT'                                             397
      AG_PARLIST = 'A_Z_W'                                                397
      CALL AXCOMPONENT                                                    397
C *                                                                       397
C Geant COMPONENT : C ===> A=12 Z=6 W=0.081*0.272 <===                    397
      CALL AGSRESET                                                       398
      AG_A = 12                                                           398
      AG_Z = 6                                                            398
      AG_W = 0.081*0.272                                                  398
      AG_TITLE = 'C'                                                      398
      AG_EXNAME = 'COMPONENT'                                             398
      AG_PARLIST = 'A_Z_W'                                                398
      CALL AXCOMPONENT                                                    398
C *                                                                       398
C Geant COMPONENT : F ===> A=9 Z=19 W=0.081*0.648 <===                    398
      CALL AGSRESET                                                       399
      AG_A = 9                                                            399
      AG_Z = 19                                                           399
      AG_W = 0.081*0.648                                                  399
      AG_TITLE = 'F'                                                      399
      AG_EXNAME = 'COMPONENT'                                             399
      AG_PARLIST = 'A_Z_W'                                                399
      CALL AXCOMPONENT                                                    399
C *                                                                       399
C Geant MIXTURE : CUALNOVECMIX ===> DENS=0.673 <===                       399
      CALL AGSRESET                                                       400
      AG_DENS = 0.673                                                     400
      AG_TITLE = 'CUALNOVECMIX'                                           400
      AG_EXNAME = 'MIXTURE'                                               400
      AG_PARLIST = 'DENS'                                                 400
      CALL AXMIXTURE                                                      400
C *                                                                       400
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.57 <===                         400
      CALL AGSRESET                                                       401
      AG_A = 27                                                           401
      AG_Z = 13                                                           401
      AG_W = 0.57                                                         401
      AG_TITLE = 'AL'                                                     401
      AG_EXNAME = 'COMPONENT'                                             401
      AG_PARLIST = 'A_Z_W'                                                401
      CALL AXCOMPONENT                                                    401
C *                                                                       401
C Geant COMPONENT : O ===> A=16 Z=8 W=0.43*0.061 <===                     401
      CALL AGSRESET                                                       402
      AG_A = 16                                                           402
      AG_Z = 8                                                            402
      AG_W = 0.43*0.061                                                   402
      AG_TITLE = 'O'                                                      402
      AG_EXNAME = 'COMPONENT'                                             402
      AG_PARLIST = 'A_Z_W'                                                402
      CALL AXCOMPONENT                                                    402
C *                                                                       402
C Geant COMPONENT : H ===> A=1 Z=1 W=0.43*0.019 <===                      402
      CALL AGSRESET                                                       403
      AG_A = 1                                                            403
      AG_Z = 1                                                            403
      AG_W = 0.43*0.019                                                   403
      AG_TITLE = 'H'                                                      403
      AG_EXNAME = 'COMPONENT'                                             403
      AG_PARLIST = 'A_Z_W'                                                403
      CALL AXCOMPONENT                                                    403
C *                                                                       403
C Geant COMPONENT : C ===> A=12 Z=6 W=0.43*0.272 <===                     403
      CALL AGSRESET                                                       404
      AG_A = 12                                                           404
      AG_Z = 6                                                            404
      AG_W = 0.43*0.272                                                   404
      AG_TITLE = 'C'                                                      404
      AG_EXNAME = 'COMPONENT'                                             404
      AG_PARLIST = 'A_Z_W'                                                404
      CALL AXCOMPONENT                                                    404
C *                                                                       404
C Geant COMPONENT : F ===> A=9 Z=19 W=0.43*0.648 <===                     404
      CALL AGSRESET                                                       405
      AG_A = 9                                                            405
      AG_Z = 19                                                           405
      AG_W = 0.43*0.648                                                   405
      AG_TITLE = 'F'                                                      405
      AG_EXNAME = 'COMPONENT'                                             405
      AG_PARLIST = 'A_Z_W'                                                405
      CALL AXCOMPONENT                                                    405
C *                                                                       405
C Geant MIXTURE : ALNOVECMIX ===> DENS=1.961 <===                         405
      CALL AGSRESET                                                       406
      AG_DENS = 1.961                                                     406
      AG_TITLE = 'ALNOVECMIX'                                             406
      AG_EXNAME = 'MIXTURE'                                               406
      AG_PARLIST = 'DENS'                                                 406
      CALL AXMIXTURE                                                      406
C *                                                                       406
      RADDEG = 3.14159265/180.0                                           406
      MODULERADIUS = 13.9254                                              408
      LADDER_THK = HALFLADTHK*2                                           409
      LADDER_WIDTH = HALFLADWDT*2                                         410
      LADDER_LENGTH = HALFLADLEN*2                                        411
      DEBUG = 0                                                           412
      CALL AGSBEGM(MODULE,IPRIN)                                          413
C Check IPRIN<0                                                           413
      IF (IPRIN.LT.0) RETURN                                              413
C Create ICCT :                                                           413
C Call To Local Procedure ICCT                                            413
      ASSIGN 5020 TO BLOCK_ICCT                                           413
      GO TO  5010                                                         413
5020  CONTINUE                                                            413
C *                                                                       413
C Geant POSITION : ICCT ===> MOTHER='IDSM' X=0.0 Y=0.0 Z=-53.0448-0.5*8.  414
C 7144 <===                                                               414
      CALL AGSRESET                                                       414
      AG_MOTHER = 'IDSM'                                                  414
      AG_X = 0.0                                                          414
      AG_Y = 0.0                                                          414
      AG_Z = -53.0448-0.5*8.7144                                          414
      AG_TITLE = 'ICCT'                                                   414
      AG_EXNAME = 'POSITION'                                              414
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         414
      CALL AXPOSITION                                                     414
C *                                                                       414
C *                                                                       414
      DO 5031 NCABLESET=1,24                                              415
         ANGLEPIT_A=(360.0/24)*(NCABLESET-1)-7.396                        416
         ANGLEPIT_B=(360.0/24)*(NCABLESET-1)-9.337                        417
         ANGLEPIT_C=(360.0/24)*(NCABLESET-1)-11.279                       418
         ANGLEPIT_T=(360.0/24)*(NCABLESET-1)-4.045                        419
         CALL AGSBEGM(MODULE,IPRIN)                                       420
C    Check IPRIN<0                                                        420
         IF (IPRIN.LT.0) RETURN                                           420
C    Create ICCA :                                                        420
C    Call To Local Procedure ICCA                                         420
         ASSIGN 5050 TO BLOCK_ICCA                                        420
         GO TO  5040                                                      420
5050     CONTINUE                                                         420
C    *                                                                    420
C    Geant POSITION : ICCA ===> MOTHER='IDSM' X=24.3728*SIN(ANGLEPIT_A*R  422
C    ADDEG) Y=24.3728*COS(ANGLEPIT_A*RADDEG) Z=-231.9392+0.5*170.18 KONL  422
C    Y='ONLY' <===                                                        422
         CALL AGSRESET                                                    422
         AG_MOTHER = 'IDSM'                                               422
         AG_X = 24.3728*SIN(ANGLEPIT_A*RADDEG)                            422
         AG_Y = 24.3728*COS(ANGLEPIT_A*RADDEG)                            422
         AG_Z = -231.9392+0.5*170.18                                      422
         AG_KONLY = 'ONLY'                                                422
         AG_TITLE = 'ICCA'                                                422
         AG_EXNAME = 'POSITION'                                           422
         AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                422
         CALL AXPOSITION                                                  422
C    *                                                                    422
C    Geant POSITION : ICCA ===> MOTHER='IDSM' X=24.3728*SIN(ANGLEPIT_B*R  422
C    ADDEG) Y=24.3728*COS(ANGLEPIT_B*RADDEG) Z=-231.9392+0.5*170.18 KONL  422
C    Y='ONLY' <===                                                        422
         CALL AGSRESET                                                    424
         AG_MOTHER = 'IDSM'                                               424
         AG_X = 24.3728*SIN(ANGLEPIT_B*RADDEG)                            424
         AG_Y = 24.3728*COS(ANGLEPIT_B*RADDEG)                            424
         AG_Z = -231.9392+0.5*170.18                                      424
         AG_KONLY = 'ONLY'                                                424
         AG_TITLE = 'ICCA'                                                424
         AG_EXNAME = 'POSITION'                                           424
         AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                424
         CALL AXPOSITION                                                  424
C    *                                                                    424
C    Geant POSITION : ICCA ===> MOTHER='IDSM' X=24.3728*SIN(ANGLEPIT_C*R  424
C    ADDEG) Y=24.3728*COS(ANGLEPIT_C*RADDEG) Z=-231.9392+0.5*170.18 KONL  424
C    Y='ONLY' <===                                                        424
         CALL AGSRESET                                                    426
         AG_MOTHER = 'IDSM'                                               426
         AG_X = 24.3728*SIN(ANGLEPIT_C*RADDEG)                            426
         AG_Y = 24.3728*COS(ANGLEPIT_C*RADDEG)                            426
         AG_Z = -231.9392+0.5*170.18                                      426
         AG_KONLY = 'ONLY'                                                426
         AG_TITLE = 'ICCA'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                426
         CALL AXPOSITION                                                  426
C    *                                                                    426
         CALL AGSBEGM(MODULE,IPRIN)                                       426
C    Check IPRIN<0                                                        427
         IF (IPRIN.LT.0) RETURN                                           427
C    Create ICCL :                                                        427
C    Call To Local Procedure ICCL                                         427
         ASSIGN 5070 TO BLOCK_ICCL                                        427
         GO TO  5060                                                      427
5070     CONTINUE                                                         427
C    *                                                                    427
C    Geant POSITION : ICCL ===> MOTHER='IDSM' X=24.3855*SIN(ANGLEPIT_T*R  429
C    ADDEG) Y=24.3855*COS(ANGLEPIT_T*RADDEG) Z=-231.9392+0.5*170.18 KONL  429
C    Y='ONLY' <===                                                        429
         CALL AGSRESET                                                    429
         AG_MOTHER = 'IDSM'                                               429
         AG_X = 24.3855*SIN(ANGLEPIT_T*RADDEG)                            429
         AG_Y = 24.3855*COS(ANGLEPIT_T*RADDEG)                            429
         AG_Z = -231.9392+0.5*170.18                                      429
         AG_KONLY = 'ONLY'                                                429
         AG_TITLE = 'ICCL'                                                429
         AG_EXNAME = 'POSITION'                                           429
         AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                429
         CALL AXPOSITION                                                  429
C    *                                                                    429
         CALL AGSBEGM(MODULE,IPRIN)                                       429
C    Check IPRIN<0                                                        430
         IF (IPRIN.LT.0) RETURN                                           430
C    Create ICLP :                                                        430
C    Call To Local Procedure ICLP                                         430
         ASSIGN 5090 TO BLOCK_ICLP                                        430
         GO TO  5080                                                      430
5090     CONTINUE                                                         430
C    *                                                                    430
C    Geant POSITION : ICLP ===> MOTHER='IDSM' X=24.3855*SIN(ANGLEPIT_T*R  432
C    ADDEG) Y=24.3855*COS(ANGLEPIT_T*RADDEG) Z=-231.9392+0.5*170.18 KONL  432
C    Y='ONLY' <===                                                        432
         CALL AGSRESET                                                    432
         AG_MOTHER = 'IDSM'                                               432
         AG_X = 24.3855*SIN(ANGLEPIT_T*RADDEG)                            432
         AG_Y = 24.3855*COS(ANGLEPIT_T*RADDEG)                            432
         AG_Z = -231.9392+0.5*170.18                                      432
         AG_KONLY = 'ONLY'                                                432
         AG_TITLE = 'ICLP'                                                432
         AG_EXNAME = 'POSITION'                                           432
         AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                432
         CALL AXPOSITION                                                  432
C    *                                                                    432
5031  CONTINUE                                                            433
5032  CONTINUE                                                            433
      CALL AGSBEGM(MODULE,IPRIN)                                          434
C Check IPRIN<0                                                           434
      IF (IPRIN.LT.0) RETURN                                              434
C Create IBMO :                                                           434
C Call To Local Procedure IBMO                                            434
      ASSIGN 5110 TO BLOCK_IBMO                                           434
      GO TO  5100                                                         434
5110  CONTINUE                                                            434
C *                                                                       434
C Geant POSITION : IBMO ===> MOTHER='IDSM' KONLY='ONLY' <===              435
      CALL AGSRESET                                                       435
      AG_MOTHER = 'IDSM'                                                  435
      AG_KONLY = 'ONLY'                                                   435
      AG_TITLE = 'IBMO'                                                   435
      AG_EXNAME = 'POSITION'                                              435
      AG_PARLIST = 'MOTHER_KONLY'                                         435
      CALL AXPOSITION                                                     435
C *                                                                       435
C ----------------------------------------------------------------------  435
C ------------ IBMO                                                       435
      CALL AGSENDM                                                        437
      RETURN                                                              437
C Begin Local Procedure IBMO                                              437
C IBMO MAIN VOLUME OF IST DETECTOR                                        437
5100  CONTINUE                                                            437
         AG_TITLE='IBMO'                                                  437
         AG_MODULE=MODULE                                                 437
         AG_MARK='V00'                                                    437
         CALL AGSPUSH                                                     437
         AG_MODULE=MODULE                                                 437
C    *                                                                    437
C    Geant MATERIAL : AIR ===> inherited arguments used <===              438
         CALL AGSRESET                                                    438
         AG_TITLE = 'AIR'                                                 438
         AG_EXNAME = 'MATERIAL'                                           438
         AG_PARLIST = 'NONE'                                              438
         CALL AXMATERIAL                                                  438
C    *                                                                    438
C    Geant ATTRIBUTE : IBMO ===> SEEN=0 COLO=1 <===                       438
         CALL AGSRESET                                                    439
         AG_SEEN = 0                                                      439
         AG_COLO = 1                                                      439
         AG_TITLE = 'IBMO'                                                439
         AG_EXNAME = 'ATTRIBUTE'                                          439
         AG_PARLIST = 'SEEN_COLO'                                         439
         CALL AXATTRIBUTE                                                 439
C    *                                                                    439
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=4 ZI={-12.957-LADDER_LEN  439
C    GTH/2.0, -4.5-LADDER_LENGTH/2.0, -4.5-LADDER_LENGTH/2.0,LADDER_LENG  439
C    TH/2.0} RMN={IBMOINNERRADIUS+0.2, IBMOINNERRADIUS+0.2,IBMOINNERRADI  439
C    US, IBMOINNERRADIUS} RMX={IBMOOUTERRADIUS, IBMOOUTERRADIUS,IBMOOUTE  439
C    RRADIUS,IBMOOUTERRADIUS} <===                                        439
         CALL AGSRESET                                                    445
         AG_PHI1 = 0                                                      445
         AG_DPHI = 360                                                    445
         AG_NZ = 4                                                        445
         AG_ZI(1)=-12.957-LADDER_LENGTH/2.0                               445
         AG_ZI(2)=-4.5-LADDER_LENGTH/2.0                                  445
         AG_ZI(3)=-4.5-LADDER_LENGTH/2.0                                  445
         AG_ZI(4)=LADDER_LENGTH/2.0                                       445
         AG_RMN(1)=IBMOINNERRADIUS+0.2                                    445
         AG_RMN(2)=IBMOINNERRADIUS+0.2                                    445
         AG_RMN(3)=IBMOINNERRADIUS                                        445
         AG_RMN(4)=IBMOINNERRADIUS                                        445
         AG_RMX(1)=IBMOOUTERRADIUS                                        445
         AG_RMX(2)=IBMOOUTERRADIUS                                        445
         AG_RMX(3)=IBMOOUTERRADIUS                                        445
         AG_RMX(4)=IBMOOUTERRADIUS                                        445
         AG_TITLE = 'PCON'                                                445
         AG_EXNAME = 'SHAPE'                                              445
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           445
         CALL AXSHAPE                                                     445
C    *                                                                    445
C    *                                                                    445
         NL=1                                                             446
            GO TO 5123                                                    446
5121        NL=NL+1                                                       446
5123        IF(NL-(NLADDER).GT.0) GO TO 5122                              446
            AANGLE=(360.0/NLADDER)*(NL-1) + ANGLEOFFSET                   447
C       Check DEBUG==1                                                    448
            IF (DEBUG.EQ.1) THEN                                          448
            WRITE(LOUT,5130)PARO_DX*PARO_MULT                             448
5130        FORMAT('shift in X =', F6.4)                                  448
            WRITE(LOUT,5140)PARO_DY*PARO_MULT                             449
5140        FORMAT('shift in Y =', F6.4)                                  449
            WRITE(LOUT,5150)PARO_DZ*PARO_MULT                             450
5150        FORMAT('shift in Z =', F6.4)                                  450
            ENDIF                                                         452
            XPOS=0.0 + (PARO_DX*PARO_MULT)*PARO_LAD(NL)                   453
            YPOS=0.0 + (PARO_DY*PARO_MULT)*PARO_LAD(NL)                   454
            ZPOS=0.0 + (PARO_DZ*PARO_MULT)*PARO_LAD(NL)                   455
C       Check DEBUG==1                                                    456
            IF (DEBUG.EQ.1) THEN                                          456
            WRITE(LOUT,5160)AANGLE                                        456
5160        FORMAT('aangle=', F8.5)                                       456
            WRITE(LOUT,5170)XPOS                                          457
5170        FORMAT('x=', F8.5)                                            457
            WRITE(LOUT,5180)YPOS                                          458
5180        FORMAT('y=', F8.5)                                            458
            WRITE(LOUT,5190)ZPOS                                          459
5190        FORMAT('z=', F8.5)                                            459
            ENDIF                                                         461
C       Create IBAM :                                                     462
C       Call To Local Procedure IBAM                                      462
            ASSIGN 5210 TO BLOCK_IBAM                                     462
            GO TO  5200                                                   462
5210        CONTINUE                                                      462
C       *                                                                 462
C       Check DEBUG==1                                                    463
            IF (DEBUG.EQ.1) THEN                                          463
            WRITE(LOUT,5220)PARA_DA*PARA_MULT                             463
5220        FORMAT('rot along X =', F6.4)                                 463
            WRITE(LOUT,5230)PARA_DB*PARA_MULT                             464
5230        FORMAT('rot along Y =', F6.4)                                 464
            WRITE(LOUT,5240)PARA_DG*PARA_MULT                             465
5240        FORMAT('rot along Z =', F6.4)                                 465
            ENDIF                                                         467
C       Geant POSITION : IBAM ===> MOTHER='IBMO' X=XPOS Y=YPOS Z=ZPOS-4.  471
C       4929 ALPHAZ=(-1*AANGLE)-1*(PARA_DG*PARA_MULT*PARA_LAD(NL)) ALPHA  471
C       X=-1*(PARA_DA*PARA_MULT)*PARA_LAD(NL) ALPHAY=-1*(PARA_DB*PARA_MU  471
C       LT)*PARA_LAD(NL) KONLY='MANY' <===                                471
            CALL AGSRESET                                                 471
            AG_MOTHER = 'IBMO'                                            471
            AG_X = XPOS                                                   471
            AG_Y = YPOS                                                   471
            AG_Z = ZPOS-4.4929                                            471
            AG_ALPHAN=AG_ALPHAN+1                                         471
            AG_ALPHAK(AG_ALPHAN)=3                                        471
            AG_ALPHAL(AG_ALPHAN)=                                         471
     *      (-1*AANGLE)-1*(PARA_DG*PARA_MULT*PARA_LAD(NL))                471
            AG_ALPHAN=AG_ALPHAN+1                                         471
            AG_ALPHAK(AG_ALPHAN)=1                                        471
            AG_ALPHAL(AG_ALPHAN)=-1*(PARA_DA*PARA_MULT)*PARA_LAD(NL)      471
            AG_ALPHAN=AG_ALPHAN+1                                         471
            AG_ALPHAK(AG_ALPHAN)=2                                        471
            AG_ALPHAL(AG_ALPHAN)=-1*(PARA_DB*PARA_MULT)*PARA_LAD(NL)      471
            AG_KONLY = 'MANY'                                             471
            AG_TITLE = 'IBAM'                                             471
            AG_EXNAME = 'POSITION'                                        471
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_ALPHAX_ALPHAY_KONLY'        471
            CALL AXPOSITION                                               471
C       *                                                                 471
         GO TO 5121                                                       472
5122     CONTINUE                                                         472
C    *                                                                    473
         NCTJ=1                                                           473
            GO TO 5253                                                    473
5251        NCTJ=NCTJ+1                                                   473
5253        IF(NCTJ-(12).GT.0) GO TO 5252                                 473
            BANGLE=(360.0/12.0)*(NCTJ-1)+5.73                             474
C       Create IBCJ :                                                     475
C       Call To Local Procedure IBCJ                                      475
            ASSIGN 5270 TO BLOCK_IBCJ                                     475
            GO TO  5260                                                   475
5270        CONTINUE                                                      475
C       *                                                                 475
C       Geant POSITION : IBCJ ===> MOTHER='IBMO' X=XCTJPOS(NCTJ) Y=YCTJP  477
C       OS(NCTJ) Z=ZPOS+35.5965+0.5*11.4588 ALPHAZ=-1.0*BANGLE KONLY='MA  477
C    *NY' <===                                                            477
            CALL AGSRESET                                                 477
            AG_MOTHER = 'IBMO'                                            477
            AG_X = XCTJPOS(NCTJ)                                          477
            AG_Y = YCTJPOS(NCTJ)                                          477
            AG_Z = ZPOS+35.5965+0.5*11.4588                               477
            AG_ALPHAN=AG_ALPHAN+1                                         477
            AG_ALPHAK(AG_ALPHAN)=3                                        477
            AG_ALPHAL(AG_ALPHAN)=-1.0*BANGLE                              477
            AG_KONLY = 'MANY'                                             477
            AG_TITLE = 'IBCJ'                                             477
            AG_EXNAME = 'POSITION'                                        477
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      477
            CALL AXPOSITION                                               477
C       *                                                                 477
         GO TO 5251                                                       478
5252     CONTINUE                                                         478
         RADIUS_A=16.3832                                                 479
         RADIUS_B=16.8956                                                 480
         RADIUS_C=17.2478                                                 481
         RADIUS_T=15.0929                                                 482
         TILT_A=0.8824                                                    483
         TILT_B= - 3.5681                                                 484
         TILT_C= - 6.0912                                                 485
         TILT_T= - 1.7697                                                 486
C    *                                                                    487
         NCABLESET=1                                                      487
            GO TO 5283                                                    487
5281        NCABLESET=NCABLESET+1                                         487
5283        IF(NCABLESET-(24).GT.0) GO TO 5282                            487
            ANGLE_A=(360.0/24)*(NCABLESET-1)+TILT_A                       488
            ANGLE_B=(360.0/24)*(NCABLESET-1)+TILT_B                       489
            ANGLE_C=(360.0/24)*(NCABLESET-1)+TILT_C                       490
            ANGLE_T=(360.0/24)*(NCABLESET-1)+TILT_T                       491
C       Create ISCA :                                                     492
C       Call To Local Procedure ISCA                                      492
            ASSIGN 5300 TO BLOCK_ISCA                                     492
            GO TO  5290                                                   492
5300        CONTINUE                                                      492
C       *                                                                 492
C       Geant POSITION : ISCA ===> MOTHER='IBMO' X=RADIUS_A*SIN(ANGLE_A*  495
C       RADDEG) Y=RADIUS_A*COS(ANGLE_A*RADDEG) Z=-34.7832-0.5*18.2616 AL  495
C       PHAZ=-1.0*ANGLE_A KONLY='ONLY' <===                               495
            CALL AGSRESET                                                 495
            AG_MOTHER = 'IBMO'                                            495
            AG_X = RADIUS_A*SIN(ANGLE_A*RADDEG)                           495
            AG_Y = RADIUS_A*COS(ANGLE_A*RADDEG)                           495
            AG_Z = -34.7832-0.5*18.2616                                   495
            AG_ALPHAN=AG_ALPHAN+1                                         495
            AG_ALPHAK(AG_ALPHAN)=3                                        495
            AG_ALPHAL(AG_ALPHAN)=-1.0*ANGLE_A                             495
            AG_KONLY = 'ONLY'                                             495
            AG_TITLE = 'ISCA'                                             495
            AG_EXNAME = 'POSITION'                                        495
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      495
            CALL AXPOSITION                                               495
C       *                                                                 495
C       Create ISCB :                                                     496
C       Call To Local Procedure ISCB                                      496
            ASSIGN 5320 TO BLOCK_ISCB                                     496
            GO TO  5310                                                   496
5320        CONTINUE                                                      496
C       *                                                                 496
C       Geant POSITION : ISCB ===> MOTHER='IBMO' X=RADIUS_B*SIN(ANGLE_B*  499
C       RADDEG) Y=RADIUS_B*COS(ANGLE_B*RADDEG) Z=-32.1233-0.5*20.9215 AL  499
C       PHAZ=-1.0*ANGLE_B KONLY='ONLY' <===                               499
            CALL AGSRESET                                                 499
            AG_MOTHER = 'IBMO'                                            499
            AG_X = RADIUS_B*SIN(ANGLE_B*RADDEG)                           499
            AG_Y = RADIUS_B*COS(ANGLE_B*RADDEG)                           499
            AG_Z = -32.1233-0.5*20.9215                                   499
            AG_ALPHAN=AG_ALPHAN+1                                         499
            AG_ALPHAK(AG_ALPHAN)=3                                        499
            AG_ALPHAL(AG_ALPHAN)=-1.0*ANGLE_B                             499
            AG_KONLY = 'ONLY'                                             499
            AG_TITLE = 'ISCB'                                             499
            AG_EXNAME = 'POSITION'                                        499
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      499
            CALL AXPOSITION                                               499
C       *                                                                 499
C       Create ISCC :                                                     500
C       Call To Local Procedure ISCC                                      500
            ASSIGN 5340 TO BLOCK_ISCC                                     500
            GO TO  5330                                                   500
5340        CONTINUE                                                      500
C       *                                                                 500
C       Geant POSITION : ISCC ===> MOTHER='IBMO' X=RADIUS_C*SIN(ANGLE_C*  503
C       RADDEG) Y=RADIUS_C*COS(ANGLE_C*RADDEG) Z=-29.203-0.5*23.8418 ALP  503
C       HAZ=-1.0*ANGLE_C KONLY='ONLY' <===                                503
            CALL AGSRESET                                                 503
            AG_MOTHER = 'IBMO'                                            503
            AG_X = RADIUS_C*SIN(ANGLE_C*RADDEG)                           503
            AG_Y = RADIUS_C*COS(ANGLE_C*RADDEG)                           503
            AG_Z = -29.203-0.5*23.8418                                    503
            AG_ALPHAN=AG_ALPHAN+1                                         503
            AG_ALPHAK(AG_ALPHAN)=3                                        503
            AG_ALPHAL(AG_ALPHAN)=-1.0*ANGLE_C                             503
            AG_KONLY = 'ONLY'                                             503
            AG_TITLE = 'ISCC'                                             503
            AG_EXNAME = 'POSITION'                                        503
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      503
            CALL AXPOSITION                                               503
C       *                                                                 503
C       Create ISCT :                                                     504
C       Call To Local Procedure ISCT                                      504
            ASSIGN 5360 TO BLOCK_ISCT                                     504
            GO TO  5350                                                   504
5360        CONTINUE                                                      504
C       *                                                                 504
C       Geant POSITION : ISCT ===> MOTHER='IBMO' X=RADIUS_T*SIN(ANGLE_T*  507
C       RADDEG) Y=RADIUS_T*COS(ANGLE_T*RADDEG) Z=-44.5806-0.5*8.4642 ALP  507
C       HAZ=-1.0*ANGLE_T KONLY='ONLY' <===                                507
            CALL AGSRESET                                                 507
            AG_MOTHER = 'IBMO'                                            507
            AG_X = RADIUS_T*SIN(ANGLE_T*RADDEG)                           507
            AG_Y = RADIUS_T*COS(ANGLE_T*RADDEG)                           507
            AG_Z = -44.5806-0.5*8.4642                                    507
            AG_ALPHAN=AG_ALPHAN+1                                         507
            AG_ALPHAK(AG_ALPHAN)=3                                        507
            AG_ALPHAL(AG_ALPHAN)=-1.0*ANGLE_T                             507
            AG_KONLY = 'ONLY'                                             507
            AG_TITLE = 'ISCT'                                             507
            AG_EXNAME = 'POSITION'                                        507
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      507
            CALL AXPOSITION                                               507
C       *                                                                 507
         GO TO 5281                                                       508
5282     CONTINUE                                                         508
5101  CALL AGSPOPB                                                        509
      GO TO BLOCK_IBMO, (5100 ,5110)                                      509
C End of Local Procedure                                                  509
C ----------------------------------------------------------------------  510
C ------------ IBCJ                                                       510
C Begin Local Procedure IBCJ                                              511
C IBCJ IST COOLING LOOPS AND JOINTS                                       511
5260  CONTINUE                                                            511
         AG_TITLE='IBCJ'                                                  511
         AG_MODULE=MODULE                                                 511
         AG_MARK='V00'                                                    511
         CALL AGSPUSH                                                     511
         AG_MODULE=MODULE                                                 511
C    *                                                                    511
C    Geant MATERIAL : AIR ===> inherited arguments used <===              512
         CALL AGSRESET                                                    512
         AG_TITLE = 'AIR'                                                 512
         AG_EXNAME = 'MATERIAL'                                           512
         AG_PARLIST = 'NONE'                                              512
         CALL AXMATERIAL                                                  512
C    *                                                                    512
C    Geant ATTRIBUTE : IBCJ ===> SEEN=0 COLO=6 <===                       512
         CALL AGSRESET                                                    513
         AG_SEEN = 0                                                      513
         AG_COLO = 6                                                      513
         AG_TITLE = 'IBCJ'                                                513
         AG_EXNAME = 'ATTRIBUTE'                                          513
         AG_PARLIST = 'SEEN_COLO'                                         513
         CALL AXATTRIBUTE                                                 513
C    *                                                                    513
C    Geant SHAPE : BOX ===> DX=2.88855 DY=0.91655 DZ=5.7294 <===          513
         CALL AGSRESET                                                    514
         AG_DX = 2.88855                                                  514
         AG_DY = 0.91655                                                  514
         AG_DZ = 5.7294                                                   514
         AG_TITLE = 'BOX'                                                 514
         AG_EXNAME = 'SHAPE'                                              514
         AG_PARLIST = 'DX_DY_DZ'                                          514
         CALL AXSHAPE                                                     514
C    *                                                                    514
C    *                                                                    514
         NSEG=1                                                           515
            GO TO 5373                                                    515
5371        NSEG=NSEG+1                                                   515
5373        IF(NSEG-(20).GT.0) GO TO 5372                                 515
            TUBEANGLE=RADDEG*(2*NSEG-1)*180/(2.0*20)                      516
            XTUBEARC=-2.88855+0.5*1.8331+1.97-1.97*COS(TUBEANGLE)         517
            YTUBEARC=0.0                                                  518
            ZTUBEARC=-5.7294+2.722875+0.1778+6.35+1.97*SIN(TUBEANGLE)     519
C       Create ICJR :                                                     520
C       Call To Local Procedure ICJR                                      520
            ASSIGN 5390 TO BLOCK_ICJR                                     520
            GO TO  5380                                                   520
5390        CONTINUE                                                      520
C       *                                                                 520
C       Geant POSITION : ICJR ===> MOTHER='IBCJ' X=XTUBEARC Y=YTUBEARC Z  522
C       =ZTUBEARC ALPHAY=(2*NSEG-1)*180/(2.0*20) <===                     522
            CALL AGSRESET                                                 522
            AG_MOTHER = 'IBCJ'                                            522
            AG_X = XTUBEARC                                               522
            AG_Y = YTUBEARC                                               522
            AG_Z = ZTUBEARC                                               522
            AG_ALPHAN=AG_ALPHAN+1                                         522
            AG_ALPHAK(AG_ALPHAN)=2                                        522
            AG_ALPHAL(AG_ALPHAN)=(2*NSEG-1)*180/(2.0*20)                  522
            AG_TITLE = 'ICJR'                                             522
            AG_EXNAME = 'POSITION'                                        522
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAY'                            522
            CALL AXPOSITION                                               522
C       *                                                                 522
         GO TO 5371                                                       523
5372     CONTINUE                                                         523
C    Create ICJS :                                                        524
C    Call To Local Procedure ICJS                                         524
         ASSIGN 5410 TO BLOCK_ICJS                                        524
         GO TO  5400                                                      524
5410     CONTINUE                                                         524
C    *                                                                    524
C    Geant POSITION : ICJS ===> MOTHER='IBCJ' X=2.88855-0.5*1.8331 Y=0.0  526
C     Z=-5.7294+2.722875+0.1778+0.5*6.35 <===                             526
         CALL AGSRESET                                                    526
         AG_MOTHER = 'IBCJ'                                               526
         AG_X = 2.88855-0.5*1.8331                                        526
         AG_Y = 0.0                                                       526
         AG_Z = -5.7294+2.722875+0.1778+0.5*6.35                          526
         AG_TITLE = 'ICJS'                                                526
         AG_EXNAME = 'POSITION'                                           526
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      526
         CALL AXPOSITION                                                  526
C    *                                                                    526
C    Geant POSITION : ICJS ===> MOTHER='IBCJ' X=-2.88855+0.5*1.8331 Y=0.  526
C    0 Z=-5.7294+2.722875+0.1778+0.5*6.35 <===                            526
         CALL AGSRESET                                                    528
         AG_MOTHER = 'IBCJ'                                               528
         AG_X = -2.88855+0.5*1.8331                                       528
         AG_Y = 0.0                                                       528
         AG_Z = -5.7294+2.722875+0.1778+0.5*6.35                          528
         AG_TITLE = 'ICJS'                                                528
         AG_EXNAME = 'POSITION'                                           528
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      528
         CALL AXPOSITION                                                  528
C    *                                                                    528
C    Create ICJA :                                                        529
C    Call To Local Procedure ICJA                                         529
         ASSIGN 5430 TO BLOCK_ICJA                                        529
         GO TO  5420                                                      529
5430     CONTINUE                                                         529
C    *                                                                    529
C    Geant POSITION : ICJA ===> MOTHER='IBCJ' X=-2.88855+0.5*1.8331 Y=0.  530
C    0 Z=-5.7294+2.722875+0.5*0.1778 <===                                 530
         CALL AGSRESET                                                    530
         AG_MOTHER = 'IBCJ'                                               530
         AG_X = -2.88855+0.5*1.8331                                       530
         AG_Y = 0.0                                                       530
         AG_Z = -5.7294+2.722875+0.5*0.1778                               530
         AG_TITLE = 'ICJA'                                                530
         AG_EXNAME = 'POSITION'                                           530
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      530
         CALL AXPOSITION                                                  530
C    *                                                                    530
C    Geant POSITION : ICJA ===> MOTHER='IBCJ' X=2.88855-0.5*1.8331 Y=0.0  530
C     Z=-5.7294+2.722875+0.5*0.1778 <===                                  530
         CALL AGSRESET                                                    531
         AG_MOTHER = 'IBCJ'                                               531
         AG_X = 2.88855-0.5*1.8331                                        531
         AG_Y = 0.0                                                       531
         AG_Z = -5.7294+2.722875+0.5*0.1778                               531
         AG_TITLE = 'ICJA'                                                531
         AG_EXNAME = 'POSITION'                                           531
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      531
         CALL AXPOSITION                                                  531
C    *                                                                    531
C    Create ICJU :                                                        532
C    Call To Local Procedure ICJU                                         532
         ASSIGN 5450 TO BLOCK_ICJU                                        532
         GO TO  5440                                                      532
5450     CONTINUE                                                         532
C    *                                                                    532
C    Geant POSITION : ICJU ===> MOTHER='IBCJ' X=-2.88855+0.5*1.8331 Y=0.  533
C    0 Z=-5.7294+0.5*2.722875 <===                                        533
         CALL AGSRESET                                                    533
         AG_MOTHER = 'IBCJ'                                               533
         AG_X = -2.88855+0.5*1.8331                                       533
         AG_Y = 0.0                                                       533
         AG_Z = -5.7294+0.5*2.722875                                      533
         AG_TITLE = 'ICJU'                                                533
         AG_EXNAME = 'POSITION'                                           533
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      533
         CALL AXPOSITION                                                  533
C    *                                                                    533
C    Geant POSITION : ICJU ===> MOTHER='IBCJ' X=2.88855-0.5*1.8331 Y=0.0  533
C     Z=-5.7294+0.5*2.722875 <===                                         533
         CALL AGSRESET                                                    534
         AG_MOTHER = 'IBCJ'                                               534
         AG_X = 2.88855-0.5*1.8331                                        534
         AG_Y = 0.0                                                       534
         AG_Z = -5.7294+0.5*2.722875                                      534
         AG_TITLE = 'ICJU'                                                534
         AG_EXNAME = 'POSITION'                                           534
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      534
         CALL AXPOSITION                                                  534
C    *                                                                    534
C    Create ICLN :                                                        535
C    Call To Local Procedure ICLN                                         535
         ASSIGN 5470 TO BLOCK_ICLN                                        535
         GO TO  5460                                                      535
5470     CONTINUE                                                         535
C    *                                                                    535
C    Geant POSITION : ICLN ===> MOTHER='IBCJ' X=-2.88855+0.5*1.8331 Y=0.  537
C    0 Z=-5.7294+0.5*2.722875+0.5*0.1778+0.5*6.35 <===                    537
         CALL AGSRESET                                                    537
         AG_MOTHER = 'IBCJ'                                               537
         AG_X = -2.88855+0.5*1.8331                                       537
         AG_Y = 0.0                                                       537
         AG_Z = -5.7294+0.5*2.722875+0.5*0.1778+0.5*6.35                  537
         AG_TITLE = 'ICLN'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      537
         CALL AXPOSITION                                                  537
C    *                                                                    537
C    Geant POSITION : ICLN ===> MOTHER='IBCJ' X=2.88855-0.5*1.8331 Y=0.0  537
C     Z=-5.7294+0.5*2.722875+0.5*0.1778+0.5*6.35 <===                     537
         CALL AGSRESET                                                    539
         AG_MOTHER = 'IBCJ'                                               539
         AG_X = 2.88855-0.5*1.8331                                        539
         AG_Y = 0.0                                                       539
         AG_Z = -5.7294+0.5*2.722875+0.5*0.1778+0.5*6.35                  539
         AG_TITLE = 'ICLN'                                                539
         AG_EXNAME = 'POSITION'                                           539
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      539
         CALL AXPOSITION                                                  539
C    *                                                                    539
5261  CALL AGSPOPB                                                        540
      GO TO BLOCK_IBCJ, (5260 ,5270)                                      540
C End of Local Procedure                                                  540
C ----------------------------------------------------------------------  541
C ------------ ICJR                                                       541
C Begin Local Procedure ICJR                                              542
C ICJR COOLING LOOP ARC PART                                              542
5380  CONTINUE                                                            542
         AG_TITLE='ICJR'                                                  542
         AG_MODULE=MODULE                                                 542
         AG_MARK='V00'                                                    542
         CALL AGSPUSH                                                     542
         AG_MODULE=MODULE                                                 542
C    *                                                                    542
C    Geant ATTRIBUTE : ICJR ===> SEEN=1 COLO=4 <===                       543
         CALL AGSRESET                                                    543
         AG_SEEN = 1                                                      543
         AG_COLO = 4                                                      543
         AG_TITLE = 'ICJR'                                                543
         AG_EXNAME = 'ATTRIBUTE'                                          543
         AG_PARLIST = 'SEEN_COLO'                                         543
         CALL AXATTRIBUTE                                                 543
C    *                                                                    543
C    Geant MATERIAL : ALNOVECMIX ===> inherited arguments used <===       543
         CALL AGSRESET                                                    544
         AG_TITLE = 'ALNOVECMIX'                                          544
         AG_EXNAME = 'MATERIAL'                                           544
         AG_PARLIST = 'NONE'                                              544
         CALL AXMATERIAL                                                  544
C    *                                                                    544
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.238125 DZ=0.5*3.1415926*1.97/  544
C    20 <===                                                              544
         CALL AGSRESET                                                    545
         AG_RMIN = 0                                                      545
         AG_RMAX = 0.238125                                               545
         AG_DZ = 0.5*3.1415926*1.97/20                                    545
         AG_TITLE = 'TUBE'                                                545
         AG_EXNAME = 'SHAPE'                                              545
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      545
         CALL AXSHAPE                                                     545
C    *                                                                    545
5381  CALL AGSPOPB                                                        546
      GO TO BLOCK_ICJR, (5380 ,5390)                                      546
C End of Local Procedure                                                  546
C ----------------------------------------------------------------------  547
C ------------ ICJS                                                       547
C Begin Local Procedure ICJS                                              548
C ICJS COOLING LOOP TUBE PART RIGHT                                       548
5400  CONTINUE                                                            548
         AG_TITLE='ICJS'                                                  548
         AG_MODULE=MODULE                                                 548
         AG_MARK='V00'                                                    548
         CALL AGSPUSH                                                     548
         AG_MODULE=MODULE                                                 548
C    *                                                                    548
C    Geant ATTRIBUTE : ICJS ===> SEEN=1 COLO=4 <===                       549
         CALL AGSRESET                                                    549
         AG_SEEN = 1                                                      549
         AG_COLO = 4                                                      549
         AG_TITLE = 'ICJS'                                                549
         AG_EXNAME = 'ATTRIBUTE'                                          549
         AG_PARLIST = 'SEEN_COLO'                                         549
         CALL AXATTRIBUTE                                                 549
C    *                                                                    549
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        549
         CALL AGSRESET                                                    550
         AG_TITLE = 'ALUMINIUM'                                           550
         AG_EXNAME = 'MATERIAL'                                           550
         AG_PARLIST = 'NONE'                                              550
         CALL AXMATERIAL                                                  550
C    *                                                                    550
C    Geant SHAPE : TUBE ===> RMIN=0.18225 RMAX=0.238125 DZ=3.175 <===     550
         CALL AGSRESET                                                    551
         AG_RMIN = 0.18225                                                551
         AG_RMAX = 0.238125                                               551
         AG_DZ = 3.175                                                    551
         AG_TITLE = 'TUBE'                                                551
         AG_EXNAME = 'SHAPE'                                              551
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      551
         CALL AXSHAPE                                                     551
C    *                                                                    551
5401  CALL AGSPOPB                                                        552
      GO TO BLOCK_ICJS, (5400 ,5410)                                      552
C End of Local Procedure                                                  552
C ----------------------------------------------------------------------  553
C ------------ ICJA                                                       553
C Begin Local Procedure ICJA                                              554
C ICJA COOLING LOOP CONE PART LEFT                                        554
5420  CONTINUE                                                            554
         AG_TITLE='ICJA'                                                  554
         AG_MODULE=MODULE                                                 554
         AG_MARK='V00'                                                    554
         CALL AGSPUSH                                                     554
         AG_MODULE=MODULE                                                 554
C    *                                                                    554
C    Geant ATTRIBUTE : ICJA ===> SEEN=1 COLO=4 <===                       555
         CALL AGSRESET                                                    555
         AG_SEEN = 1                                                      555
         AG_COLO = 4                                                      555
         AG_TITLE = 'ICJA'                                                555
         AG_EXNAME = 'ATTRIBUTE'                                          555
         AG_PARLIST = 'SEEN_COLO'                                         555
         CALL AXATTRIBUTE                                                 555
C    *                                                                    555
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        555
         CALL AGSRESET                                                    556
         AG_TITLE = 'ALUMINIUM'                                           556
         AG_EXNAME = 'MATERIAL'                                           556
         AG_PARLIST = 'NONE'                                              556
         CALL AXMATERIAL                                                  556
C    *                                                                    556
C    Geant SHAPE : CONE ===> DZ=0.5*0.1778 RMN1=0.5*0.6325, RMX1=0.5*0.7  556
C    442 RMN2=0.5*0.3645 RMX2=0.5*0.47625 <===                            556
         CALL AGSRESET                                                    558
         AG_DZ = 0.5*0.1778                                               558
         AG_RMN1 = 0.5*0.6325                                             558
         AG_RMX1 = 0.5*0.7442                                             558
         AG_RMN2 = 0.5*0.3645                                             558
         AG_RMX2 = 0.5*0.47625                                            558
         AG_TITLE = 'CONE'                                                558
         AG_EXNAME = 'SHAPE'                                              558
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            558
         CALL AXSHAPE                                                     558
C    *                                                                    558
5421  CALL AGSPOPB                                                        559
      GO TO BLOCK_ICJA, (5420 ,5430)                                      559
C End of Local Procedure                                                  559
C ----------------------------------------------------------------------  560
C ------------ ICJU                                                       560
C Begin Local Procedure ICJU                                              561
C ICJU COOLING LOOP CONNECTOR LEFT                                        561
5440  CONTINUE                                                            561
         AG_TITLE='ICJU'                                                  561
         AG_MODULE=MODULE                                                 561
         AG_MARK='V00'                                                    561
         CALL AGSPUSH                                                     561
         AG_MODULE=MODULE                                                 561
C    *                                                                    561
C    Geant ATTRIBUTE : ICJU ===> SEEN=1 COLO=4 <===                       562
         CALL AGSRESET                                                    562
         AG_SEEN = 1                                                      562
         AG_COLO = 4                                                      562
         AG_TITLE = 'ICJU'                                                562
         AG_EXNAME = 'ATTRIBUTE'                                          562
         AG_PARLIST = 'SEEN_COLO'                                         562
         CALL AXATTRIBUTE                                                 562
C    *                                                                    562
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        562
         CALL AGSRESET                                                    563
         AG_TITLE = 'ALUMINIUM'                                           563
         AG_EXNAME = 'MATERIAL'                                           563
         AG_PARLIST = 'NONE'                                              563
         CALL AXMATERIAL                                                  563
C    *                                                                    563
C    Geant SHAPE : TUBE ===> RMIN=0.18225 RMAX=0.5*1.8331, DZ=0.5*2.7228  563
C    75 <===                                                              563
         CALL AGSRESET                                                    565
         AG_RMIN = 0.18225                                                565
         AG_RMAX = 0.5*1.8331                                             565
         AG_DZ = 0.5*2.722875                                             565
         AG_TITLE = 'TUBE'                                                565
         AG_EXNAME = 'SHAPE'                                              565
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      565
         CALL AXSHAPE                                                     565
C    *                                                                    565
5441  CALL AGSPOPB                                                        566
      GO TO BLOCK_ICJU, (5440 ,5450)                                      566
C End of Local Procedure                                                  566
C ----------------------------------------------------------------------  567
C ------------ ICLN                                                       567
C Begin Local Procedure ICLN                                              568
C ICLN COOLING LIQUID THROUGH ICJN, ICJA AND ICJU VOLUMES                 568
5460  CONTINUE                                                            568
         AG_TITLE='ICLN'                                                  568
         AG_MODULE=MODULE                                                 568
         AG_MARK='V00'                                                    568
         CALL AGSPUSH                                                     568
         AG_MODULE=MODULE                                                 568
C    *                                                                    568
C    Geant ATTRIBUTE : ICLN ===> SEEN=1 COLO=920 <===                     569
         CALL AGSRESET                                                    569
         AG_SEEN = 1                                                      569
         AG_COLO = 920                                                    569
         AG_TITLE = 'ICLN'                                                569
         AG_EXNAME = 'ATTRIBUTE'                                          569
         AG_PARLIST = 'SEEN_COLO'                                         569
         CALL AXATTRIBUTE                                                 569
C    *                                                                    569
C    Geant MATERIAL : NOVEC7200 ===> inherited arguments used <===        569
         CALL AGSRESET                                                    570
         AG_TITLE = 'NOVEC7200'                                           570
         AG_EXNAME = 'MATERIAL'                                           570
         AG_PARLIST = 'NONE'                                              570
         CALL AXMATERIAL                                                  570
C    *                                                                    570
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*0.3645 DZ=0.5*2.722875+0.5*  570
C    0.1778+0.5*6.35 <===                                                 570
         CALL AGSRESET                                                    571
         AG_RMIN = 0                                                      571
         AG_RMAX = 0.5*0.3645                                             571
         AG_DZ = 0.5*2.722875+0.5*0.1778+0.5*6.35                         571
         AG_TITLE = 'TUBE'                                                571
         AG_EXNAME = 'SHAPE'                                              571
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      571
         CALL AXSHAPE                                                     571
C    *                                                                    571
5461  CALL AGSPOPB                                                        572
      GO TO BLOCK_ICLN, (5460 ,5470)                                      572
C End of Local Procedure                                                  572
C ----------------------------------------------------------------------  573
C ------------ ISCA                                                       573
C Begin Local Procedure ISCA                                              574
C ISCA IST SHORT PURPLE CABLE A IN TRANSITION CONE                        574
5290  CONTINUE                                                            574
         AG_TITLE='ISCA'                                                  574
         AG_MODULE=MODULE                                                 574
         AG_MARK='V00'                                                    574
         CALL AGSPUSH                                                     574
         AG_MODULE=MODULE                                                 574
C    *                                                                    574
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           575
         CALL AGSRESET                                                    575
         AG_TITLE = 'COPPER'                                              575
         AG_EXNAME = 'MATERIAL'                                           575
         AG_PARLIST = 'NONE'                                              575
         CALL AXMATERIAL                                                  575
C    *                                                                    575
C    Geant ATTRIBUTE : ISCA ===> SEEN=1 COLO=629 <===                     575
         CALL AGSRESET                                                    576
         AG_SEEN = 1                                                      576
         AG_COLO = 629                                                    576
         AG_TITLE = 'ISCA'                                                576
         AG_EXNAME = 'ATTRIBUTE'                                          576
         AG_PARLIST = 'SEEN_COLO'                                         576
         CALL AXATTRIBUTE                                                 576
C    *                                                                    576
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*CABLEDIAMETER, DZ=0.5*18.26  576
C    16 <===                                                              576
         CALL AGSRESET                                                    578
         AG_RMIN = 0                                                      578
         AG_RMAX = 0.5*CABLEDIAMETER                                      578
         AG_DZ = 0.5*18.2616                                              578
         AG_TITLE = 'TUBE'                                                578
         AG_EXNAME = 'SHAPE'                                              578
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      578
         CALL AXSHAPE                                                     578
C    *                                                                    578
5291  CALL AGSPOPB                                                        579
      GO TO BLOCK_ISCA, (5290 ,5300)                                      579
C End of Local Procedure                                                  579
C ----------------------------------------------------------------------  580
C ------------ ISCB                                                       580
C Begin Local Procedure ISCB                                              581
C ISCB IST SHORT PURPLE CABLE B IN TRANSITION CONE                        581
5310  CONTINUE                                                            581
         AG_TITLE='ISCB'                                                  581
         AG_MODULE=MODULE                                                 581
         AG_MARK='V00'                                                    581
         CALL AGSPUSH                                                     581
         AG_MODULE=MODULE                                                 581
C    *                                                                    581
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           582
         CALL AGSRESET                                                    582
         AG_TITLE = 'COPPER'                                              582
         AG_EXNAME = 'MATERIAL'                                           582
         AG_PARLIST = 'NONE'                                              582
         CALL AXMATERIAL                                                  582
C    *                                                                    582
C    Geant ATTRIBUTE : ISCB ===> SEEN=1 COLO=629 <===                     582
         CALL AGSRESET                                                    583
         AG_SEEN = 1                                                      583
         AG_COLO = 629                                                    583
         AG_TITLE = 'ISCB'                                                583
         AG_EXNAME = 'ATTRIBUTE'                                          583
         AG_PARLIST = 'SEEN_COLO'                                         583
         CALL AXATTRIBUTE                                                 583
C    *                                                                    583
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*CABLEDIAMETER, DZ=0.5*20.92  583
C    15 <===                                                              583
         CALL AGSRESET                                                    585
         AG_RMIN = 0                                                      585
         AG_RMAX = 0.5*CABLEDIAMETER                                      585
         AG_DZ = 0.5*20.9215                                              585
         AG_TITLE = 'TUBE'                                                585
         AG_EXNAME = 'SHAPE'                                              585
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      585
         CALL AXSHAPE                                                     585
C    *                                                                    585
5311  CALL AGSPOPB                                                        586
      GO TO BLOCK_ISCB, (5310 ,5320)                                      586
C End of Local Procedure                                                  586
C ----------------------------------------------------------------------  587
C ------------ ISCC                                                       587
C Begin Local Procedure ISCC                                              588
C ISCC IST SHORT PURPLE CABLE C IN TRANSITION CONE                        588
5330  CONTINUE                                                            588
         AG_TITLE='ISCC'                                                  588
         AG_MODULE=MODULE                                                 588
         AG_MARK='V00'                                                    588
         CALL AGSPUSH                                                     588
         AG_MODULE=MODULE                                                 588
C    *                                                                    588
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           589
         CALL AGSRESET                                                    589
         AG_TITLE = 'COPPER'                                              589
         AG_EXNAME = 'MATERIAL'                                           589
         AG_PARLIST = 'NONE'                                              589
         CALL AXMATERIAL                                                  589
C    *                                                                    589
C    Geant ATTRIBUTE : ISCC ===> SEEN=1 COLO=629 <===                     589
         CALL AGSRESET                                                    590
         AG_SEEN = 1                                                      590
         AG_COLO = 629                                                    590
         AG_TITLE = 'ISCC'                                                590
         AG_EXNAME = 'ATTRIBUTE'                                          590
         AG_PARLIST = 'SEEN_COLO'                                         590
         CALL AXATTRIBUTE                                                 590
C    *                                                                    590
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*CABLEDIAMETER, DZ=0.5*23.84  590
C    18 <===                                                              590
         CALL AGSRESET                                                    592
         AG_RMIN = 0                                                      592
         AG_RMAX = 0.5*CABLEDIAMETER                                      592
         AG_DZ = 0.5*23.8418                                              592
         AG_TITLE = 'TUBE'                                                592
         AG_EXNAME = 'SHAPE'                                              592
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      592
         CALL AXSHAPE                                                     592
C    *                                                                    592
5331  CALL AGSPOPB                                                        593
      GO TO BLOCK_ISCC, (5330 ,5340)                                      593
C End of Local Procedure                                                  593
C ----------------------------------------------------------------------  594
C ------------ ISCT                                                       594
C Begin Local Procedure ISCT                                              595
C ISCT IST SHORT COOLING TUBE IN TRANSITION CONE                          595
5350  CONTINUE                                                            595
         AG_TITLE='ISCT'                                                  595
         AG_MODULE=MODULE                                                 595
         AG_MARK='V00'                                                    595
         CALL AGSPUSH                                                     595
         AG_MODULE=MODULE                                                 595
C    *                                                                    595
C    Geant MATERIAL : AIR ===> inherited arguments used <===              596
         CALL AGSRESET                                                    596
         AG_TITLE = 'AIR'                                                 596
         AG_EXNAME = 'MATERIAL'                                           596
         AG_PARLIST = 'NONE'                                              596
         CALL AXMATERIAL                                                  596
C    *                                                                    596
C    Geant ATTRIBUTE : ISCT ===> SEEN=0 COLO=6 <===                       596
         CALL AGSRESET                                                    597
         AG_SEEN = 0                                                      597
         AG_COLO = 6                                                      597
         AG_TITLE = 'ISCT'                                                597
         AG_EXNAME = 'ATTRIBUTE'                                          597
         AG_PARLIST = 'SEEN_COLO'                                         597
         CALL AXATTRIBUTE                                                 597
C    *                                                                    597
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*1.8331 DZ=0.5*8.4642 <===    597
         CALL AGSRESET                                                    598
         AG_RMIN = 0                                                      598
         AG_RMAX = 0.5*1.8331                                             598
         AG_DZ = 0.5*8.4642                                               598
         AG_TITLE = 'TUBE'                                                598
         AG_EXNAME = 'SHAPE'                                              598
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      598
         CALL AXSHAPE                                                     598
C    *                                                                    598
C    Create ICTJ :                                                        599
C    Call To Local Procedure ICTJ                                         599
         ASSIGN 5490 TO BLOCK_ICTJ                                        599
         GO TO  5480                                                      599
5490     CONTINUE                                                         599
C    *                                                                    599
C    Geant POSITION : ICTJ ===> MOTHER='ISCT' X=0.0 Y=0.0 Z=0.5*8.4642-0  600
C    .5*2.8889 <===                                                       600
         CALL AGSRESET                                                    600
         AG_MOTHER = 'ISCT'                                               600
         AG_X = 0.0                                                       600
         AG_Y = 0.0                                                       600
         AG_Z = 0.5*8.4642-0.5*2.8889                                     600
         AG_TITLE = 'ICTJ'                                                600
         AG_EXNAME = 'POSITION'                                           600
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      600
         CALL AXPOSITION                                                  600
C    *                                                                    600
C    Create ICTA :                                                        601
C    Call To Local Procedure ICTA                                         601
         ASSIGN 5510 TO BLOCK_ICTA                                        601
         GO TO  5500                                                      601
5510     CONTINUE                                                         601
C    *                                                                    601
C    Geant POSITION : ICTA ===> MOTHER='ISCT' X=0.0 Y=0.0 Z=0.5*8.4642-2  602
C    .8889-0.5*0.1778 <===                                                602
         CALL AGSRESET                                                    602
         AG_MOTHER = 'ISCT'                                               602
         AG_X = 0.0                                                       602
         AG_Y = 0.0                                                       602
         AG_Z = 0.5*8.4642-2.8889-0.5*0.1778                              602
         AG_TITLE = 'ICTA'                                                602
         AG_EXNAME = 'POSITION'                                           602
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      602
         CALL AXPOSITION                                                  602
C    *                                                                    602
C    Create ICTB :                                                        603
C    Call To Local Procedure ICTB                                         603
         ASSIGN 5530 TO BLOCK_ICTB                                        603
         GO TO  5520                                                      603
5530     CONTINUE                                                         603
C    *                                                                    603
C    Geant POSITION : ICTB ===> MOTHER='ISCT' X=0.0 Y=0.0 Z=-0.5*8.4642+  604
C    0.5*5.3975 <===                                                      604
         CALL AGSRESET                                                    604
         AG_MOTHER = 'ISCT'                                               604
         AG_X = 0.0                                                       604
         AG_Y = 0.0                                                       604
         AG_Z = -0.5*8.4642+0.5*5.3975                                    604
         AG_TITLE = 'ICTB'                                                604
         AG_EXNAME = 'POSITION'                                           604
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      604
         CALL AXPOSITION                                                  604
C    *                                                                    604
C    Create ICLE :                                                        605
C    Call To Local Procedure ICLE                                         605
         ASSIGN 5550 TO BLOCK_ICLE                                        605
         GO TO  5540                                                      605
5550     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : ICLE ===> MOTHER='ISCT' X=0.0 Y=0.0 Z=0.0 <===      606
         CALL AGSRESET                                                    606
         AG_MOTHER = 'ISCT'                                               606
         AG_X = 0.0                                                       606
         AG_Y = 0.0                                                       606
         AG_Z = 0.0                                                       606
         AG_TITLE = 'ICLE'                                                606
         AG_EXNAME = 'POSITION'                                           606
         AG_PARLIST = 'MOTHER_X_Y_Z'                                      606
         CALL AXPOSITION                                                  606
C    *                                                                    606
5351  CALL AGSPOPB                                                        607
      GO TO BLOCK_ISCT, (5350 ,5360)                                      607
C End of Local Procedure                                                  607
C ----------------------------------------------------------------------  608
C ------------ ICLE                                                       608
C Begin Local Procedure ICLE                                              609
C ICLE COOLING LIQUID IN ISCT VOLUME                                      609
5540  CONTINUE                                                            609
         AG_TITLE='ICLE'                                                  609
         AG_MODULE=MODULE                                                 609
         AG_MARK='V00'                                                    609
         CALL AGSPUSH                                                     609
         AG_MODULE=MODULE                                                 609
C    *                                                                    609
C    Geant ATTRIBUTE : ICLE ===> SEEN=1 COLO=920 <===                     610
         CALL AGSRESET                                                    610
         AG_SEEN = 1                                                      610
         AG_COLO = 920                                                    610
         AG_TITLE = 'ICLE'                                                610
         AG_EXNAME = 'ATTRIBUTE'                                          610
         AG_PARLIST = 'SEEN_COLO'                                         610
         CALL AXATTRIBUTE                                                 610
C    *                                                                    610
C    Geant MATERIAL : NOVEC7200 ===> inherited arguments used <===        610
         CALL AGSRESET                                                    611
         AG_TITLE = 'NOVEC7200'                                           611
         AG_EXNAME = 'MATERIAL'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXMATERIAL                                                  611
C    *                                                                    611
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*0.5334 DZ=0.5*8.4642 <===    611
         CALL AGSRESET                                                    612
         AG_RMIN = 0                                                      612
         AG_RMAX = 0.5*0.5334                                             612
         AG_DZ = 0.5*8.4642                                               612
         AG_TITLE = 'TUBE'                                                612
         AG_EXNAME = 'SHAPE'                                              612
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      612
         CALL AXSHAPE                                                     612
C    *                                                                    612
5541  CALL AGSPOPB                                                        613
      GO TO BLOCK_ICLE, (5540 ,5550)                                      613
C End of Local Procedure                                                  613
C ----------------------------------------------------------------------  614
C ------------ ICTA                                                       614
C Begin Local Procedure ICTA                                              615
C ICTA COOLING TUBE CONE PART                                             615
5500  CONTINUE                                                            615
         AG_TITLE='ICTA'                                                  615
         AG_MODULE=MODULE                                                 615
         AG_MARK='V00'                                                    615
         CALL AGSPUSH                                                     615
         AG_MODULE=MODULE                                                 615
C    *                                                                    615
C    Geant ATTRIBUTE : ICTA ===> SEEN=1 COLO=4 <===                       616
         CALL AGSRESET                                                    616
         AG_SEEN = 1                                                      616
         AG_COLO = 4                                                      616
         AG_TITLE = 'ICTA'                                                616
         AG_EXNAME = 'ATTRIBUTE'                                          616
         AG_PARLIST = 'SEEN_COLO'                                         616
         CALL AXATTRIBUTE                                                 616
C    *                                                                    616
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        616
         CALL AGSRESET                                                    617
         AG_TITLE = 'ALUMINIUM'                                           617
         AG_EXNAME = 'MATERIAL'                                           617
         AG_PARLIST = 'NONE'                                              617
         CALL AXMATERIAL                                                  617
C    *                                                                    617
C    Geant SHAPE : CONE ===> DZ=0.5*0.1778 RMN1=0.5*0.5334, RMX1=0.5*0.6  617
C    35 RMN2=0.5*0.8014 RMX2=0.5*0.903 <===                               617
         CALL AGSRESET                                                    619
         AG_DZ = 0.5*0.1778                                               619
         AG_RMN1 = 0.5*0.5334                                             619
         AG_RMX1 = 0.5*0.635                                              619
         AG_RMN2 = 0.5*0.8014                                             619
         AG_RMX2 = 0.5*0.903                                              619
         AG_TITLE = 'CONE'                                                619
         AG_EXNAME = 'SHAPE'                                              619
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            619
         CALL AXSHAPE                                                     619
C    *                                                                    619
5501  CALL AGSPOPB                                                        620
      GO TO BLOCK_ICTA, (5500 ,5510)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ ICTJ                                                       621
C Begin Local Procedure ICTJ                                              622
C ICTJ COOLING TUBE CONNECTOR LEFT                                        622
5480  CONTINUE                                                            622
         AG_TITLE='ICTJ'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant ATTRIBUTE : ICTJ ===> SEEN=1 COLO=4 <===                       623
         CALL AGSRESET                                                    623
         AG_SEEN = 1                                                      623
         AG_COLO = 4                                                      623
         AG_TITLE = 'ICTJ'                                                623
         AG_EXNAME = 'ATTRIBUTE'                                          623
         AG_PARLIST = 'SEEN_COLO'                                         623
         CALL AXATTRIBUTE                                                 623
C    *                                                                    623
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        623
         CALL AGSRESET                                                    624
         AG_TITLE = 'ALUMINIUM'                                           624
         AG_EXNAME = 'MATERIAL'                                           624
         AG_PARLIST = 'NONE'                                              624
         CALL AXMATERIAL                                                  624
C    *                                                                    624
C    Geant SHAPE : TUBE ===> RMIN=0.5*0.5334 RMAX=0.5*1.8331, DZ=0.5*2.8  624
C    889 <===                                                             624
         CALL AGSRESET                                                    626
         AG_RMIN = 0.5*0.5334                                             626
         AG_RMAX = 0.5*1.8331                                             626
         AG_DZ = 0.5*2.8889                                               626
         AG_TITLE = 'TUBE'                                                626
         AG_EXNAME = 'SHAPE'                                              626
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      626
         CALL AXSHAPE                                                     626
C    *                                                                    626
5481  CALL AGSPOPB                                                        627
      GO TO BLOCK_ICTJ, (5480 ,5490)                                      627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ ICTB                                                       628
C Begin Local Procedure ICTB                                              629
C ICTB COOLING TUBE PART                                                  629
5520  CONTINUE                                                            629
         AG_TITLE='ICTB'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant ATTRIBUTE : ICTB ===> SEEN=1 COLO=4 <===                       630
         CALL AGSRESET                                                    630
         AG_SEEN = 1                                                      630
         AG_COLO = 4                                                      630
         AG_TITLE = 'ICTB'                                                630
         AG_EXNAME = 'ATTRIBUTE'                                          630
         AG_PARLIST = 'SEEN_COLO'                                         630
         CALL AXATTRIBUTE                                                 630
C    *                                                                    630
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        630
         CALL AGSRESET                                                    631
         AG_TITLE = 'ALUMINIUM'                                           631
         AG_EXNAME = 'MATERIAL'                                           631
         AG_PARLIST = 'NONE'                                              631
         CALL AXMATERIAL                                                  631
C    *                                                                    631
C    Geant SHAPE : TUBE ===> RMIN=0.5*0.5334 RMAX=0.5*0.635, DZ=0.5*5.39  631
C    75 <===                                                              631
         CALL AGSRESET                                                    633
         AG_RMIN = 0.5*0.5334                                             633
         AG_RMAX = 0.5*0.635                                              633
         AG_DZ = 0.5*5.3975                                               633
         AG_TITLE = 'TUBE'                                                633
         AG_EXNAME = 'SHAPE'                                              633
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      633
         CALL AXSHAPE                                                     633
C    *                                                                    633
5521  CALL AGSPOPB                                                        634
      GO TO BLOCK_ICTB, (5520 ,5530)                                      634
C End of Local Procedure                                                  634
C ----------------------------------------------------------------------  635
C ------------ IBAM                                                       635
C Begin Local Procedure IBAM                                              636
C IBAM IST LADDER                                                         636
5200  CONTINUE                                                            636
         AG_TITLE='IBAM'                                                  636
         AG_MODULE=MODULE                                                 636
         AG_MARK='V00'                                                    636
         CALL AGSPUSH                                                     636
         AG_MODULE=MODULE                                                 636
C    *                                                                    636
C    Geant MATERIAL : AIR ===> inherited arguments used <===              637
         CALL AGSRESET                                                    637
         AG_TITLE = 'AIR'                                                 637
         AG_EXNAME = 'MATERIAL'                                           637
         AG_PARLIST = 'NONE'                                              637
         CALL AXMATERIAL                                                  637
C    *                                                                    637
C    Geant ATTRIBUTE : IBAM ===> SEEN=1 COLO=6 <===                       637
         CALL AGSRESET                                                    638
         AG_SEEN = 1                                                      638
         AG_COLO = 6                                                      638
         AG_TITLE = 'IBAM'                                                638
         AG_EXNAME = 'ATTRIBUTE'                                          638
         AG_PARLIST = 'SEEN_COLO'                                         638
         CALL AXATTRIBUTE                                                 638
C    *                                                                    638
C    Geant SHAPE : TUBS ===> RMIN=11.84 RMAX=17.5 PHI1=-18+90, PHI2=18+9  638
C    0 DZ=LADDER_LENGTH/2.0 <===                                          638
         CALL AGSRESET                                                    640
         AG_RMIN = 11.84                                                  640
         AG_RMAX = 17.5                                                   640
         AG_PHI1 = -18+90                                                 640
         AG_PHI2 = 18+90                                                  640
         AG_DZ = LADDER_LENGTH/2.0                                        640
         AG_TITLE = 'TUBS'                                                640
         AG_EXNAME = 'SHAPE'                                              640
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            640
         CALL AXSHAPE                                                     640
C    *                                                                    640
C    Create ILCC :                                                        641
C    Call To Local Procedure ILCC                                         641
         ASSIGN 5570 TO BLOCK_ILCC                                        641
         GO TO  5560                                                      641
5570     CONTINUE                                                         641
C    *                                                                    641
         X= HALFLADWDT - 0.15 - HALFCOMBWDT                               642
         Y= - HALFLADTHK + 2.4244 + 2*HALFSKINTHK + HALFCOMBTHK           643
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             644
C    Geant POSITION : ILCC ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-HAL  647
C    FCOMBLEN-2*HALFENDCAPMOMLEN-OUTSIDETUBEWESTLEN ALPHAZ=-1.0*TILT <==  647
C    =                                                                    647
         CALL AGSRESET                                                    647
         AG_MOTHER = 'IBAM'                                               647
         AG_X = XP                                                        647
         AG_Y = YP                                                        647
         AG_Z =                                                           647
     *   HALFLADLEN-HALFCOMBLEN-2*HALFENDCAPMOMLEN-OUTSIDETUBEWESTLEN     647
         AG_ALPHAN=AG_ALPHAN+1                                            647
         AG_ALPHAK(AG_ALPHAN)=3                                           647
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   647
         AG_TITLE = 'ILCC'                                                647
         AG_EXNAME = 'POSITION'                                           647
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                               647
         CALL AXPOSITION                                                  647
C    *                                                                    647
C    Create IBCF :                                                        648
C    Call To Local Procedure IBCF                                         648
         ASSIGN 5590 TO BLOCK_IBCF                                        648
         GO TO  5580                                                      648
5590     CONTINUE                                                         648
C    *                                                                    648
         X= - HALFLADWDT + DISTFOAMTOLADDER + HALFFOAMWDT                 649
         Y=HALFFOAMTHK + 2.4244 - HALFLADTHK + 2*HALFSKINTHK              650
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             651
C    Geant POSITION : IBCF ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-HAL  654
C    FFOAMLEN-2*HALFENDCAPMOMLEN-OUTSIDETUBEWESTLEN ALPHAZ=-1.0*TILT KON  654
C    LY='MANY' <===                                                       654
         CALL AGSRESET                                                    654
         AG_MOTHER = 'IBAM'                                               654
         AG_X = XP                                                        654
         AG_Y = YP                                                        654
         AG_Z =                                                           654
     *   HALFLADLEN-HALFFOAMLEN-2*HALFENDCAPMOMLEN-OUTSIDETUBEWESTLEN     654
         AG_ALPHAN=AG_ALPHAN+1                                            654
         AG_ALPHAK(AG_ALPHAN)=3                                           654
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   654
         AG_KONLY = 'MANY'                                                654
         AG_TITLE = 'IBCF'                                                654
         AG_EXNAME = 'POSITION'                                           654
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                         654
         CALL AXPOSITION                                                  654
C    *                                                                    654
C    Create ILST :                                                        655
C    Call To Local Procedure ILST                                         655
         ASSIGN 5610 TO BLOCK_ILST                                        655
         GO TO  5600                                                      655
5610     CONTINUE                                                         655
C    *                                                                    655
         X=HALFLADWDT - HALFSKINWDT - 0.15                                656
         Y= - HALFLADTHK + 2*HALFCOMBTHK + 2.4244 + 3*HALFSKINTHK -       657
     *   HALFTHKAIRINHBD                                                  657
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             658
C    Geant POSITION : ILST ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-HAL  660
C    FSKINLEN-OUTSIDETUBEWESTLEN-1.75 ALPHAZ=-1.0*TILT <===               660
         CALL AGSRESET                                                    660
         AG_MOTHER = 'IBAM'                                               660
         AG_X = XP                                                        660
         AG_Y = YP                                                        660
         AG_Z = HALFLADLEN-HALFSKINLEN-OUTSIDETUBEWESTLEN-1.75            660
         AG_ALPHAN=AG_ALPHAN+1                                            660
         AG_ALPHAK(AG_ALPHAN)=3                                           660
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   660
         AG_TITLE = 'ILST'                                                660
         AG_EXNAME = 'POSITION'                                           660
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                               660
         CALL AXPOSITION                                                  660
C    *                                                                    660
C    Create ILSB :                                                        661
C    Call To Local Procedure ILSB                                         661
         ASSIGN 5630 TO BLOCK_ILSB                                        661
         GO TO  5620                                                      661
5630     CONTINUE                                                         661
C    *                                                                    661
         X=HALFLADWDT - HALFSKINWDT - 0.15                                662
         Y= - HALFLADTHK + 2.4244 + HALFSKINTHK + HALFTHKAIRINHBD         663
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             664
C    Geant POSITION : ILSB ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-HAL  666
C    FSKINLEN-OUTSIDETUBEWESTLEN-1.75 ALPHAZ=-1.0*TILT <===               666
         CALL AGSRESET                                                    666
         AG_MOTHER = 'IBAM'                                               666
         AG_X = XP                                                        666
         AG_Y = YP                                                        666
         AG_Z = HALFLADLEN-HALFSKINLEN-OUTSIDETUBEWESTLEN-1.75            666
         AG_ALPHAN=AG_ALPHAN+1                                            666
         AG_ALPHAK(AG_ALPHAN)=3                                           666
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   666
         AG_TITLE = 'ILSB'                                                666
         AG_EXNAME = 'POSITION'                                           666
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                               666
         CALL AXPOSITION                                                  666
C    *                                                                    666
C    Create IBHT :                                                        667
C    Call To Local Procedure IBHT                                         667
         ASSIGN 5650 TO BLOCK_IBHT                                        667
         GO TO  5640                                                      667
5650     CONTINUE                                                         667
C    *                                                                    667
         X=HALFLADWDT - HALFHBDWDTTB - 0.15                               668
         Y= - HALFLADTHK + 4*HALFSKINTHK + 2*HALFCOMBTHK + 2.4244 +       669
     *   HALFHBDTHKTB - HALFTHKAIRINHBD                                   669
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             670
C    Geant POSITION : IBHT ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-HAL  672
C    FHBDLENTB-OUTSIDETUBEWESTLEN-1.75 ALPHAZ=-1.0*TILT <===              672
         CALL AGSRESET                                                    672
         AG_MOTHER = 'IBAM'                                               672
         AG_X = XP                                                        672
         AG_Y = YP                                                        672
         AG_Z = HALFLADLEN-HALFHBDLENTB-OUTSIDETUBEWESTLEN-1.75           672
         AG_ALPHAN=AG_ALPHAN+1                                            672
         AG_ALPHAK(AG_ALPHAN)=3                                           672
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   672
         AG_TITLE = 'IBHT'                                                672
         AG_EXNAME = 'POSITION'                                           672
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                               672
         CALL AXPOSITION                                                  672
C    *                                                                    672
C    Create IBHB :                                                        673
C    Call To Local Procedure IBHB                                         673
         ASSIGN 5670 TO BLOCK_IBHB                                        673
         GO TO  5660                                                      673
5670     CONTINUE                                                         673
C    *                                                                    673
         X=HALFLADWDT - HALFHBDWDTTB - 0.15                               674
         Y= - HALFLADTHK + 2.4244 - HALFHBDTHKTB + HALFTHKAIRINHBD        675
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             676
C    Geant POSITION : IBHB ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-HAL  678
C    FHBDLENTB-OUTSIDETUBEWESTLEN-1.75 ALPHAZ=-1.0*TILT <===              678
         CALL AGSRESET                                                    678
         AG_MOTHER = 'IBAM'                                               678
         AG_X = XP                                                        678
         AG_Y = YP                                                        678
         AG_Z = HALFLADLEN-HALFHBDLENTB-OUTSIDETUBEWESTLEN-1.75           678
         AG_ALPHAN=AG_ALPHAN+1                                            678
         AG_ALPHAK(AG_ALPHAN)=3                                           678
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   678
         AG_TITLE = 'IBHB'                                                678
         AG_EXNAME = 'POSITION'                                           678
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                               678
         CALL AXPOSITION                                                  678
C    *                                                                    678
C    Create IBHC :                                                        679
C    Call To Local Procedure IBHC                                         679
         ASSIGN 5690 TO BLOCK_IBHC                                        679
         GO TO  5680                                                      679
5690     CONTINUE                                                         679
C    *                                                                    679
         X= - HALFLADWDT + DISTFOAMTOLADDER                               680
         Y= - HALFLADTHK + HALFFOAMTHK + 2.4244 + 2*HALFSKINTHK           681
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             682
C    Geant POSITION : IBHC ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-HAL  684
C    FHBDARCLEN-OUTSIDETUBEWESTLEN-1.75 ALPHAZ=-1.0*TILT <===             684
         CALL AGSRESET                                                    684
         AG_MOTHER = 'IBAM'                                               684
         AG_X = XP                                                        684
         AG_Y = YP                                                        684
         AG_Z = HALFLADLEN-HALFHBDARCLEN-OUTSIDETUBEWESTLEN-1.75          684
         AG_ALPHAN=AG_ALPHAN+1                                            684
         AG_ALPHAK(AG_ALPHAN)=3                                           684
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   684
         AG_TITLE = 'IBHC'                                                684
         AG_EXNAME = 'POSITION'                                           684
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                               684
         CALL AXPOSITION                                                  684
C    *                                                                    684
C    Create IBAT :                                                        685
C    Call To Local Procedure IBAT                                         685
         ASSIGN 5710 TO BLOCK_IBAT                                        685
         GO TO  5700                                                      685
5710     CONTINUE                                                         685
C    *                                                                    685
         X= - HALFLADWDT + DISTFOAMTOLADDER + HALFFOAMWDT + TUBERMAX      686
         Y=HALFLADTHK - 2*HALFTBOARDTHK - 2*HALFTBSLOTTHK -               688
     *   2*HALFHBDTHKTB - 2*HALFSKINTHK - DISTTUBETOPSKIN                 688
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             689
C    Geant POSITION : IBAT ===> X=XP Y=YP Z=HALFTUBELEN-HALFLADLEN ALPHA  690
C    Z=-1.0*TILT KONLY='MANY' <===                                        690
         CALL AGSRESET                                                    690
         AG_X = XP                                                        690
         AG_Y = YP                                                        690
         AG_Z = HALFTUBELEN-HALFLADLEN                                    690
         AG_ALPHAN=AG_ALPHAN+1                                            690
         AG_ALPHAK(AG_ALPHAN)=3                                           690
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   690
         AG_KONLY = 'MANY'                                                690
         AG_TITLE = 'IBAT'                                                690
         AG_EXNAME = 'POSITION'                                           690
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                690
         CALL AXPOSITION                                                  690
C    *                                                                    690
C    Create IBCW :                                                        691
C    Call To Local Procedure IBCW                                         691
         ASSIGN 5730 TO BLOCK_IBCW                                        691
         GO TO  5720                                                      691
5730     CONTINUE                                                         691
C    *                                                                    691
         X= - HALFLADWDT + DISTFOAMTOLADDER + HALFFOAMWDT + TUBERMAX      692
         Y=HALFLADTHK - 2*HALFTBOARDTHK - 2*HALFTBSLOTTHK -               694
     *   2*HALFHBDTHKTB - 2*HALFSKINTHK - DISTTUBETOPSKIN                 694
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             695
C    Geant POSITION : IBCW ===> X=XP Y=YP Z=LIQUIDINTUBELEN-HALFLADLEN A  696
C    LPHAZ=-1.0*TILT <===                                                 696
         CALL AGSRESET                                                    696
         AG_X = XP                                                        696
         AG_Y = YP                                                        696
         AG_Z = LIQUIDINTUBELEN-HALFLADLEN                                696
         AG_ALPHAN=AG_ALPHAN+1                                            696
         AG_ALPHAK(AG_ALPHAN)=3                                           696
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   696
         AG_TITLE = 'IBCW'                                                696
         AG_EXNAME = 'POSITION'                                           696
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      696
         CALL AXPOSITION                                                  696
C    *                                                                    696
C    Create IBRB :                                                        697
C    Call To Local Procedure IBRB                                         697
         ASSIGN 5750 TO BLOCK_IBRB                                        697
         GO TO  5740                                                      697
5750     CONTINUE                                                         697
C    *                                                                    697
         X= - HALFLADWDT + HALFTBOARDWDT                                  698
         Y=HALFLADTHK - HALFTBOARDTHK                                     699
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             700
C    Geant POSITION : IBRB ===> X=XP Y=YP Z=-HALFLADLEN+OUTSIDETUBEEASTL  703
C    EN-OUTSIDETBOARDLEN+HALFTBOARDLEN ALPHAZ=-1.0*TILT <===              703
         CALL AGSRESET                                                    703
         AG_X = XP                                                        703
         AG_Y = YP                                                        703
         AG_Z =                                                           703
     *   -HALFLADLEN+OUTSIDETUBEEASTLEN-OUTSIDETBOARDLEN+HALFTBOARDLEN    703
         AG_ALPHAN=AG_ALPHAN+1                                            703
         AG_ALPHAK(AG_ALPHAN)=3                                           703
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   703
         AG_TITLE = 'IBRB'                                                703
         AG_EXNAME = 'POSITION'                                           703
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      703
         CALL AXPOSITION                                                  703
C    *                                                                    703
C    Create IRSA :                                                        704
C    Call To Local Procedure IRSA                                         704
         ASSIGN 5770 TO BLOCK_IRSA                                        704
         GO TO  5760                                                      704
5770     CONTINUE                                                         704
C    *                                                                    704
         X= - HALFLADWDT + DISTFOAMTOLADDER + DISTSLOTCTOFOAM +           705
     *   5*HALFTBSLOTWDT + 2*TBSLOTGAPSIZE                                705
         Y=HALFLADTHK - 2*HALFTBOARDTHK - HALFTBSLOTTHK                   706
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             707
C    Geant POSITION : IRSA ===> X=XP Y=YP Z=-HALFLADLEN+OUTSIDETUBEEASTL  709
C    EN+1.75+0.03+HALFTBSLOTLEN ALPHAZ=-1.0*TILT <===                     709
         CALL AGSRESET                                                    709
         AG_X = XP                                                        709
         AG_Y = YP                                                        709
         AG_Z = -HALFLADLEN+OUTSIDETUBEEASTLEN+1.75+0.03+HALFTBSLOTLEN    709
         AG_ALPHAN=AG_ALPHAN+1                                            709
         AG_ALPHAK(AG_ALPHAN)=3                                           709
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   709
         AG_TITLE = 'IRSA'                                                709
         AG_EXNAME = 'POSITION'                                           709
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      709
         CALL AXPOSITION                                                  709
C    *                                                                    709
C    Create IRSB :                                                        710
C    Call To Local Procedure IRSB                                         710
         ASSIGN 5790 TO BLOCK_IRSB                                        710
         GO TO  5780                                                      710
5790     CONTINUE                                                         710
C    *                                                                    710
         X= - HALFLADWDT + DISTFOAMTOLADDER + DISTSLOTCTOFOAM +           711
     *   3*HALFTBSLOTWDT + TBSLOTGAPSIZE                                  711
         Y=HALFLADTHK - 2*HALFTBOARDTHK - HALFTBSLOTTHK                   712
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             713
C    Geant POSITION : IRSB ===> X=XP Y=YP Z=-HALFLADLEN+OUTSIDETUBEEASTL  716
C    EN+1.75+0.03+3*HALFTBSLOTLEN-OVERLAPSLOTSLEN ALPHAZ=-1.0*TILT <===   716
         CALL AGSRESET                                                    716
         AG_X = XP                                                        716
         AG_Y = YP                                                        716
         AG_Z =                                                           716
     *   -HALFLADLEN+OUTSIDETUBEEASTLEN+1.75+0.03+3*HALFTBSLOTLEN-        716
     *   OVERLAPSLOTSLEN                                                  716
         AG_ALPHAN=AG_ALPHAN+1                                            716
         AG_ALPHAK(AG_ALPHAN)=3                                           716
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   716
         AG_TITLE = 'IRSB'                                                716
         AG_EXNAME = 'POSITION'                                           716
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      716
         CALL AXPOSITION                                                  716
C    *                                                                    716
C    Create IRSC :                                                        717
C    Call To Local Procedure IRSC                                         717
         ASSIGN 5810 TO BLOCK_IRSC                                        717
         GO TO  5800                                                      717
5810     CONTINUE                                                         717
C    *                                                                    717
         X= - HALFLADWDT + DISTFOAMTOLADDER + DISTSLOTCTOFOAM +           718
     *   HALFTBSLOTWDT                                                    718
         Y=HALFLADTHK - 2*HALFTBOARDTHK - HALFTBSLOTTHK                   719
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             720
C    Geant POSITION : IRSC ===> X=XP Y=YP Z=-HALFLADLEN+OUTSIDETUBEEASTL  723
C    EN+1.75+0.03+5*HALFTBSLOTLEN-2*OVERLAPSLOTSLEN ALPHAZ=-1.0*TILT <==  723
C    =                                                                    723
         CALL AGSRESET                                                    723
         AG_X = XP                                                        723
         AG_Y = YP                                                        723
         AG_Z =                                                           723
     *   -HALFLADLEN+OUTSIDETUBEEASTLEN+1.75+0.03+5*HALFTBSLOTLEN-2*      723
     *   OVERLAPSLOTSLEN                                                  723
         AG_ALPHAN=AG_ALPHAN+1                                            723
         AG_ALPHAK(AG_ALPHAN)=3                                           723
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   723
         AG_TITLE = 'IRSC'                                                723
         AG_EXNAME = 'POSITION'                                           723
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      723
         CALL AXPOSITION                                                  723
C    *                                                                    723
C    Create IBTS :                                                        724
C    Call To Local Procedure IBTS                                         724
         ASSIGN 5830 TO BLOCK_IBTS                                        724
         GO TO  5820                                                      724
5830     CONTINUE                                                         724
C    *                                                                    724
         X= - HALFLADWDT + DISTFOAMTOLADDER + 0.2581 + 0.5*0.1625         725
         Y=2.4244 - 2*HALFHBDTHKTB - HALFLADTHK - 0.5*0.06                726
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             727
C    Geant POSITION : IBTS ===> MOTHER='IBAM' X=XP Y=YP Z=-HALFLADLEN+OU  729
C    TSIDETUBEEASTLEN+21.0235+0.5*0.1625 ALPHAZ=-1.0*TILT <===            729
         CALL AGSRESET                                                    729
         AG_MOTHER = 'IBAM'                                               729
         AG_X = XP                                                        729
         AG_Y = YP                                                        729
         AG_Z = -HALFLADLEN+OUTSIDETUBEEASTLEN+21.0235+0.5*0.1625         729
         AG_ALPHAN=AG_ALPHAN+1                                            729
         AG_ALPHAK(AG_ALPHAN)=3                                           729
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   729
         AG_TITLE = 'IBTS'                                                729
         AG_EXNAME = 'POSITION'                                           729
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                               729
         CALL AXPOSITION                                                  729
C    *                                                                    729
C    Create IECW :                                                        730
C    Call To Local Procedure IECW                                         730
         ASSIGN 5850 TO BLOCK_IECW                                        730
         GO TO  5840                                                      730
5850     CONTINUE                                                         730
C    *                                                                    730
         X=HALFLADWDT - HALFENDCAPMOMWDT - 0.15                           731
         Y=HALFENDCAPMOMTHK + 2.4244 - HALFLADTHK                         732
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             733
C    Geant POSITION : IECW ===> MOTHER='IBAM' X=XP Y=YP Z=HALFLADLEN-OUT  735
C    SIDETUBEWESTLEN-HALFENDCAPMOMLEN ALPHAZ=-1.0*TILT KONLY='MANY' <===  735
         CALL AGSRESET                                                    735
         AG_MOTHER = 'IBAM'                                               735
         AG_X = XP                                                        735
         AG_Y = YP                                                        735
         AG_Z = HALFLADLEN-OUTSIDETUBEWESTLEN-HALFENDCAPMOMLEN            735
         AG_ALPHAN=AG_ALPHAN+1                                            735
         AG_ALPHAK(AG_ALPHAN)=3                                           735
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   735
         AG_KONLY = 'MANY'                                                735
         AG_TITLE = 'IECW'                                                735
         AG_EXNAME = 'POSITION'                                           735
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                         735
         CALL AXPOSITION                                                  735
C    *                                                                    735
C    Create IECE :                                                        736
C    Call To Local Procedure IECE                                         736
         ASSIGN 5870 TO BLOCK_IECE                                        736
         GO TO  5860                                                      736
5870     CONTINUE                                                         736
C    *                                                                    736
         X=HALFLADWDT - HALFENDCAPMOMWDT - 0.15                           737
         Y=HALFENDCAPMOMTHK + 2.4244 - HALFLADTHK                         738
         CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                             739
C    Geant POSITION : IECE ===> MOTHER='IBAM' X=XP Y=YP Z=-HALFLADLEN+OU  742
C    TSIDETUBEEASTLEN+HALFENDCAPMOMLEN ALPHAZ=-1.0*TILT KONLY='MANY' <==  742
C    =                                                                    742
         CALL AGSRESET                                                    742
         AG_MOTHER = 'IBAM'                                               742
         AG_X = XP                                                        742
         AG_Y = YP                                                        742
         AG_Z = -HALFLADLEN+OUTSIDETUBEEASTLEN+HALFENDCAPMOMLEN           742
         AG_ALPHAN=AG_ALPHAN+1                                            742
         AG_ALPHAK(AG_ALPHAN)=3                                           742
         AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                   742
         AG_KONLY = 'MANY'                                                742
         AG_TITLE = 'IECE'                                                742
         AG_EXNAME = 'POSITION'                                           742
         AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                         742
         CALL AXPOSITION                                                  742
C    *                                                                    742
C    *                                                                    742
         NU=1                                                             743
            GO TO 5883                                                    743
5881        NU=NU+1                                                       743
5883        IF(NU-(NUNIT).GT.0) GO TO 5882                                743
C       Create IBLM :                                                     744
C       Call To Local Procedure IBLM                                      744
            ASSIGN 5900 TO BLOCK_IBLM                                     744
            GO TO  5890                                                   744
5900        CONTINUE                                                      744
C       *                                                                 744
            X=HALFLADWDT-HALFSENMOMWDT-DISTSENSORTOLADDER-0.15            745
            Y=2.4244-2*HALFHBDTHKTB-HALFSENMOMTHK-HALFLADTHK              746
            CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                          747
C       Check DEBUG==1                                                    748
            IF (DEBUG.EQ.1) THEN                                          748
            WRITE(LOUT,5910)X                                             748
5910        FORMAT('xl ', F7.4)                                           748
            WRITE(LOUT,5920)Y                                             749
5920        FORMAT('yl ', F7.4)                                           749
            WRITE(LOUT,5930)XPOS                                          750
5930        FORMAT('xp ', F7.4)                                           750
            WRITE(LOUT,5940)YPOS                                          751
5940        FORMAT('yp ', F7.4)                                           751
            ENDIF                                                         753
C       Geant POSITION : IBLM ===> MOTHER='IBAM' X=XP Y=YP Z=-1.0*HALFLA  756
C       DLEN+DISTLADDERTOIBLM+HALFSENMOMLEN*(2*NU-1) ALPHAZ=-1.0*TILT KO  756
C       NLY='ONLY' <===                                                   756
            CALL AGSRESET                                                 756
            AG_MOTHER = 'IBAM'                                            756
            AG_X = XP                                                     756
            AG_Y = YP                                                     756
            AG_Z =                                                        756
     *      -1.0*HALFLADLEN+DISTLADDERTOIBLM+HALFSENMOMLEN*(2*NU-1)       756
            AG_ALPHAN=AG_ALPHAN+1                                         756
            AG_ALPHAK(AG_ALPHAN)=3                                        756
            AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                756
            AG_KONLY = 'ONLY'                                             756
            AG_TITLE = 'IBLM'                                             756
            AG_EXNAME = 'POSITION'                                        756
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      756
            CALL AXPOSITION                                               756
C       *                                                                 756
         GO TO 5881                                                       757
5882     CONTINUE                                                         757
C    *                                                                    758
         NSC=1                                                            758
            GO TO 5953                                                    758
5951        NSC=NSC+1                                                     758
5953        IF(NSC-(2).GT.0) GO TO 5952                                   758
C       Create ISCM :                                                     759
C       Call To Local Procedure ISCM                                      759
            ASSIGN 5970 TO BLOCK_ISCM                                     759
            GO TO  5960                                                   759
5970        CONTINUE                                                      759
C       *                                                                 759
            X=HALFLADWDT-HALFENDCAPMOMWDT-0.15                            760
            Y=-HALFLADTHK+2.4244+2*HALFENDCAPMOMTHK-0.2845                761
            CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                          762
C       Geant POSITION : ISCM ===> MOTHER='IBAM' X=XP Y=YP Z=-HALFLADLEN  765
C       +OUTSIDETUBEEASTLEN+0.75+(1.5+HALFCOMBLEN)*(2*NSC-2) ALPHAZ=-1.0  765
C       *TILT KONLY='MANY' <===                                           765
            CALL AGSRESET                                                 765
            AG_MOTHER = 'IBAM'                                            765
            AG_X = XP                                                     765
            AG_Y = YP                                                     765
            AG_Z =                                                        765
     *      -HALFLADLEN+OUTSIDETUBEEASTLEN+0.75+(1.5+HALFCOMBLEN)*(2*     765
     *      NSC-2)                                                        765
            AG_ALPHAN=AG_ALPHAN+1                                         765
            AG_ALPHAK(AG_ALPHAN)=3                                        765
            AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                765
            AG_KONLY = 'MANY'                                             765
            AG_TITLE = 'ISCM'                                             765
            AG_EXNAME = 'POSITION'                                        765
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      765
            CALL AXPOSITION                                               765
C       *                                                                 765
         GO TO 5951                                                       766
5952     CONTINUE                                                         766
C    *                                                                    767
         NS=1                                                             767
            GO TO 5983                                                    767
5981        NS=NS+1                                                       767
5983        IF(NS-(2).GT.0) GO TO 5982                                    767
C       Create IBSR :                                                     768
C       Call To Local Procedure IBSR                                      768
            ASSIGN 6000 TO BLOCK_IBSR                                     768
            GO TO  5990                                                   768
6000        CONTINUE                                                      768
C       *                                                                 768
            X=HALFLADWDT-1.0*HALFSRMOMWDT                                 769
            Y=HALFSRMOMTHK-1.0*HALFLADTHK                                 770
            CALL TRANS_TO_IBAM( X,Y,TILT,XP,YP )                          771
C       Geant POSITION : IBSR ===> MOTHER='IBAM' X=XP Y=YP Z=-HALFLADLEN  773
C       +9.9143+HALFSRMOMLEN+58.88*(NS-1) ALPHAZ=-1.0*TILT KONLY='MANY'   773
C       <===                                                              773
            CALL AGSRESET                                                 773
            AG_MOTHER = 'IBAM'                                            773
            AG_X = XP                                                     773
            AG_Y = YP                                                     773
            AG_Z = -HALFLADLEN+9.9143+HALFSRMOMLEN+58.88*(NS-1)           773
            AG_ALPHAN=AG_ALPHAN+1                                         773
            AG_ALPHAK(AG_ALPHAN)=3                                        773
            AG_ALPHAL(AG_ALPHAN)=-1.0*TILT                                773
            AG_KONLY = 'MANY'                                             773
            AG_TITLE = 'IBSR'                                             773
            AG_EXNAME = 'POSITION'                                        773
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_KONLY'                      773
            CALL AXPOSITION                                               773
C       *                                                                 773
         GO TO 5981                                                       774
5982     CONTINUE                                                         774
5201  CALL AGSPOPB                                                        775
      GO TO BLOCK_IBAM, (5200 ,5210)                                      775
C End of Local Procedure                                                  775
C ----------------------------------------------------------------------  776
C ------------ ILCC                                                       776
C Begin Local Procedure ILCC                                              777
C ILCC CHONEYCOMB MOTHER VOLUME                                           777
5560  CONTINUE                                                            777
         AG_TITLE='ILCC'                                                  777
         AG_MODULE=MODULE                                                 777
         AG_MARK='V00'                                                    777
         CALL AGSPUSH                                                     777
         AG_MODULE=MODULE                                                 777
C    *                                                                    777
C    Geant ATTRIBUTE : ILCC ===> SEEN=0 COLO=1 <===                       778
         CALL AGSRESET                                                    778
         AG_SEEN = 0                                                      778
         AG_COLO = 1                                                      778
         AG_TITLE = 'ILCC'                                                778
         AG_EXNAME = 'ATTRIBUTE'                                          778
         AG_PARLIST = 'SEEN_COLO'                                         778
         CALL AXATTRIBUTE                                                 778
C    *                                                                    778
C    Geant MATERIAL : AIR ===> inherited arguments used <===              778
         CALL AGSRESET                                                    779
         AG_TITLE = 'AIR'                                                 779
         AG_EXNAME = 'MATERIAL'                                           779
         AG_PARLIST = 'NONE'                                              779
         CALL AXMATERIAL                                                  779
C    *                                                                    779
C    Geant SHAPE : BOX ===> DX=HALFCOMBWDT DY=HALFCOMBTHK, DZ=HALFCOMBLE  779
C    N <===                                                               779
         CALL AGSRESET                                                    781
         AG_DX = HALFCOMBWDT                                              781
         AG_DY = HALFCOMBTHK                                              781
         AG_DZ = HALFCOMBLEN                                              781
         AG_TITLE = 'BOX'                                                 781
         AG_EXNAME = 'SHAPE'                                              781
         AG_PARLIST = 'DX_DY_DZ'                                          781
         CALL AXSHAPE                                                     781
C    *                                                                    781
C    Create ICCU :                                                        782
C    Call To Local Procedure ICCU                                         782
         ASSIGN 6020 TO BLOCK_ICCU                                        782
         GO TO  6010                                                      782
6020     CONTINUE                                                         782
C    *                                                                    782
C    Geant POSITION : ICCU ===> X=0.0 Y=0.0 Z=-HALFCOMBLEN+HALFCOMBLENCO  783
C    NNECT <===                                                           783
         CALL AGSRESET                                                    783
         AG_X = 0.0                                                       783
         AG_Y = 0.0                                                       783
         AG_Z = -HALFCOMBLEN+HALFCOMBLENCONNECT                           783
         AG_TITLE = 'ICCU'                                                783
         AG_EXNAME = 'POSITION'                                           783
         AG_PARLIST = 'X_Y_Z'                                             783
         CALL AXPOSITION                                                  783
C    *                                                                    783
C    Create ICCD :                                                        784
C    Call To Local Procedure ICCD                                         784
         ASSIGN 6040 TO BLOCK_ICCD                                        784
         GO TO  6030                                                      784
6040     CONTINUE                                                         784
C    *                                                                    784
C    Geant POSITION : ICCD ===> X=0.0 Y=0.0 Z=HALFCOMBLENCONNECT <===     785
         CALL AGSRESET                                                    785
         AG_X = 0.0                                                       785
         AG_Y = 0.0                                                       785
         AG_Z = HALFCOMBLENCONNECT                                        785
         AG_TITLE = 'ICCD'                                                785
         AG_EXNAME = 'POSITION'                                           785
         AG_PARLIST = 'X_Y_Z'                                             785
         CALL AXPOSITION                                                  785
C    *                                                                    785
5561  CALL AGSPOPB                                                        786
      GO TO BLOCK_ILCC, (5560 ,5570)                                      786
C End of Local Procedure                                                  786
C ----------------------------------------------------------------------  787
C ------------ ICCU                                                       787
C Begin Local Procedure ICCU                                              788
C ICCU IS THE IST CARBON HONEYCOMB BOX VOLUME ON EAST SIDE                788
6010  CONTINUE                                                            788
         AG_TITLE='ICCU'                                                  788
         AG_MODULE=MODULE                                                 788
         AG_MARK='V00'                                                    788
         CALL AGSPUSH                                                     788
         AG_MODULE=MODULE                                                 788
C    *                                                                    788
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          789
         CALL AGSRESET                                                    789
         AG_TITLE = 'CFRPMIX'                                             789
         AG_EXNAME = 'MATERIAL'                                           789
         AG_PARLIST = 'NONE'                                              789
         CALL AXMATERIAL                                                  789
C    *                                                                    789
C    Geant ATTRIBUTE : ICCU ===> SEEN=1 COLO=1 <===                       789
         CALL AGSRESET                                                    790
         AG_SEEN = 1                                                      790
         AG_COLO = 1                                                      790
         AG_TITLE = 'ICCU'                                                790
         AG_EXNAME = 'ATTRIBUTE'                                          790
         AG_PARLIST = 'SEEN_COLO'                                         790
         CALL AXATTRIBUTE                                                 790
C    *                                                                    790
C    Geant SHAPE : BOX ===> DX=HALFCOMBWDT DY=HALFCOMBTHKCONNECT, DZ=HAL  790
C    FCOMBLENCONNECT <===                                                 790
         CALL AGSRESET                                                    792
         AG_DX = HALFCOMBWDT                                              792
         AG_DY = HALFCOMBTHKCONNECT                                       792
         AG_DZ = HALFCOMBLENCONNECT                                       792
         AG_TITLE = 'BOX'                                                 792
         AG_EXNAME = 'SHAPE'                                              792
         AG_PARLIST = 'DX_DY_DZ'                                          792
         CALL AXSHAPE                                                     792
C    *                                                                    792
6011  CALL AGSPOPB                                                        793
      GO TO BLOCK_ICCU, (6010 ,6020)                                      793
C End of Local Procedure                                                  793
C ----------------------------------------------------------------------  794
C ------------ ICCD                                                       794
C Begin Local Procedure ICCD                                              795
C ICCD IS THE IST CARBON HONEYCOMB BOX VOLUME ON WEST SIDE                795
6030  CONTINUE                                                            795
         AG_TITLE='ICCD'                                                  795
         AG_MODULE=MODULE                                                 795
         AG_MARK='V00'                                                    795
         CALL AGSPUSH                                                     795
         AG_MODULE=MODULE                                                 795
C    *                                                                    795
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          796
         CALL AGSRESET                                                    796
         AG_TITLE = 'CFRPMIX'                                             796
         AG_EXNAME = 'MATERIAL'                                           796
         AG_PARLIST = 'NONE'                                              796
         CALL AXMATERIAL                                                  796
C    *                                                                    796
C    Geant ATTRIBUTE : ICCD ===> SEEN=1 COLO=1 <===                       796
         CALL AGSRESET                                                    797
         AG_SEEN = 1                                                      797
         AG_COLO = 1                                                      797
         AG_TITLE = 'ICCD'                                                797
         AG_EXNAME = 'ATTRIBUTE'                                          797
         AG_PARLIST = 'SEEN_COLO'                                         797
         CALL AXATTRIBUTE                                                 797
C    *                                                                    797
C    Geant SHAPE : BOX ===> DX=HALFCOMBWDT DY=HALFCOMBTHK, DZ=HALFCOMBLE  797
C    N-HALFCOMBLENCONNECT <===                                            797
         CALL AGSRESET                                                    799
         AG_DX = HALFCOMBWDT                                              799
         AG_DY = HALFCOMBTHK                                              799
         AG_DZ = HALFCOMBLEN-HALFCOMBLENCONNECT                           799
         AG_TITLE = 'BOX'                                                 799
         AG_EXNAME = 'SHAPE'                                              799
         AG_PARLIST = 'DX_DY_DZ'                                          799
         CALL AXSHAPE                                                     799
C    *                                                                    799
6031  CALL AGSPOPB                                                        800
      GO TO BLOCK_ICCD, (6030 ,6040)                                      800
C End of Local Procedure                                                  800
C ----------------------------------------------------------------------  801
C ------------ IBCF                                                       801
C Begin Local Procedure IBCF                                              802
C IBCF MOTHER OF THE CFOAM ASSEMBLY                                       802
5580  CONTINUE                                                            802
         AG_TITLE='IBCF'                                                  802
         AG_MODULE=MODULE                                                 802
         AG_MARK='V00'                                                    802
         CALL AGSPUSH                                                     802
         AG_MODULE=MODULE                                                 802
C    *                                                                    802
C    Geant MATERIAL : AIR ===> inherited arguments used <===              803
         CALL AGSRESET                                                    803
         AG_TITLE = 'AIR'                                                 803
         AG_EXNAME = 'MATERIAL'                                           803
         AG_PARLIST = 'NONE'                                              803
         CALL AXMATERIAL                                                  803
C    *                                                                    803
C    Geant ATTRIBUTE : IBCF ===> SEEN=0 COLO=1 <===                       803
         CALL AGSRESET                                                    804
         AG_SEEN = 0                                                      804
         AG_COLO = 1                                                      804
         AG_TITLE = 'IBCF'                                                804
         AG_EXNAME = 'ATTRIBUTE'                                          804
         AG_PARLIST = 'SEEN_COLO'                                         804
         CALL AXATTRIBUTE                                                 804
C    *                                                                    804
C    Geant SHAPE : BOX ===> DX=HALFFOAMWDT DY=HALFFOAMTHK, DZ=HALFFOAMLE  804
C    N <===                                                               804
         CALL AGSRESET                                                    806
         AG_DX = HALFFOAMWDT                                              806
         AG_DY = HALFFOAMTHK                                              806
         AG_DZ = HALFFOAMLEN                                              806
         AG_TITLE = 'BOX'                                                 806
         AG_EXNAME = 'SHAPE'                                              806
         AG_PARLIST = 'DX_DY_DZ'                                          806
         CALL AXSHAPE                                                     806
C    *                                                                    806
C    Create ICFC :                                                        807
C    Call To Local Procedure ICFC                                         807
         ASSIGN 6060 TO BLOCK_ICFC                                        807
         GO TO  6050                                                      807
6060     CONTINUE                                                         807
C    *                                                                    807
C    Geant POSITION : ICFC ===> X=0.0 Y=0.0 Z=-HALFFOAMLEN+HALFFOAMLENCO  808
C    NNECT KONLY='MANY' <===                                              808
         CALL AGSRESET                                                    808
         AG_X = 0.0                                                       808
         AG_Y = 0.0                                                       808
         AG_Z = -HALFFOAMLEN+HALFFOAMLENCONNECT                           808
         AG_KONLY = 'MANY'                                                808
         AG_TITLE = 'ICFC'                                                808
         AG_EXNAME = 'POSITION'                                           808
         AG_PARLIST = 'X_Y_Z_KONLY'                                       808
         CALL AXPOSITION                                                  808
C    *                                                                    808
C    Create ICFD :                                                        809
C    Call To Local Procedure ICFD                                         809
         ASSIGN 6080 TO BLOCK_ICFD                                        809
         GO TO  6070                                                      809
6080     CONTINUE                                                         809
C    *                                                                    809
C    Geant POSITION : ICFD ===> X=0.0 Y=0.0 Z=HALFFOAMLENCONNECT KONLY='  810
C    *MANY' <===                                                          810
         CALL AGSRESET                                                    810
         AG_X = 0.0                                                       810
         AG_Y = 0.0                                                       810
         AG_Z = HALFFOAMLENCONNECT                                        810
         AG_KONLY = 'MANY'                                                810
         AG_TITLE = 'ICFD'                                                810
         AG_EXNAME = 'POSITION'                                           810
         AG_PARLIST = 'X_Y_Z_KONLY'                                       810
         CALL AXPOSITION                                                  810
C    *                                                                    810
C    Create ICFB :                                                        811
C    Call To Local Procedure ICFB                                         811
         ASSIGN 6100 TO BLOCK_ICFB                                        811
         GO TO  6090                                                      811
6100     CONTINUE                                                         811
C    *                                                                    811
C    Geant POSITION : ICFB ===> X=TUBERMAX Y=HALFFOAMTHK-0.2922/2.0 Z=0.  812
C    0 KONLY='ONLY' <===                                                  812
         CALL AGSRESET                                                    812
         AG_X = TUBERMAX                                                  812
         AG_Y = HALFFOAMTHK-0.2922/2.0                                    812
         AG_Z = 0.0                                                       812
         AG_KONLY = 'ONLY'                                                812
         AG_TITLE = 'ICFB'                                                812
         AG_EXNAME = 'POSITION'                                           812
         AG_PARLIST = 'X_Y_Z_KONLY'                                       812
         CALL AXPOSITION                                                  812
C    *                                                                    812
C    Create ICFA :                                                        813
C    Call To Local Procedure ICFA                                         813
         ASSIGN 6120 TO BLOCK_ICFA                                        813
         GO TO  6110                                                      813
6120     CONTINUE                                                         813
C    *                                                                    813
C    Geant POSITION : ICFA ===> X=TUBERMAX Y=HALFFOAMTHK-0.2922 Z=0.0 KO  814
C    NLY='ONLY' <===                                                      814
         CALL AGSRESET                                                    814
         AG_X = TUBERMAX                                                  814
         AG_Y = HALFFOAMTHK-0.2922                                        814
         AG_Z = 0.0                                                       814
         AG_KONLY = 'ONLY'                                                814
         AG_TITLE = 'ICFA'                                                814
         AG_EXNAME = 'POSITION'                                           814
         AG_PARLIST = 'X_Y_Z_KONLY'                                       814
         CALL AXPOSITION                                                  814
C    *                                                                    814
5581  CALL AGSPOPB                                                        815
      GO TO BLOCK_IBCF, (5580 ,5590)                                      815
C End of Local Procedure                                                  815
C ----------------------------------------------------------------------  816
C ------------ ICFC                                                       816
C Begin Local Procedure ICFC                                              817
C ICFC IS THE IST CARBON FOAM BOX VOLUME ON EAST SIDE                     817
6050  CONTINUE                                                            817
         AG_TITLE='ICFC'                                                  817
         AG_MODULE=MODULE                                                 817
         AG_MARK='V00'                                                    817
         CALL AGSPUSH                                                     817
         AG_MODULE=MODULE                                                 817
C    *                                                                    817
C    Geant MATERIAL : CARBONFOAM ===> inherited arguments used <===       818
         CALL AGSRESET                                                    818
         AG_TITLE = 'CARBONFOAM'                                          818
         AG_EXNAME = 'MATERIAL'                                           818
         AG_PARLIST = 'NONE'                                              818
         CALL AXMATERIAL                                                  818
C    *                                                                    818
C    Geant ATTRIBUTE : ICFC ===> SEEN=1 COLO=1 <===                       818
         CALL AGSRESET                                                    819
         AG_SEEN = 1                                                      819
         AG_COLO = 1                                                      819
         AG_TITLE = 'ICFC'                                                819
         AG_EXNAME = 'ATTRIBUTE'                                          819
         AG_PARLIST = 'SEEN_COLO'                                         819
         CALL AXATTRIBUTE                                                 819
C    *                                                                    819
C    Geant SHAPE : BOX ===> DX=HALFFOAMWDT DY=HALFFOAMTHKCONNECT, DZ=HAL  819
C    FFOAMLENCONNECT <===                                                 819
         CALL AGSRESET                                                    821
         AG_DX = HALFFOAMWDT                                              821
         AG_DY = HALFFOAMTHKCONNECT                                       821
         AG_DZ = HALFFOAMLENCONNECT                                       821
         AG_TITLE = 'BOX'                                                 821
         AG_EXNAME = 'SHAPE'                                              821
         AG_PARLIST = 'DX_DY_DZ'                                          821
         CALL AXSHAPE                                                     821
C    *                                                                    821
6051  CALL AGSPOPB                                                        822
      GO TO BLOCK_ICFC, (6050 ,6060)                                      822
C End of Local Procedure                                                  822
C ----------------------------------------------------------------------  823
C ------------ ICFD                                                       823
C Begin Local Procedure ICFD                                              824
C ICFD IS THE IST CARBON FOAM BOX VOLUME ON WEST SIDE                     824
6070  CONTINUE                                                            824
         AG_TITLE='ICFD'                                                  824
         AG_MODULE=MODULE                                                 824
         AG_MARK='V00'                                                    824
         CALL AGSPUSH                                                     824
         AG_MODULE=MODULE                                                 824
C    *                                                                    824
C    Geant MATERIAL : CARBONFOAM ===> inherited arguments used <===       825
         CALL AGSRESET                                                    825
         AG_TITLE = 'CARBONFOAM'                                          825
         AG_EXNAME = 'MATERIAL'                                           825
         AG_PARLIST = 'NONE'                                              825
         CALL AXMATERIAL                                                  825
C    *                                                                    825
C    Geant ATTRIBUTE : ICFD ===> SEEN=1 COLO=1 <===                       825
         CALL AGSRESET                                                    826
         AG_SEEN = 1                                                      826
         AG_COLO = 1                                                      826
         AG_TITLE = 'ICFD'                                                826
         AG_EXNAME = 'ATTRIBUTE'                                          826
         AG_PARLIST = 'SEEN_COLO'                                         826
         CALL AXATTRIBUTE                                                 826
C    *                                                                    826
C    Geant SHAPE : BOX ===> DX=HALFFOAMWDT DY=HALFFOAMTHK, DZ=HALFFOAMLE  826
C    N-HALFFOAMLENCONNECT <===                                            826
         CALL AGSRESET                                                    828
         AG_DX = HALFFOAMWDT                                              828
         AG_DY = HALFFOAMTHK                                              828
         AG_DZ = HALFFOAMLEN-HALFFOAMLENCONNECT                           828
         AG_TITLE = 'BOX'                                                 828
         AG_EXNAME = 'SHAPE'                                              828
         AG_PARLIST = 'DX_DY_DZ'                                          828
         CALL AXSHAPE                                                     828
C    *                                                                    828
6071  CALL AGSPOPB                                                        829
      GO TO BLOCK_ICFD, (6070 ,6080)                                      829
C End of Local Procedure                                                  829
C ----------------------------------------------------------------------  830
C ------------ ICFB                                                       830
C Begin Local Procedure ICFB                                              831
C ICFB IS THE CFOAM AIR GROOVE BOX AREA                                   831
6090  CONTINUE                                                            831
         AG_TITLE='ICFB'                                                  831
         AG_MODULE=MODULE                                                 831
         AG_MARK='V00'                                                    831
         CALL AGSPUSH                                                     831
         AG_MODULE=MODULE                                                 831
C    *                                                                    831
C    Geant MATERIAL : AIR ===> inherited arguments used <===              832
         CALL AGSRESET                                                    832
         AG_TITLE = 'AIR'                                                 832
         AG_EXNAME = 'MATERIAL'                                           832
         AG_PARLIST = 'NONE'                                              832
         CALL AXMATERIAL                                                  832
C    *                                                                    832
C    Geant ATTRIBUTE : ICFB ===> SEEN=1 COLO=1 <===                       832
         CALL AGSRESET                                                    833
         AG_SEEN = 1                                                      833
         AG_COLO = 1                                                      833
         AG_TITLE = 'ICFB'                                                833
         AG_EXNAME = 'ATTRIBUTE'                                          833
         AG_PARLIST = 'SEEN_COLO'                                         833
         CALL AXATTRIBUTE                                                 833
C    *                                                                    833
C    Geant SHAPE : BOX ===> DX=TUBERMAX DY=0.2922/2.0 DZ=HALFFOAMLEN <==  833
C    =                                                                    833
         CALL AGSRESET                                                    834
         AG_DX = TUBERMAX                                                 834
         AG_DY = 0.2922/2.0                                               834
         AG_DZ = HALFFOAMLEN                                              834
         AG_TITLE = 'BOX'                                                 834
         AG_EXNAME = 'SHAPE'                                              834
         AG_PARLIST = 'DX_DY_DZ'                                          834
         CALL AXSHAPE                                                     834
C    *                                                                    834
6091  CALL AGSPOPB                                                        835
      GO TO BLOCK_ICFB, (6090 ,6100)                                      835
C End of Local Procedure                                                  835
C ----------------------------------------------------------------------  836
C ------------ ICFA                                                       836
C Begin Local Procedure ICFA                                              837
C ICFA IS THE CFOAM AIR GROOVE ARC AREA                                   837
6110  CONTINUE                                                            837
         AG_TITLE='ICFA'                                                  837
         AG_MODULE=MODULE                                                 837
         AG_MARK='V00'                                                    837
         CALL AGSPUSH                                                     837
         AG_MODULE=MODULE                                                 837
C    *                                                                    837
C    Geant MATERIAL : AIR ===> inherited arguments used <===              838
         CALL AGSRESET                                                    838
         AG_TITLE = 'AIR'                                                 838
         AG_EXNAME = 'MATERIAL'                                           838
         AG_PARLIST = 'NONE'                                              838
         CALL AXMATERIAL                                                  838
C    *                                                                    838
C    Geant ATTRIBUTE : ICFA ===> SEEN=1 COLO=1 <===                       838
         CALL AGSRESET                                                    839
         AG_SEEN = 1                                                      839
         AG_COLO = 1                                                      839
         AG_TITLE = 'ICFA'                                                839
         AG_EXNAME = 'ATTRIBUTE'                                          839
         AG_PARLIST = 'SEEN_COLO'                                         839
         CALL AXATTRIBUTE                                                 839
C    *                                                                    839
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=TUBERMAX PHI1=180.0, PHI2=360.0  839
C     DZ=HALFFOAMLEN <===                                                 839
         CALL AGSRESET                                                    841
         AG_RMIN = 0                                                      841
         AG_RMAX = TUBERMAX                                               841
         AG_PHI1 = 180.0                                                  841
         AG_PHI2 = 360.0                                                  841
         AG_DZ = HALFFOAMLEN                                              841
         AG_TITLE = 'TUBS'                                                841
         AG_EXNAME = 'SHAPE'                                              841
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            841
         CALL AXSHAPE                                                     841
C    *                                                                    841
6111  CALL AGSPOPB                                                        842
      GO TO BLOCK_ICFA, (6110 ,6120)                                      842
C End of Local Procedure                                                  842
C ----------------------------------------------------------------------  843
C ------------ IBAT                                                       843
C Begin Local Procedure IBAT                                              844
C IBAT ALUMINIUM COOLING TUBE MOTHER VOLUME                               844
5700  CONTINUE                                                            844
         AG_TITLE='IBAT'                                                  844
         AG_MODULE=MODULE                                                 844
         AG_MARK='V00'                                                    844
         CALL AGSPUSH                                                     844
         AG_MODULE=MODULE                                                 844
C    *                                                                    844
C    Geant ATTRIBUTE : IBAT ===> SEEN=0 COLO=1 <===                       845
         CALL AGSRESET                                                    845
         AG_SEEN = 0                                                      845
         AG_COLO = 1                                                      845
         AG_TITLE = 'IBAT'                                                845
         AG_EXNAME = 'ATTRIBUTE'                                          845
         AG_PARLIST = 'SEEN_COLO'                                         845
         CALL AXATTRIBUTE                                                 845
C    *                                                                    845
C    Geant MATERIAL : AIR ===> inherited arguments used <===              845
         CALL AGSRESET                                                    846
         AG_TITLE = 'AIR'                                                 846
         AG_EXNAME = 'MATERIAL'                                           846
         AG_PARLIST = 'NONE'                                              846
         CALL AXMATERIAL                                                  846
C    *                                                                    846
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=TUBECONEOUTERRMAX, DZ=HALFTUBEL  846
C    EN <===                                                              846
         CALL AGSRESET                                                    848
         AG_RMIN = 0                                                      848
         AG_RMAX = TUBECONEOUTERRMAX                                      848
         AG_DZ = HALFTUBELEN                                              848
         AG_TITLE = 'TUBE'                                                848
         AG_EXNAME = 'SHAPE'                                              848
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      848
         CALL AXSHAPE                                                     848
C    *                                                                    848
C    Create ICTC :                                                        849
C    Call To Local Procedure ICTC                                         849
         ASSIGN 6140 TO BLOCK_ICTC                                        849
         GO TO  6130                                                      849
6140     CONTINUE                                                         849
C    *                                                                    849
C    Geant POSITION : ICTC ===> X=0.0 Y=0.0 Z=0.0 <===                    850
         CALL AGSRESET                                                    850
         AG_X = 0.0                                                       850
         AG_Y = 0.0                                                       850
         AG_Z = 0.0                                                       850
         AG_TITLE = 'ICTC'                                                850
         AG_EXNAME = 'POSITION'                                           850
         AG_PARLIST = 'X_Y_Z'                                             850
         CALL AXPOSITION                                                  850
C    *                                                                    850
C    Create ICTE :                                                        851
C    Call To Local Procedure ICTE                                         851
         ASSIGN 6160 TO BLOCK_ICTE                                        851
         GO TO  6150                                                      851
6160     CONTINUE                                                         851
C    *                                                                    851
C    Geant POSITION : ICTE ===> X=0.0 Y=0.0 Z=-HALFTUBELEN+HALFTUBECONEL  852
C    EN <===                                                              852
         CALL AGSRESET                                                    852
         AG_X = 0.0                                                       852
         AG_Y = 0.0                                                       852
         AG_Z = -HALFTUBELEN+HALFTUBECONELEN                              852
         AG_TITLE = 'ICTE'                                                852
         AG_EXNAME = 'POSITION'                                           852
         AG_PARLIST = 'X_Y_Z'                                             852
         CALL AXPOSITION                                                  852
C    *                                                                    852
C    Create ICTW :                                                        853
C    Call To Local Procedure ICTW                                         853
         ASSIGN 6180 TO BLOCK_ICTW                                        853
         GO TO  6170                                                      853
6180     CONTINUE                                                         853
C    *                                                                    853
C    Geant POSITION : ICTW ===> X=0.0 Y=0.0 Z=HALFTUBELEN-HALFTUBECONELE  854
C    N <===                                                               854
         CALL AGSRESET                                                    854
         AG_X = 0.0                                                       854
         AG_Y = 0.0                                                       854
         AG_Z = HALFTUBELEN-HALFTUBECONELEN                               854
         AG_TITLE = 'ICTW'                                                854
         AG_EXNAME = 'POSITION'                                           854
         AG_PARLIST = 'X_Y_Z'                                             854
         CALL AXPOSITION                                                  854
C    *                                                                    854
5701  CALL AGSPOPB                                                        855
      GO TO BLOCK_IBAT, (5700 ,5710)                                      855
C End of Local Procedure                                                  855
C ----------------------------------------------------------------------  856
C ------------ ICTC                                                       856
C Begin Local Procedure ICTC                                              857
C ICTC ALUMINIUM COOLING TUBE CYLINDER                                    857
6130  CONTINUE                                                            857
         AG_TITLE='ICTC'                                                  857
         AG_MODULE=MODULE                                                 857
         AG_MARK='V00'                                                    857
         CALL AGSPUSH                                                     857
         AG_MODULE=MODULE                                                 857
C    *                                                                    857
C    Geant ATTRIBUTE : ICTC ===> SEEN=1 COLO=4 <===                       858
         CALL AGSRESET                                                    858
         AG_SEEN = 1                                                      858
         AG_COLO = 4                                                      858
         AG_TITLE = 'ICTC'                                                858
         AG_EXNAME = 'ATTRIBUTE'                                          858
         AG_PARLIST = 'SEEN_COLO'                                         858
         CALL AXATTRIBUTE                                                 858
C    *                                                                    858
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        858
         CALL AGSRESET                                                    859
         AG_TITLE = 'ALUMINIUM'                                           859
         AG_EXNAME = 'MATERIAL'                                           859
         AG_PARLIST = 'NONE'                                              859
         CALL AXMATERIAL                                                  859
C    *                                                                    859
C    Geant SHAPE : TUBE ===> RMIN=TUBERMIN RMAX=TUBERMAX DZ=HALFTUBELEN-  859
C    HALFTUBECONELEN <===                                                 859
         CALL AGSRESET                                                    860
         AG_RMIN = TUBERMIN                                               860
         AG_RMAX = TUBERMAX                                               860
         AG_DZ = HALFTUBELEN-HALFTUBECONELEN                              860
         AG_TITLE = 'TUBE'                                                860
         AG_EXNAME = 'SHAPE'                                              860
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      860
         CALL AXSHAPE                                                     860
C    *                                                                    860
6131  CALL AGSPOPB                                                        861
      GO TO BLOCK_ICTC, (6130 ,6140)                                      861
C End of Local Procedure                                                  861
C ----------------------------------------------------------------------  862
C ------------ ICTE                                                       862
C Begin Local Procedure ICTE                                              863
C ICTE ALUMINIUM COOLING TUBE CONE CONNECTOR EAST SIDE                    863
6150  CONTINUE                                                            863
         AG_TITLE='ICTE'                                                  863
         AG_MODULE=MODULE                                                 863
         AG_MARK='V00'                                                    863
         CALL AGSPUSH                                                     863
         AG_MODULE=MODULE                                                 863
C    *                                                                    863
C    Geant ATTRIBUTE : ICTE ===> SEEN=1 COLO=4 <===                       864
         CALL AGSRESET                                                    864
         AG_SEEN = 1                                                      864
         AG_COLO = 4                                                      864
         AG_TITLE = 'ICTE'                                                864
         AG_EXNAME = 'ATTRIBUTE'                                          864
         AG_PARLIST = 'SEEN_COLO'                                         864
         CALL AXATTRIBUTE                                                 864
C    *                                                                    864
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        864
         CALL AGSRESET                                                    865
         AG_TITLE = 'ALUMINIUM'                                           865
         AG_EXNAME = 'MATERIAL'                                           865
         AG_PARLIST = 'NONE'                                              865
         CALL AXMATERIAL                                                  865
C    *                                                                    865
C    Geant SHAPE : CONE ===> DZ=HALFTUBECONELEN RMN1=TUBECONEOUTERRMIN,   865
C    RMX1=TUBECONEOUTERRMAX RMN2=TUBERMIN, RMX2=TUBERMAX <===             865
         CALL AGSRESET                                                    868
         AG_DZ = HALFTUBECONELEN                                          868
         AG_RMN1 = TUBECONEOUTERRMIN                                      868
         AG_RMX1 = TUBECONEOUTERRMAX                                      868
         AG_RMN2 = TUBERMIN                                               868
         AG_RMX2 = TUBERMAX                                               868
         AG_TITLE = 'CONE'                                                868
         AG_EXNAME = 'SHAPE'                                              868
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            868
         CALL AXSHAPE                                                     868
C    *                                                                    868
6151  CALL AGSPOPB                                                        869
      GO TO BLOCK_ICTE, (6150 ,6160)                                      869
C End of Local Procedure                                                  869
C ----------------------------------------------------------------------  870
C ------------ ICTW                                                       870
C Begin Local Procedure ICTW                                              871
C ICTW ALUMINIUM COOLING TUBE CONE CONNECTOR WEST SIDE                    871
6170  CONTINUE                                                            871
         AG_TITLE='ICTW'                                                  871
         AG_MODULE=MODULE                                                 871
         AG_MARK='V00'                                                    871
         CALL AGSPUSH                                                     871
         AG_MODULE=MODULE                                                 871
C    *                                                                    871
C    Geant ATTRIBUTE : ICTW ===> SEEN=1 COLO=4 <===                       872
         CALL AGSRESET                                                    872
         AG_SEEN = 1                                                      872
         AG_COLO = 4                                                      872
         AG_TITLE = 'ICTW'                                                872
         AG_EXNAME = 'ATTRIBUTE'                                          872
         AG_PARLIST = 'SEEN_COLO'                                         872
         CALL AXATTRIBUTE                                                 872
C    *                                                                    872
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        872
         CALL AGSRESET                                                    873
         AG_TITLE = 'ALUMINIUM'                                           873
         AG_EXNAME = 'MATERIAL'                                           873
         AG_PARLIST = 'NONE'                                              873
         CALL AXMATERIAL                                                  873
C    *                                                                    873
C    Geant SHAPE : CONE ===> DZ=HALFTUBECONELEN RMN1=TUBERMIN, RMX1=TUBE  873
C    RMAX RMN2=TUBECONEOUTERRMIN RMX2=TUBECONEOUTERRMAX <===              873
         CALL AGSRESET                                                    875
         AG_DZ = HALFTUBECONELEN                                          875
         AG_RMN1 = TUBERMIN                                               875
         AG_RMX1 = TUBERMAX                                               875
         AG_RMN2 = TUBECONEOUTERRMIN                                      875
         AG_RMX2 = TUBECONEOUTERRMAX                                      875
         AG_TITLE = 'CONE'                                                875
         AG_EXNAME = 'SHAPE'                                              875
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            875
         CALL AXSHAPE                                                     875
C    *                                                                    875
6171  CALL AGSPOPB                                                        876
      GO TO BLOCK_ICTW, (6170 ,6180)                                      876
C End of Local Procedure                                                  876
C ----------------------------------------------------------------------  877
C ------------ IBCW                                                       877
C Begin Local Procedure IBCW                                              878
C IBCW COOLING LIQUID IN ALUMNIUM TUBE                                    878
5720  CONTINUE                                                            878
         AG_TITLE='IBCW'                                                  878
         AG_MODULE=MODULE                                                 878
         AG_MARK='V00'                                                    878
         CALL AGSPUSH                                                     878
         AG_MODULE=MODULE                                                 878
C    *                                                                    878
C    Geant ATTRIBUTE : IBCW ===> SEEN=1 COLO=920 <===                     879
         CALL AGSRESET                                                    879
         AG_SEEN = 1                                                      879
         AG_COLO = 920                                                    879
         AG_TITLE = 'IBCW'                                                879
         AG_EXNAME = 'ATTRIBUTE'                                          879
         AG_PARLIST = 'SEEN_COLO'                                         879
         CALL AXATTRIBUTE                                                 879
C    *                                                                    879
C    Geant MATERIAL : NOVEC7200 ===> inherited arguments used <===        879
         CALL AGSRESET                                                    880
         AG_TITLE = 'NOVEC7200'                                           880
         AG_EXNAME = 'MATERIAL'                                           880
         AG_PARLIST = 'NONE'                                              880
         CALL AXMATERIAL                                                  880
C    *                                                                    880
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=TUBERMIN DZ=HALFTUBELEN <===   880
         CALL AGSRESET                                                    881
         AG_RMIN = 0.0                                                    881
         AG_RMAX = TUBERMIN                                               881
         AG_DZ = HALFTUBELEN                                              881
         AG_TITLE = 'TUBE'                                                881
         AG_EXNAME = 'SHAPE'                                              881
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      881
         CALL AXSHAPE                                                     881
C    *                                                                    881
5721  CALL AGSPOPB                                                        882
      GO TO BLOCK_IBCW, (5720 ,5730)                                      882
C End of Local Procedure                                                  882
C ----------------------------------------------------------------------  883
C ------------ ILST                                                       883
C Begin Local Procedure ILST                                              884
C ILST CF SKIN TOP LAYER MOTHER VOLUME                                    884
5600  CONTINUE                                                            884
         AG_TITLE='ILST'                                                  884
         AG_MODULE=MODULE                                                 884
         AG_MARK='V00'                                                    884
         CALL AGSPUSH                                                     884
         AG_MODULE=MODULE                                                 884
C    *                                                                    884
C    Geant ATTRIBUTE : ILST ===> SEEN=0 COLO=1 <===                       885
         CALL AGSRESET                                                    885
         AG_SEEN = 0                                                      885
         AG_COLO = 1                                                      885
         AG_TITLE = 'ILST'                                                885
         AG_EXNAME = 'ATTRIBUTE'                                          885
         AG_PARLIST = 'SEEN_COLO'                                         885
         CALL AXATTRIBUTE                                                 885
C    *                                                                    885
C    Geant MATERIAL : AIR ===> inherited arguments used <===              885
         CALL AGSRESET                                                    886
         AG_TITLE = 'AIR'                                                 886
         AG_EXNAME = 'MATERIAL'                                           886
         AG_PARLIST = 'NONE'                                              886
         CALL AXMATERIAL                                                  886
C    *                                                                    886
C    Geant SHAPE : BOX ===> DX=HALFSKINWDT DY=HALFTHKSKINMOM, DZ=HALFSKI  886
C    NLEN <===                                                            886
         CALL AGSRESET                                                    888
         AG_DX = HALFSKINWDT                                              888
         AG_DY = HALFTHKSKINMOM                                           888
         AG_DZ = HALFSKINLEN                                              888
         AG_TITLE = 'BOX'                                                 888
         AG_EXNAME = 'SHAPE'                                              888
         AG_PARLIST = 'DX_DY_DZ'                                          888
         CALL AXSHAPE                                                     888
C    *                                                                    888
C    Create ISTC :                                                        889
C    Call To Local Procedure ISTC                                         889
         ASSIGN 6200 TO BLOCK_ISTC                                        889
         GO TO  6190                                                      889
6200     CONTINUE                                                         889
C    *                                                                    889
C    Geant POSITION : ISTC ===> X=0.0 Y=-HALFTHKSKINMOM+HALFSKINTHK Z=HA  891
C    LFLENSKINCONNECT-HALFSKINLEN <===                                    891
         CALL AGSRESET                                                    891
         AG_X = 0.0                                                       891
         AG_Y = -HALFTHKSKINMOM+HALFSKINTHK                               891
         AG_Z = HALFLENSKINCONNECT-HALFSKINLEN                            891
         AG_TITLE = 'ISTC'                                                891
         AG_EXNAME = 'POSITION'                                           891
         AG_PARLIST = 'X_Y_Z'                                             891
         CALL AXPOSITION                                                  891
C    *                                                                    891
C    Create ISTH :                                                        892
C    Call To Local Procedure ISTH                                         892
         ASSIGN 6220 TO BLOCK_ISTH                                        892
         GO TO  6210                                                      892
6220     CONTINUE                                                         892
C    *                                                                    892
C    Geant POSITION : ISTH ===> X=0.0 Y=-HALFSKINTHK+HALFTHKSKINMOM Z=HA  893
C    LFLENSKINCONNECT <===                                                893
         CALL AGSRESET                                                    893
         AG_X = 0.0                                                       893
         AG_Y = -HALFSKINTHK+HALFTHKSKINMOM                               893
         AG_Z = HALFLENSKINCONNECT                                        893
         AG_TITLE = 'ISTH'                                                893
         AG_EXNAME = 'POSITION'                                           893
         AG_PARLIST = 'X_Y_Z'                                             893
         CALL AXPOSITION                                                  893
C    *                                                                    893
5601  CALL AGSPOPB                                                        894
      GO TO BLOCK_ILST, (5600 ,5610)                                      894
C End of Local Procedure                                                  894
C ----------------------------------------------------------------------  895
C ------------ ISTC                                                       895
C Begin Local Procedure ISTC                                              897
C ISTC IS THE CF SKIN BOX VOLUME ON CONNECTER SIDE IN TOP CF SKIN MOTHER  897
C , VOLUME                                                                897
6190  CONTINUE                                                            897
         AG_TITLE='ISTC'                                                  897
         AG_MODULE=MODULE                                                 897
         AG_MARK='V00'                                                    897
         CALL AGSPUSH                                                     897
         AG_MODULE=MODULE                                                 897
C    *                                                                    897
C    Geant MATERIAL : CFRPMIX2 ===> inherited arguments used <===         898
         CALL AGSRESET                                                    898
         AG_TITLE = 'CFRPMIX2'                                            898
         AG_EXNAME = 'MATERIAL'                                           898
         AG_PARLIST = 'NONE'                                              898
         CALL AXMATERIAL                                                  898
C    *                                                                    898
C    Geant ATTRIBUTE : ISTC ===> SEEN=1 COLO=3 <===                       898
         CALL AGSRESET                                                    899
         AG_SEEN = 1                                                      899
         AG_COLO = 3                                                      899
         AG_TITLE = 'ISTC'                                                899
         AG_EXNAME = 'ATTRIBUTE'                                          899
         AG_PARLIST = 'SEEN_COLO'                                         899
         CALL AXATTRIBUTE                                                 899
C    *                                                                    899
C    Geant SHAPE : BOX ===> DX=HALFSKINWDT DY=HALFSKINTHK, DZ=HALFLENSKI  899
C    NCONNECT <===                                                        899
         CALL AGSRESET                                                    901
         AG_DX = HALFSKINWDT                                              901
         AG_DY = HALFSKINTHK                                              901
         AG_DZ = HALFLENSKINCONNECT                                       901
         AG_TITLE = 'BOX'                                                 901
         AG_EXNAME = 'SHAPE'                                              901
         AG_PARLIST = 'DX_DY_DZ'                                          901
         CALL AXSHAPE                                                     901
C    *                                                                    901
6191  CALL AGSPOPB                                                        902
      GO TO BLOCK_ISTC, (6190 ,6200)                                      902
C End of Local Procedure                                                  902
C ----------------------------------------------------------------------  903
C ------------ ISTH                                                       903
C Begin Local Procedure ISTH                                              904
C ISTH IS THE CF SKIN BOX VOLUME ON WEST SIDE IN TOP CF SKIN MOTHER VOLU  904
C ME                                                                      904
6210  CONTINUE                                                            904
         AG_TITLE='ISTH'                                                  904
         AG_MODULE=MODULE                                                 904
         AG_MARK='V00'                                                    904
         CALL AGSPUSH                                                     904
         AG_MODULE=MODULE                                                 904
C    *                                                                    904
C    Geant MATERIAL : CFRPMIX2 ===> inherited arguments used <===         905
         CALL AGSRESET                                                    905
         AG_TITLE = 'CFRPMIX2'                                            905
         AG_EXNAME = 'MATERIAL'                                           905
         AG_PARLIST = 'NONE'                                              905
         CALL AXMATERIAL                                                  905
C    *                                                                    905
C    Geant ATTRIBUTE : ISTH ===> SEEN=1 COLO=3 <===                       905
         CALL AGSRESET                                                    906
         AG_SEEN = 1                                                      906
         AG_COLO = 3                                                      906
         AG_TITLE = 'ISTH'                                                906
         AG_EXNAME = 'ATTRIBUTE'                                          906
         AG_PARLIST = 'SEEN_COLO'                                         906
         CALL AXATTRIBUTE                                                 906
C    *                                                                    906
C    Geant SHAPE : BOX ===> DX=HALFSKINWDT DY=HALFSKINTHK, DZ=HALFSKINLE  906
C    N-HALFLENSKINCONNECT <===                                            906
         CALL AGSRESET                                                    908
         AG_DX = HALFSKINWDT                                              908
         AG_DY = HALFSKINTHK                                              908
         AG_DZ = HALFSKINLEN-HALFLENSKINCONNECT                           908
         AG_TITLE = 'BOX'                                                 908
         AG_EXNAME = 'SHAPE'                                              908
         AG_PARLIST = 'DX_DY_DZ'                                          908
         CALL AXSHAPE                                                     908
C    *                                                                    908
6211  CALL AGSPOPB                                                        909
      GO TO BLOCK_ISTH, (6210 ,6220)                                      909
C End of Local Procedure                                                  909
C ----------------------------------------------------------------------  910
C ------------ ILSB                                                       910
C Begin Local Procedure ILSB                                              911
C ILSB CF SKIN BOTTOM LAYER MOTHER VOLUME                                 911
5620  CONTINUE                                                            911
         AG_TITLE='ILSB'                                                  911
         AG_MODULE=MODULE                                                 911
         AG_MARK='V00'                                                    911
         CALL AGSPUSH                                                     911
         AG_MODULE=MODULE                                                 911
C    *                                                                    911
C    Geant ATTRIBUTE : ILSB ===> SEEN=0 COLO=1 <===                       912
         CALL AGSRESET                                                    912
         AG_SEEN = 0                                                      912
         AG_COLO = 1                                                      912
         AG_TITLE = 'ILSB'                                                912
         AG_EXNAME = 'ATTRIBUTE'                                          912
         AG_PARLIST = 'SEEN_COLO'                                         912
         CALL AXATTRIBUTE                                                 912
C    *                                                                    912
C    Geant MATERIAL : AIR ===> inherited arguments used <===              912
         CALL AGSRESET                                                    913
         AG_TITLE = 'AIR'                                                 913
         AG_EXNAME = 'MATERIAL'                                           913
         AG_PARLIST = 'NONE'                                              913
         CALL AXMATERIAL                                                  913
C    *                                                                    913
C    Geant SHAPE : BOX ===> DX=HALFSKINWDT DY=HALFTHKSKINMOM, DZ=HALFSKI  913
C    NLEN <===                                                            913
         CALL AGSRESET                                                    915
         AG_DX = HALFSKINWDT                                              915
         AG_DY = HALFTHKSKINMOM                                           915
         AG_DZ = HALFSKINLEN                                              915
         AG_TITLE = 'BOX'                                                 915
         AG_EXNAME = 'SHAPE'                                              915
         AG_PARLIST = 'DX_DY_DZ'                                          915
         CALL AXSHAPE                                                     915
C    *                                                                    915
C    Create ISBC :                                                        916
C    Call To Local Procedure ISBC                                         916
         ASSIGN 6240 TO BLOCK_ISBC                                        916
         GO TO  6230                                                      916
6240     CONTINUE                                                         916
C    *                                                                    916
C    Geant POSITION : ISBC ===> X=0.0 Y=HALFTHKSKINMOM-HALFSKINTHK Z=HAL  918
C    FLENSKINCONNECT-HALFSKINLEN <===                                     918
         CALL AGSRESET                                                    918
         AG_X = 0.0                                                       918
         AG_Y = HALFTHKSKINMOM-HALFSKINTHK                                918
         AG_Z = HALFLENSKINCONNECT-HALFSKINLEN                            918
         AG_TITLE = 'ISBC'                                                918
         AG_EXNAME = 'POSITION'                                           918
         AG_PARLIST = 'X_Y_Z'                                             918
         CALL AXPOSITION                                                  918
C    *                                                                    918
C    Create ISBH :                                                        919
C    Call To Local Procedure ISBH                                         919
         ASSIGN 6260 TO BLOCK_ISBH                                        919
         GO TO  6250                                                      919
6260     CONTINUE                                                         919
C    *                                                                    919
C    Geant POSITION : ISBH ===> X=0.0 Y=-HALFTHKSKINMOM+HALFSKINTHK Z=HA  920
C    LFLENSKINCONNECT <===                                                920
         CALL AGSRESET                                                    920
         AG_X = 0.0                                                       920
         AG_Y = -HALFTHKSKINMOM+HALFSKINTHK                               920
         AG_Z = HALFLENSKINCONNECT                                        920
         AG_TITLE = 'ISBH'                                                920
         AG_EXNAME = 'POSITION'                                           920
         AG_PARLIST = 'X_Y_Z'                                             920
         CALL AXPOSITION                                                  920
C    *                                                                    920
5621  CALL AGSPOPB                                                        921
      GO TO BLOCK_ILSB, (5620 ,5630)                                      921
C End of Local Procedure                                                  921
C ----------------------------------------------------------------------  922
C ------------ ISBC                                                       922
C Begin Local Procedure ISBC                                              924
C ISBC IS THE CF SKIN BOX VOLUME ON CONNECTER SIDE IN BOTTOM CF SKIN, MO  924
C THER VOLUME                                                             924
6230  CONTINUE                                                            924
         AG_TITLE='ISBC'                                                  924
         AG_MODULE=MODULE                                                 924
         AG_MARK='V00'                                                    924
         CALL AGSPUSH                                                     924
         AG_MODULE=MODULE                                                 924
C    *                                                                    924
C    Geant MATERIAL : CFRPMIX2 ===> inherited arguments used <===         925
         CALL AGSRESET                                                    925
         AG_TITLE = 'CFRPMIX2'                                            925
         AG_EXNAME = 'MATERIAL'                                           925
         AG_PARLIST = 'NONE'                                              925
         CALL AXMATERIAL                                                  925
C    *                                                                    925
C    Geant ATTRIBUTE : ISBC ===> SEEN=1 COLO=3 <===                       925
         CALL AGSRESET                                                    926
         AG_SEEN = 1                                                      926
         AG_COLO = 3                                                      926
         AG_TITLE = 'ISBC'                                                926
         AG_EXNAME = 'ATTRIBUTE'                                          926
         AG_PARLIST = 'SEEN_COLO'                                         926
         CALL AXATTRIBUTE                                                 926
C    *                                                                    926
C    Geant SHAPE : BOX ===> DX=HALFSKINWDT DY=HALFSKINTHK, DZ=HALFLENSKI  926
C    NCONNECT <===                                                        926
         CALL AGSRESET                                                    928
         AG_DX = HALFSKINWDT                                              928
         AG_DY = HALFSKINTHK                                              928
         AG_DZ = HALFLENSKINCONNECT                                       928
         AG_TITLE = 'BOX'                                                 928
         AG_EXNAME = 'SHAPE'                                              928
         AG_PARLIST = 'DX_DY_DZ'                                          928
         CALL AXSHAPE                                                     928
C    *                                                                    928
6231  CALL AGSPOPB                                                        929
      GO TO BLOCK_ISBC, (6230 ,6240)                                      929
C End of Local Procedure                                                  929
C ----------------------------------------------------------------------  930
C ------------ ISBH                                                       930
C Begin Local Procedure ISBH                                              932
C ISBH IS THE CF SKIN BOX VOLUME ON WEST SIDE IN BOTTOM CF SKIN MOTHER,   932
C VOLUME                                                                  932
6250  CONTINUE                                                            932
         AG_TITLE='ISBH'                                                  932
         AG_MODULE=MODULE                                                 932
         AG_MARK='V00'                                                    932
         CALL AGSPUSH                                                     932
         AG_MODULE=MODULE                                                 932
C    *                                                                    932
C    Geant MATERIAL : CFRPMIX2 ===> inherited arguments used <===         933
         CALL AGSRESET                                                    933
         AG_TITLE = 'CFRPMIX2'                                            933
         AG_EXNAME = 'MATERIAL'                                           933
         AG_PARLIST = 'NONE'                                              933
         CALL AXMATERIAL                                                  933
C    *                                                                    933
C    Geant ATTRIBUTE : ISBH ===> SEEN=1 COLO=3 <===                       933
         CALL AGSRESET                                                    934
         AG_SEEN = 1                                                      934
         AG_COLO = 3                                                      934
         AG_TITLE = 'ISBH'                                                934
         AG_EXNAME = 'ATTRIBUTE'                                          934
         AG_PARLIST = 'SEEN_COLO'                                         934
         CALL AXATTRIBUTE                                                 934
C    *                                                                    934
C    Geant SHAPE : BOX ===> DX=HALFSKINWDT DY=HALFSKINTHK, DZ=HALFSKINLE  934
C    N-HALFLENSKINCONNECT <===                                            934
         CALL AGSRESET                                                    936
         AG_DX = HALFSKINWDT                                              936
         AG_DY = HALFSKINTHK                                              936
         AG_DZ = HALFSKINLEN-HALFLENSKINCONNECT                           936
         AG_TITLE = 'BOX'                                                 936
         AG_EXNAME = 'SHAPE'                                              936
         AG_PARLIST = 'DX_DY_DZ'                                          936
         CALL AXSHAPE                                                     936
C    *                                                                    936
6251  CALL AGSPOPB                                                        937
      GO TO BLOCK_ISBH, (6250 ,6260)                                      937
C End of Local Procedure                                                  937
C ----------------------------------------------------------------------  938
C ------------ IBHT                                                       938
C Begin Local Procedure IBHT                                              939
C IBHT KAPTON HYBRID TOP LAYER MOTHER VOLUME                              939
5640  CONTINUE                                                            939
         AG_TITLE='IBHT'                                                  939
         AG_MODULE=MODULE                                                 939
         AG_MARK='V00'                                                    939
         CALL AGSPUSH                                                     939
         AG_MODULE=MODULE                                                 939
C    *                                                                    939
C    Geant ATTRIBUTE : IBHT ===> SEEN=0 COLO=1 <===                       940
         CALL AGSRESET                                                    940
         AG_SEEN = 0                                                      940
         AG_COLO = 1                                                      940
         AG_TITLE = 'IBHT'                                                940
         AG_EXNAME = 'ATTRIBUTE'                                          940
         AG_PARLIST = 'SEEN_COLO'                                         940
         CALL AXATTRIBUTE                                                 940
C    *                                                                    940
C    Geant MATERIAL : AIR ===> inherited arguments used <===              940
         CALL AGSRESET                                                    941
         AG_TITLE = 'AIR'                                                 941
         AG_EXNAME = 'MATERIAL'                                           941
         AG_PARLIST = 'NONE'                                              941
         CALL AXMATERIAL                                                  941
C    *                                                                    941
C    Geant SHAPE : BOX ===> DX=HALFHBDWDTTB DY=HALFHBDTHKTBCONNECT, DZ=H  941
C    ALFHBDLENTB <===                                                     941
         CALL AGSRESET                                                    943
         AG_DX = HALFHBDWDTTB                                             943
         AG_DY = HALFHBDTHKTBCONNECT                                      943
         AG_DZ = HALFHBDLENTB                                             943
         AG_TITLE = 'BOX'                                                 943
         AG_EXNAME = 'SHAPE'                                              943
         AG_PARLIST = 'DX_DY_DZ'                                          943
         CALL AXSHAPE                                                     943
C    *                                                                    943
C    Create IHTC :                                                        944
C    Call To Local Procedure IHTC                                         944
         ASSIGN 6280 TO BLOCK_IHTC                                        944
         GO TO  6270                                                      944
6280     CONTINUE                                                         944
C    *                                                                    944
C    Geant POSITION : IHTC ===> X=0.0 Y=0.0 Z=-HALFHBDLENTB+HALFHBDLENTB  945
C    CONNECT <===                                                         945
         CALL AGSRESET                                                    945
         AG_X = 0.0                                                       945
         AG_Y = 0.0                                                       945
         AG_Z = -HALFHBDLENTB+HALFHBDLENTBCONNECT                         945
         AG_TITLE = 'IHTC'                                                945
         AG_EXNAME = 'POSITION'                                           945
         AG_PARLIST = 'X_Y_Z'                                             945
         CALL AXPOSITION                                                  945
C    *                                                                    945
C    Create IHTH :                                                        946
C    Call To Local Procedure IHTH                                         946
         ASSIGN 6300 TO BLOCK_IHTH                                        946
         GO TO  6290                                                      946
6300     CONTINUE                                                         946
C    *                                                                    946
C    Geant POSITION : IHTH ===> X=0.0 Y=-HALFHBDTHKTB+HALFHBDTHKTBCONNEC  947
C    T Z=HALFHBDLENTBCONNECT <===                                         947
         CALL AGSRESET                                                    947
         AG_X = 0.0                                                       947
         AG_Y = -HALFHBDTHKTB+HALFHBDTHKTBCONNECT                         947
         AG_Z = HALFHBDLENTBCONNECT                                       947
         AG_TITLE = 'IHTH'                                                947
         AG_EXNAME = 'POSITION'                                           947
         AG_PARLIST = 'X_Y_Z'                                             947
         CALL AXPOSITION                                                  947
C    *                                                                    947
5641  CALL AGSPOPB                                                        948
      GO TO BLOCK_IBHT, (5640 ,5650)                                      948
C End of Local Procedure                                                  948
C ----------------------------------------------------------------------  949
C ------------ IHTC                                                       949
C Begin Local Procedure IHTC                                              950
C IHTC IS THE HYBRID ON CONNECTER SIDE IN TOP HYBRID MOTHER VOLUME        950
6270  CONTINUE                                                            950
         AG_TITLE='IHTC'                                                  950
         AG_MODULE=MODULE                                                 950
         AG_MARK='V00'                                                    950
         CALL AGSPUSH                                                     950
         AG_MODULE=MODULE                                                 950
C    *                                                                    950
C    Geant MATERIAL : KAPTON ===> inherited arguments used <===           951
         CALL AGSRESET                                                    951
         AG_TITLE = 'KAPTON'                                              951
         AG_EXNAME = 'MATERIAL'                                           951
         AG_PARLIST = 'NONE'                                              951
         CALL AXMATERIAL                                                  951
C    *                                                                    951
C    Geant ATTRIBUTE : IHTC ===> SEEN=1 COLO=2 <===                       951
         CALL AGSRESET                                                    952
         AG_SEEN = 1                                                      952
         AG_COLO = 2                                                      952
         AG_TITLE = 'IHTC'                                                952
         AG_EXNAME = 'ATTRIBUTE'                                          952
         AG_PARLIST = 'SEEN_COLO'                                         952
         CALL AXATTRIBUTE                                                 952
C    *                                                                    952
C    Geant SHAPE : BOX ===> DX=HALFHBDWDTTB DY=HALFHBDTHKTBCONNECT, DZ=H  952
C    ALFHBDLENTBCONNECT <===                                              952
         CALL AGSRESET                                                    954
         AG_DX = HALFHBDWDTTB                                             954
         AG_DY = HALFHBDTHKTBCONNECT                                      954
         AG_DZ = HALFHBDLENTBCONNECT                                      954
         AG_TITLE = 'BOX'                                                 954
         AG_EXNAME = 'SHAPE'                                              954
         AG_PARLIST = 'DX_DY_DZ'                                          954
         CALL AXSHAPE                                                     954
C    *                                                                    954
6271  CALL AGSPOPB                                                        955
      GO TO BLOCK_IHTC, (6270 ,6280)                                      955
C End of Local Procedure                                                  955
C ----------------------------------------------------------------------  956
C ------------ IHTH                                                       956
C Begin Local Procedure IHTH                                              957
C IHTH IS THE HYBIRD ON WEST SIDE IN TOP HYBRID MOTHER VOLUME             957
6290  CONTINUE                                                            957
         AG_TITLE='IHTH'                                                  957
         AG_MODULE=MODULE                                                 957
         AG_MARK='V00'                                                    957
         CALL AGSPUSH                                                     957
         AG_MODULE=MODULE                                                 957
C    *                                                                    957
C    Geant MATERIAL : KAPTON ===> inherited arguments used <===           958
         CALL AGSRESET                                                    958
         AG_TITLE = 'KAPTON'                                              958
         AG_EXNAME = 'MATERIAL'                                           958
         AG_PARLIST = 'NONE'                                              958
         CALL AXMATERIAL                                                  958
C    *                                                                    958
C    Geant ATTRIBUTE : IHTH ===> SEEN=1 COLO=2 <===                       958
         CALL AGSRESET                                                    959
         AG_SEEN = 1                                                      959
         AG_COLO = 2                                                      959
         AG_TITLE = 'IHTH'                                                959
         AG_EXNAME = 'ATTRIBUTE'                                          959
         AG_PARLIST = 'SEEN_COLO'                                         959
         CALL AXATTRIBUTE                                                 959
C    *                                                                    959
C    Geant SHAPE : BOX ===> DX=HALFHBDWDTTB DY=HALFHBDTHKTB, DZ=HALFHBDL  959
C    ENTB-HALFHBDLENTBCONNECT <===                                        959
         CALL AGSRESET                                                    961
         AG_DX = HALFHBDWDTTB                                             961
         AG_DY = HALFHBDTHKTB                                             961
         AG_DZ = HALFHBDLENTB-HALFHBDLENTBCONNECT                         961
         AG_TITLE = 'BOX'                                                 961
         AG_EXNAME = 'SHAPE'                                              961
         AG_PARLIST = 'DX_DY_DZ'                                          961
         CALL AXSHAPE                                                     961
C    *                                                                    961
6291  CALL AGSPOPB                                                        962
      GO TO BLOCK_IHTH, (6290 ,6300)                                      962
C End of Local Procedure                                                  962
C ----------------------------------------------------------------------  963
C ------------ IBHB                                                       963
C Begin Local Procedure IBHB                                              964
C IBHB KAPTON HYBRID BOTTOM LAYER MOTHER VOLUME                           964
5660  CONTINUE                                                            964
         AG_TITLE='IBHB'                                                  964
         AG_MODULE=MODULE                                                 964
         AG_MARK='V00'                                                    964
         CALL AGSPUSH                                                     964
         AG_MODULE=MODULE                                                 964
C    *                                                                    964
C    Geant ATTRIBUTE : IBHB ===> SEEN=0 COLO=1 <===                       965
         CALL AGSRESET                                                    965
         AG_SEEN = 0                                                      965
         AG_COLO = 1                                                      965
         AG_TITLE = 'IBHB'                                                965
         AG_EXNAME = 'ATTRIBUTE'                                          965
         AG_PARLIST = 'SEEN_COLO'                                         965
         CALL AXATTRIBUTE                                                 965
C    *                                                                    965
C    Geant MATERIAL : AIR ===> inherited arguments used <===              965
         CALL AGSRESET                                                    966
         AG_TITLE = 'AIR'                                                 966
         AG_EXNAME = 'MATERIAL'                                           966
         AG_PARLIST = 'NONE'                                              966
         CALL AXMATERIAL                                                  966
C    *                                                                    966
C    Geant SHAPE : BOX ===> DX=HALFHBDWDTTB DY=HALFHBDTHKTBCONNECT, DZ=H  966
C    ALFHBDLENTB <===                                                     966
         CALL AGSRESET                                                    968
         AG_DX = HALFHBDWDTTB                                             968
         AG_DY = HALFHBDTHKTBCONNECT                                      968
         AG_DZ = HALFHBDLENTB                                             968
         AG_TITLE = 'BOX'                                                 968
         AG_EXNAME = 'SHAPE'                                              968
         AG_PARLIST = 'DX_DY_DZ'                                          968
         CALL AXSHAPE                                                     968
C    *                                                                    968
C    Create IHBC :                                                        969
C    Call To Local Procedure IHBC                                         969
         ASSIGN 6320 TO BLOCK_IHBC                                        969
         GO TO  6310                                                      969
6320     CONTINUE                                                         969
C    *                                                                    969
C    Geant POSITION : IHBC ===> X=0.0 Y=0.0 Z=-HALFHBDLENTB+HALFHBDLENTB  970
C    CONNECT <===                                                         970
         CALL AGSRESET                                                    970
         AG_X = 0.0                                                       970
         AG_Y = 0.0                                                       970
         AG_Z = -HALFHBDLENTB+HALFHBDLENTBCONNECT                         970
         AG_TITLE = 'IHBC'                                                970
         AG_EXNAME = 'POSITION'                                           970
         AG_PARLIST = 'X_Y_Z'                                             970
         CALL AXPOSITION                                                  970
C    *                                                                    970
C    Create IHBH :                                                        971
C    Call To Local Procedure IHBH                                         971
         ASSIGN 6340 TO BLOCK_IHBH                                        971
         GO TO  6330                                                      971
6340     CONTINUE                                                         971
C    *                                                                    971
C    Geant POSITION : IHBH ===> X=0.0 Y=HALFHBDTHKTB-HALFHBDTHKTBCONNECT  972
C     Z=HALFHBDLENTBCONNECT <===                                          972
         CALL AGSRESET                                                    972
         AG_X = 0.0                                                       972
         AG_Y = HALFHBDTHKTB-HALFHBDTHKTBCONNECT                          972
         AG_Z = HALFHBDLENTBCONNECT                                       972
         AG_TITLE = 'IHBH'                                                972
         AG_EXNAME = 'POSITION'                                           972
         AG_PARLIST = 'X_Y_Z'                                             972
         CALL AXPOSITION                                                  972
C    *                                                                    972
5661  CALL AGSPOPB                                                        973
      GO TO BLOCK_IBHB, (5660 ,5670)                                      973
C End of Local Procedure                                                  973
C ----------------------------------------------------------------------  974
C ------------ IHBC                                                       974
C Begin Local Procedure IHBC                                              975
C IHBC IS THE HYBRID ON CONNECTER SIDE IN BOTTOM HYBRID MOTHER VOLUME     975
6310  CONTINUE                                                            975
         AG_TITLE='IHBC'                                                  975
         AG_MODULE=MODULE                                                 975
         AG_MARK='V00'                                                    975
         CALL AGSPUSH                                                     975
         AG_MODULE=MODULE                                                 975
C    *                                                                    975
C    Geant MATERIAL : KAPTON ===> inherited arguments used <===           976
         CALL AGSRESET                                                    976
         AG_TITLE = 'KAPTON'                                              976
         AG_EXNAME = 'MATERIAL'                                           976
         AG_PARLIST = 'NONE'                                              976
         CALL AXMATERIAL                                                  976
C    *                                                                    976
C    Geant ATTRIBUTE : IHBC ===> SEEN=1 COLO=2 <===                       976
         CALL AGSRESET                                                    977
         AG_SEEN = 1                                                      977
         AG_COLO = 2                                                      977
         AG_TITLE = 'IHBC'                                                977
         AG_EXNAME = 'ATTRIBUTE'                                          977
         AG_PARLIST = 'SEEN_COLO'                                         977
         CALL AXATTRIBUTE                                                 977
C    *                                                                    977
C    Geant SHAPE : BOX ===> DX=HALFHBDWDTTB DY=HALFHBDTHKTBCONNECT, DZ=H  977
C    ALFHBDLENTBCONNECT <===                                              977
         CALL AGSRESET                                                    979
         AG_DX = HALFHBDWDTTB                                             979
         AG_DY = HALFHBDTHKTBCONNECT                                      979
         AG_DZ = HALFHBDLENTBCONNECT                                      979
         AG_TITLE = 'BOX'                                                 979
         AG_EXNAME = 'SHAPE'                                              979
         AG_PARLIST = 'DX_DY_DZ'                                          979
         CALL AXSHAPE                                                     979
C    *                                                                    979
6311  CALL AGSPOPB                                                        980
      GO TO BLOCK_IHBC, (6310 ,6320)                                      980
C End of Local Procedure                                                  980
C ----------------------------------------------------------------------  981
C ------------ IHBH                                                       981
C Begin Local Procedure IHBH                                              982
C IHBH IS THE HYBRID ON WEST SIDE IN BOTTOM HYBRID MOTHER VOLUME          982
6330  CONTINUE                                                            982
         AG_TITLE='IHBH'                                                  982
         AG_MODULE=MODULE                                                 982
         AG_MARK='V00'                                                    982
         CALL AGSPUSH                                                     982
         AG_MODULE=MODULE                                                 982
C    *                                                                    982
C    Geant MATERIAL : KAPTON ===> inherited arguments used <===           983
         CALL AGSRESET                                                    983
         AG_TITLE = 'KAPTON'                                              983
         AG_EXNAME = 'MATERIAL'                                           983
         AG_PARLIST = 'NONE'                                              983
         CALL AXMATERIAL                                                  983
C    *                                                                    983
C    Geant ATTRIBUTE : IHBH ===> SEEN=1 COLO=2 <===                       983
         CALL AGSRESET                                                    984
         AG_SEEN = 1                                                      984
         AG_COLO = 2                                                      984
         AG_TITLE = 'IHBH'                                                984
         AG_EXNAME = 'ATTRIBUTE'                                          984
         AG_PARLIST = 'SEEN_COLO'                                         984
         CALL AXATTRIBUTE                                                 984
C    *                                                                    984
C    Geant SHAPE : BOX ===> DX=HALFHBDWDTTB DY=HALFHBDTHKTB, DZ=HALFHBDL  984
C    ENTB-HALFHBDLENTBCONNECT <===                                        984
         CALL AGSRESET                                                    986
         AG_DX = HALFHBDWDTTB                                             986
         AG_DY = HALFHBDTHKTB                                             986
         AG_DZ = HALFHBDLENTB-HALFHBDLENTBCONNECT                         986
         AG_TITLE = 'BOX'                                                 986
         AG_EXNAME = 'SHAPE'                                              986
         AG_PARLIST = 'DX_DY_DZ'                                          986
         CALL AXSHAPE                                                     986
C    *                                                                    986
6331  CALL AGSPOPB                                                        987
      GO TO BLOCK_IHBH, (6330 ,6340)                                      987
C End of Local Procedure                                                  987
C ----------------------------------------------------------------------  988
C ------------ IBHC                                                       988
C Begin Local Procedure IBHC                                              989
C IBHC KAPTON HYBRID NORTH ARC LAYER MOTHER VOLUME                        989
5680  CONTINUE                                                            989
         AG_TITLE='IBHC'                                                  989
         AG_MODULE=MODULE                                                 989
         AG_MARK='V00'                                                    989
         CALL AGSPUSH                                                     989
         AG_MODULE=MODULE                                                 989
C    *                                                                    989
C    Geant ATTRIBUTE : IBHC ===> SEEN=1 COLO=2 <===                       990
         CALL AGSRESET                                                    990
         AG_SEEN = 1                                                      990
         AG_COLO = 2                                                      990
         AG_TITLE = 'IBHC'                                                990
         AG_EXNAME = 'ATTRIBUTE'                                          990
         AG_PARLIST = 'SEEN_COLO'                                         990
         CALL AXATTRIBUTE                                                 990
C    *                                                                    990
C    Geant MATERIAL : KAPTON ===> inherited arguments used <===           990
         CALL AGSRESET                                                    991
         AG_TITLE = 'KAPTON'                                              991
         AG_EXNAME = 'MATERIAL'                                           991
         AG_PARLIST = 'NONE'                                              991
         CALL AXMATERIAL                                                  991
C    *                                                                    991
C    Geant SHAPE : TUBS ===> RMIN=HBDARCRMIN RMAX=HBDARCRMAX, PHI1=90.0   991
C    PHI2=270.0 DZ=HALFHBDARCLEN <===                                     991
         CALL AGSRESET                                                    993
         AG_RMIN = HBDARCRMIN                                             993
         AG_RMAX = HBDARCRMAX                                             993
         AG_PHI1 = 90.0                                                   993
         AG_PHI2 = 270.0                                                  993
         AG_DZ = HALFHBDARCLEN                                            993
         AG_TITLE = 'TUBS'                                                993
         AG_EXNAME = 'SHAPE'                                              993
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            993
         CALL AXSHAPE                                                     993
C    *                                                                    993
C    Create IHCA :                                                        994
C    Call To Local Procedure IHCA                                         994
         ASSIGN 6360 TO BLOCK_IHCA                                        994
         GO TO  6350                                                      994
6360     CONTINUE                                                         994
C    *                                                                    994
C    Geant POSITION : IHCA ===> X=0.0 Y=0.0 Z=-AIRINHBDHALFLEN+HALFHBDAR  995
C    CLEN <===                                                            995
         CALL AGSRESET                                                    995
         AG_X = 0.0                                                       995
         AG_Y = 0.0                                                       995
         AG_Z = -AIRINHBDHALFLEN+HALFHBDARCLEN                            995
         AG_TITLE = 'IHCA'                                                995
         AG_EXNAME = 'POSITION'                                           995
         AG_PARLIST = 'X_Y_Z'                                             995
         CALL AXPOSITION                                                  995
C    *                                                                    995
5681  CALL AGSPOPB                                                        996
      GO TO BLOCK_IBHC, (5680 ,5690)                                      996
C End of Local Procedure                                                  996
C ----------------------------------------------------------------------  997
C ------------ IHCA                                                       997
C Begin Local Procedure IHCA                                              998
C IHCA IS THE INNER AIR LAYER IN NORTH ARC HYBRID MOTHER VOLUME           998
6350  CONTINUE                                                            998
         AG_TITLE='IHCA'                                                  998
         AG_MODULE=MODULE                                                 998
         AG_MARK='V00'                                                    998
         CALL AGSPUSH                                                     998
         AG_MODULE=MODULE                                                 998
C    *                                                                    998
C    Geant MATERIAL : AIR ===> inherited arguments used <===              999
         CALL AGSRESET                                                    999
         AG_TITLE = 'AIR'                                                 999
         AG_EXNAME = 'MATERIAL'                                           999
         AG_PARLIST = 'NONE'                                              999
         CALL AXMATERIAL                                                  999
C    *                                                                    999
C    Geant ATTRIBUTE : IHCA ===> SEEN=1 COLO=1 <===                       999
         CALL AGSRESET                                                    1000
         AG_SEEN = 1                                                      1000
         AG_COLO = 1                                                      1000
         AG_TITLE = 'IHCA'                                                1000
         AG_EXNAME = 'ATTRIBUTE'                                          1000
         AG_PARLIST = 'SEEN_COLO'                                         1000
         CALL AXATTRIBUTE                                                 1000
C    *                                                                    1000
C    Geant SHAPE : TUBS ===> RMIN=HBDARCRMIN RMAX=AIRINHBDARCRMAX, PHI1=  1000
C    90.0 PHI2=270.0 DZ=AIRINHBDHALFLEN <===                              1000
         CALL AGSRESET                                                    1002
         AG_RMIN = HBDARCRMIN                                             1002
         AG_RMAX = AIRINHBDARCRMAX                                        1002
         AG_PHI1 = 90.0                                                   1002
         AG_PHI2 = 270.0                                                  1002
         AG_DZ = AIRINHBDHALFLEN                                          1002
         AG_TITLE = 'TUBS'                                                1002
         AG_EXNAME = 'SHAPE'                                              1002
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1002
         CALL AXSHAPE                                                     1002
C    *                                                                    1002
6351  CALL AGSPOPB                                                        1003
      GO TO BLOCK_IHCA, (6350 ,6360)                                      1003
C End of Local Procedure                                                  1003
C ----------------------------------------------------------------------  1004
C ------------ IBRB                                                       1004
C Begin Local Procedure IBRB                                              1005
C IBRB TRANSIT BOARD                                                      1005
5740  CONTINUE                                                            1005
         AG_TITLE='IBRB'                                                  1005
         AG_MODULE=MODULE                                                 1005
         AG_MARK='V00'                                                    1005
         CALL AGSPUSH                                                     1005
         AG_MODULE=MODULE                                                 1005
C    *                                                                    1005
C    Geant ATTRIBUTE : IBRB ===> SEEN=1 COLO=629 <===                     1006
         CALL AGSRESET                                                    1006
         AG_SEEN = 1                                                      1006
         AG_COLO = 629                                                    1006
         AG_TITLE = 'IBRB'                                                1006
         AG_EXNAME = 'ATTRIBUTE'                                          1006
         AG_PARLIST = 'SEEN_COLO'                                         1006
         CALL AXATTRIBUTE                                                 1006
C    *                                                                    1006
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1006
         CALL AGSRESET                                                    1007
         AG_TITLE = 'G10'                                                 1007
         AG_EXNAME = 'MATERIAL'                                           1007
         AG_PARLIST = 'NONE'                                              1007
         CALL AXMATERIAL                                                  1007
C    *                                                                    1007
C    Geant SHAPE : BOX ===> DX=HALFTBOARDWDT DY=HALFTBOARDTHK, DZ=HALFTB  1007
C    OARDLEN <===                                                         1007
         CALL AGSRESET                                                    1009
         AG_DX = HALFTBOARDWDT                                            1009
         AG_DY = HALFTBOARDTHK                                            1009
         AG_DZ = HALFTBOARDLEN                                            1009
         AG_TITLE = 'BOX'                                                 1009
         AG_EXNAME = 'SHAPE'                                              1009
         AG_PARLIST = 'DX_DY_DZ'                                          1009
         CALL AXSHAPE                                                     1009
C    *                                                                    1009
5741  CALL AGSPOPB                                                        1010
      GO TO BLOCK_IBRB, (5740 ,5750)                                      1010
C End of Local Procedure                                                  1010
C ----------------------------------------------------------------------  1011
C ------------ IRSA                                                       1011
C Begin Local Procedure IRSA                                              1012
C IRSA TRANSIT BOARD SLOT A                                               1012
5760  CONTINUE                                                            1012
         AG_TITLE='IRSA'                                                  1012
         AG_MODULE=MODULE                                                 1012
         AG_MARK='V00'                                                    1012
         CALL AGSPUSH                                                     1012
         AG_MODULE=MODULE                                                 1012
C    *                                                                    1012
C    Geant ATTRIBUTE : IRSA ===> SEEN=1 COLO=1 <===                       1013
         CALL AGSRESET                                                    1013
         AG_SEEN = 1                                                      1013
         AG_COLO = 1                                                      1013
         AG_TITLE = 'IRSA'                                                1013
         AG_EXNAME = 'ATTRIBUTE'                                          1013
         AG_PARLIST = 'SEEN_COLO'                                         1013
         CALL AXATTRIBUTE                                                 1013
C    *                                                                    1013
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1013
         CALL AGSRESET                                                    1014
         AG_TITLE = 'DELRIN'                                              1014
         AG_EXNAME = 'MATERIAL'                                           1014
         AG_PARLIST = 'NONE'                                              1014
         CALL AXMATERIAL                                                  1014
C    *                                                                    1014
C    Geant SHAPE : BOX ===> DX=HALFTBSLOTWDT DY=HALFTBSLOTTHK, DZ=HALFTB  1014
C    SLOTLEN <===                                                         1014
         CALL AGSRESET                                                    1016
         AG_DX = HALFTBSLOTWDT                                            1016
         AG_DY = HALFTBSLOTTHK                                            1016
         AG_DZ = HALFTBSLOTLEN                                            1016
         AG_TITLE = 'BOX'                                                 1016
         AG_EXNAME = 'SHAPE'                                              1016
         AG_PARLIST = 'DX_DY_DZ'                                          1016
         CALL AXSHAPE                                                     1016
C    *                                                                    1016
5761  CALL AGSPOPB                                                        1017
      GO TO BLOCK_IRSA, (5760 ,5770)                                      1017
C End of Local Procedure                                                  1017
C ----------------------------------------------------------------------  1018
C ------------ IRSB                                                       1018
C Begin Local Procedure IRSB                                              1019
C IRSB TRANSIT BOARD SLOT B                                               1019
5780  CONTINUE                                                            1019
         AG_TITLE='IRSB'                                                  1019
         AG_MODULE=MODULE                                                 1019
         AG_MARK='V00'                                                    1019
         CALL AGSPUSH                                                     1019
         AG_MODULE=MODULE                                                 1019
C    *                                                                    1019
C    Geant ATTRIBUTE : IRSB ===> SEEN=1 COLO=1 <===                       1020
         CALL AGSRESET                                                    1020
         AG_SEEN = 1                                                      1020
         AG_COLO = 1                                                      1020
         AG_TITLE = 'IRSB'                                                1020
         AG_EXNAME = 'ATTRIBUTE'                                          1020
         AG_PARLIST = 'SEEN_COLO'                                         1020
         CALL AXATTRIBUTE                                                 1020
C    *                                                                    1020
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1020
         CALL AGSRESET                                                    1021
         AG_TITLE = 'DELRIN'                                              1021
         AG_EXNAME = 'MATERIAL'                                           1021
         AG_PARLIST = 'NONE'                                              1021
         CALL AXMATERIAL                                                  1021
C    *                                                                    1021
C    Geant SHAPE : BOX ===> DX=HALFTBSLOTWDT DY=HALFTBSLOTTHK, DZ=HALFTB  1021
C    SLOTLEN <===                                                         1021
         CALL AGSRESET                                                    1023
         AG_DX = HALFTBSLOTWDT                                            1023
         AG_DY = HALFTBSLOTTHK                                            1023
         AG_DZ = HALFTBSLOTLEN                                            1023
         AG_TITLE = 'BOX'                                                 1023
         AG_EXNAME = 'SHAPE'                                              1023
         AG_PARLIST = 'DX_DY_DZ'                                          1023
         CALL AXSHAPE                                                     1023
C    *                                                                    1023
5781  CALL AGSPOPB                                                        1024
      GO TO BLOCK_IRSB, (5780 ,5790)                                      1024
C End of Local Procedure                                                  1024
C ----------------------------------------------------------------------  1025
C ------------ IRSC                                                       1025
C Begin Local Procedure IRSC                                              1026
C IRSC TRANSIT BOARD SLOT C                                               1026
5800  CONTINUE                                                            1026
         AG_TITLE='IRSC'                                                  1026
         AG_MODULE=MODULE                                                 1026
         AG_MARK='V00'                                                    1026
         CALL AGSPUSH                                                     1026
         AG_MODULE=MODULE                                                 1026
C    *                                                                    1026
C    Geant ATTRIBUTE : IRSC ===> SEEN=1 COLO=1 <===                       1027
         CALL AGSRESET                                                    1027
         AG_SEEN = 1                                                      1027
         AG_COLO = 1                                                      1027
         AG_TITLE = 'IRSC'                                                1027
         AG_EXNAME = 'ATTRIBUTE'                                          1027
         AG_PARLIST = 'SEEN_COLO'                                         1027
         CALL AXATTRIBUTE                                                 1027
C    *                                                                    1027
C    Geant MATERIAL : DELRIN ===> inherited arguments used <===           1027
         CALL AGSRESET                                                    1028
         AG_TITLE = 'DELRIN'                                              1028
         AG_EXNAME = 'MATERIAL'                                           1028
         AG_PARLIST = 'NONE'                                              1028
         CALL AXMATERIAL                                                  1028
C    *                                                                    1028
C    Geant SHAPE : BOX ===> DX=HALFTBSLOTWDT DY=HALFTBSLOTTHK, DZ=HALFTB  1028
C    SLOTLEN <===                                                         1028
         CALL AGSRESET                                                    1030
         AG_DX = HALFTBSLOTWDT                                            1030
         AG_DY = HALFTBSLOTTHK                                            1030
         AG_DZ = HALFTBSLOTLEN                                            1030
         AG_TITLE = 'BOX'                                                 1030
         AG_EXNAME = 'SHAPE'                                              1030
         AG_PARLIST = 'DX_DY_DZ'                                          1030
         CALL AXSHAPE                                                     1030
C    *                                                                    1030
5801  CALL AGSPOPB                                                        1031
      GO TO BLOCK_IRSC, (5800 ,5810)                                      1031
C End of Local Procedure                                                  1031
C ----------------------------------------------------------------------  1032
C ------------ IBTS                                                       1032
C Begin Local Procedure IBTS                                              1033
C IBTS IS THE THERMAL SENSOR                                              1033
5820  CONTINUE                                                            1033
         AG_TITLE='IBTS'                                                  1033
         AG_MODULE=MODULE                                                 1033
         AG_MARK='V00'                                                    1033
         CALL AGSPUSH                                                     1033
         AG_MODULE=MODULE                                                 1033
C    *                                                                    1033
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1034
         CALL AGSRESET                                                    1034
         AG_TITLE = 'SILICON'                                             1034
         AG_EXNAME = 'MATERIAL'                                           1034
         AG_PARLIST = 'NONE'                                              1034
         CALL AXMATERIAL                                                  1034
C    *                                                                    1034
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    1034
         CALL AGSRESET                                                    1035
         AG_ISVOL = 0                                                     1035
         AG_TITLE = 'PASSIVESILICON'                                      1035
         AG_EXNAME = 'MATERIAL'                                           1035
         AG_PARLIST = 'ISVOL'                                             1035
         CALL AXMATERIAL                                                  1035
C    *                                                                    1035
C    Geant ATTRIBUTE : IBTS ===> SEEN=1 COLO=2 <===                       1035
         CALL AGSRESET                                                    1036
         AG_SEEN = 1                                                      1036
         AG_COLO = 2                                                      1036
         AG_TITLE = 'IBTS'                                                1036
         AG_EXNAME = 'ATTRIBUTE'                                          1036
         AG_PARLIST = 'SEEN_COLO'                                         1036
         CALL AXATTRIBUTE                                                 1036
C    *                                                                    1036
C    Geant SHAPE : BOX ===> DX=0.1625/2.0 DY=0.06/2.0 DZ=0.1625/2.0 <===  1036
         CALL AGSRESET                                                    1037
         AG_DX = 0.1625/2.0                                               1037
         AG_DY = 0.06/2.0                                                 1037
         AG_DZ = 0.1625/2.0                                               1037
         AG_TITLE = 'BOX'                                                 1037
         AG_EXNAME = 'SHAPE'                                              1037
         AG_PARLIST = 'DX_DY_DZ'                                          1037
         CALL AXSHAPE                                                     1037
C    *                                                                    1037
5821  CALL AGSPOPB                                                        1038
      GO TO BLOCK_IBTS, (5820 ,5830)                                      1038
C End of Local Procedure                                                  1038
C ----------------------------------------------------------------------  1039
C ------------ IECW                                                       1039
C Begin Local Procedure IECW                                              1040
C IECW MOTHER OF THE WEST-SIDE ENDCAP ASSEMBLY                            1040
5840  CONTINUE                                                            1040
         AG_TITLE='IECW'                                                  1040
         AG_MODULE=MODULE                                                 1040
         AG_MARK='V00'                                                    1040
         CALL AGSPUSH                                                     1040
         AG_MODULE=MODULE                                                 1040
C    *                                                                    1040
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1041
         CALL AGSRESET                                                    1041
         AG_TITLE = 'PEEKCF30'                                            1041
         AG_EXNAME = 'MATERIAL'                                           1041
         AG_PARLIST = 'NONE'                                              1041
         CALL AXMATERIAL                                                  1041
C    *                                                                    1041
C    Geant ATTRIBUTE : IECW ===> SEEN=1 COLO=3 <===                       1041
         CALL AGSRESET                                                    1042
         AG_SEEN = 1                                                      1042
         AG_COLO = 3                                                      1042
         AG_TITLE = 'IECW'                                                1042
         AG_EXNAME = 'ATTRIBUTE'                                          1042
         AG_PARLIST = 'SEEN_COLO'                                         1042
         CALL AXATTRIBUTE                                                 1042
C    *                                                                    1042
C    Geant SHAPE : BOX ===> DX=HALFENDCAPMOMWDT DY=HALFENDCAPMOMTHK, DZ=  1042
C    HALFENDCAPMOMLEN <===                                                1042
         CALL AGSRESET                                                    1044
         AG_DX = HALFENDCAPMOMWDT                                         1044
         AG_DY = HALFENDCAPMOMTHK                                         1044
         AG_DZ = HALFENDCAPMOMLEN                                         1044
         AG_TITLE = 'BOX'                                                 1044
         AG_EXNAME = 'SHAPE'                                              1044
         AG_PARLIST = 'DX_DY_DZ'                                          1044
         CALL AXSHAPE                                                     1044
C    *                                                                    1044
C    Create ICWT :                                                        1045
C    Call To Local Procedure ICWT                                         1045
         ASSIGN 6380 TO BLOCK_ICWT                                        1045
         GO TO  6370                                                      1045
6380     CONTINUE                                                         1045
C    *                                                                    1045
C    Geant POSITION : ICWT ===> X=0.0 Y=HALFENDCAPMOMTHK-0.0275/2.0 Z=-H  1047
C    ALFENDCAPMOMLEN+CONNECTENDCAPSKINLEN/2.0 <===                        1047
         CALL AGSRESET                                                    1047
         AG_X = 0.0                                                       1047
         AG_Y = HALFENDCAPMOMTHK-0.0275/2.0                               1047
         AG_Z = -HALFENDCAPMOMLEN+CONNECTENDCAPSKINLEN/2.0                1047
         AG_TITLE = 'ICWT'                                                1047
         AG_EXNAME = 'POSITION'                                           1047
         AG_PARLIST = 'X_Y_Z'                                             1047
         CALL AXPOSITION                                                  1047
C    *                                                                    1047
C    Create ICWB :                                                        1048
C    Call To Local Procedure ICWB                                         1048
         ASSIGN 6400 TO BLOCK_ICWB                                        1048
         GO TO  6390                                                      1048
6400     CONTINUE                                                         1048
C    *                                                                    1048
C    Geant POSITION : ICWB ===> X=0.0 Y=-HALFENDCAPMOMTHK+0.0275/2.0 Z=-  1050
C    HALFENDCAPMOMLEN+CONNECTENDCAPSKINLEN/2.0 <===                       1050
         CALL AGSRESET                                                    1050
         AG_X = 0.0                                                       1050
         AG_Y = -HALFENDCAPMOMTHK+0.0275/2.0                              1050
         AG_Z = -HALFENDCAPMOMLEN+CONNECTENDCAPSKINLEN/2.0                1050
         AG_TITLE = 'ICWB'                                                1050
         AG_EXNAME = 'POSITION'                                           1050
         AG_PARLIST = 'X_Y_Z'                                             1050
         CALL AXPOSITION                                                  1050
C    *                                                                    1050
C    Create IABW :                                                        1051
C    Call To Local Procedure IABW                                         1051
         ASSIGN 6420 TO BLOCK_IABW                                        1051
         GO TO  6410                                                      1051
6420     CONTINUE                                                         1051
C    *                                                                    1051
C    Geant POSITION : IABW ===> X=-HALFENDCAPMOMWDT+1.23485+0.5065/2.0 Y  1053
C    =HALFENDCAPMOMTHK-0.31/2.0 Z=0.0 <===                                1053
         CALL AGSRESET                                                    1053
         AG_X = -HALFENDCAPMOMWDT+1.23485+0.5065/2.0                      1053
         AG_Y = HALFENDCAPMOMTHK-0.31/2.0                                 1053
         AG_Z = 0.0                                                       1053
         AG_TITLE = 'IABW'                                                1053
         AG_EXNAME = 'POSITION'                                           1053
         AG_PARLIST = 'X_Y_Z'                                             1053
         CALL AXPOSITION                                                  1053
C    *                                                                    1053
C    Create IAAW :                                                        1054
C    Call To Local Procedure IAAW                                         1054
         ASSIGN 6440 TO BLOCK_IAAW                                        1054
         GO TO  6430                                                      1054
6440     CONTINUE                                                         1054
C    *                                                                    1054
C    Geant POSITION : IAAW ===> X=-HALFENDCAPMOMWDT+1.23485+0.5065/2.0 Y  1056
C    =HALFENDCAPMOMTHK-0.31 Z=0.0 <===                                    1056
         CALL AGSRESET                                                    1056
         AG_X = -HALFENDCAPMOMWDT+1.23485+0.5065/2.0                      1056
         AG_Y = HALFENDCAPMOMTHK-0.31                                     1056
         AG_Z = 0.0                                                       1056
         AG_TITLE = 'IAAW'                                                1056
         AG_EXNAME = 'POSITION'                                           1056
         AG_PARLIST = 'X_Y_Z'                                             1056
         CALL AXPOSITION                                                  1056
C    *                                                                    1056
C    Create IESB :                                                        1057
C    Call To Local Procedure IESB                                         1057
         ASSIGN 6460 TO BLOCK_IESB                                        1057
         GO TO  6450                                                      1057
6460     CONTINUE                                                         1057
C    *                                                                    1057
C    Geant POSITION : IESB ===> X=0.0 Y=0.0 Z=HALFENDCAPMOMLEN-0.6-0.3/2  1058
C    .0 <===                                                              1058
         CALL AGSRESET                                                    1058
         AG_X = 0.0                                                       1058
         AG_Y = 0.0                                                       1058
         AG_Z = HALFENDCAPMOMLEN-0.6-0.3/2.0                              1058
         AG_TITLE = 'IESB'                                                1058
         AG_EXNAME = 'POSITION'                                           1058
         AG_PARLIST = 'X_Y_Z'                                             1058
         CALL AXPOSITION                                                  1058
C    *                                                                    1058
C    Create IESA :                                                        1059
C    Call To Local Procedure IESA                                         1059
         ASSIGN 6480 TO BLOCK_IESA                                        1059
         GO TO  6470                                                      1059
6480     CONTINUE                                                         1059
C    *                                                                    1059
C    Geant POSITION : IESA ===> X=0.0 Y=0.0 Z=HALFENDCAPMOMLEN-0.6-0.3 A  1060
C    LPHAX=90.0 <===                                                      1060
         CALL AGSRESET                                                    1060
         AG_X = 0.0                                                       1060
         AG_Y = 0.0                                                       1060
         AG_Z = HALFENDCAPMOMLEN-0.6-0.3                                  1060
         AG_ALPHAN=AG_ALPHAN+1                                            1060
         AG_ALPHAK(AG_ALPHAN)=1                                           1060
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1060
         AG_TITLE = 'IESA'                                                1060
         AG_EXNAME = 'POSITION'                                           1060
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1060
         CALL AXPOSITION                                                  1060
C    *                                                                    1060
C    Create IESC :                                                        1061
C    Call To Local Procedure IESC                                         1061
         ASSIGN 6500 TO BLOCK_IESC                                        1061
         GO TO  6490                                                      1061
6500     CONTINUE                                                         1061
C    *                                                                    1061
C    Geant POSITION : IESC ===> X=0.0 Y=0.0 Z=HALFENDCAPMOMLEN-0.6 ALPHA  1062
C    X=90.0 <===                                                          1062
         CALL AGSRESET                                                    1062
         AG_X = 0.0                                                       1062
         AG_Y = 0.0                                                       1062
         AG_Z = HALFENDCAPMOMLEN-0.6                                      1062
         AG_ALPHAN=AG_ALPHAN+1                                            1062
         AG_ALPHAK(AG_ALPHAN)=1                                           1062
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1062
         AG_TITLE = 'IESC'                                                1062
         AG_EXNAME = 'POSITION'                                           1062
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1062
         CALL AXPOSITION                                                  1062
C    *                                                                    1062
C    Create ITNW :                                                        1063
C    Call To Local Procedure ITNW                                         1063
         ASSIGN 6520 TO BLOCK_ITNW                                        1063
         GO TO  6510                                                      1063
6520     CONTINUE                                                         1063
C    *                                                                    1063
C    Geant POSITION : ITNW ===> X=-HALFENDCAPMOMWDT+0.55 Y=0.0 Z=HALFEND  1064
C    CAPMOMLEN-0.5 ALPHAX=90.0 <===                                       1064
         CALL AGSRESET                                                    1064
         AG_X = -HALFENDCAPMOMWDT+0.55                                    1064
         AG_Y = 0.0                                                       1064
         AG_Z = HALFENDCAPMOMLEN-0.5                                      1064
         AG_ALPHAN=AG_ALPHAN+1                                            1064
         AG_ALPHAK(AG_ALPHAN)=1                                           1064
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1064
         AG_TITLE = 'ITNW'                                                1064
         AG_EXNAME = 'POSITION'                                           1064
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1064
         CALL AXPOSITION                                                  1064
C    *                                                                    1064
C    Create ITSW :                                                        1065
C    Call To Local Procedure ITSW                                         1065
         ASSIGN 6540 TO BLOCK_ITSW                                        1065
         GO TO  6530                                                      1065
6540     CONTINUE                                                         1065
C    *                                                                    1065
C    Geant POSITION : ITSW ===> X=HALFENDCAPMOMWDT-0.55 Y=0.0 Z=HALFENDC  1066
C    APMOMLEN-0.5 ALPHAX=90.0 <===                                        1066
         CALL AGSRESET                                                    1066
         AG_X = HALFENDCAPMOMWDT-0.55                                     1066
         AG_Y = 0.0                                                       1066
         AG_Z = HALFENDCAPMOMLEN-0.5                                      1066
         AG_ALPHAN=AG_ALPHAN+1                                            1066
         AG_ALPHAK(AG_ALPHAN)=1                                           1066
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1066
         AG_TITLE = 'ITSW'                                                1066
         AG_EXNAME = 'POSITION'                                           1066
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1066
         CALL AXPOSITION                                                  1066
C    *                                                                    1066
5841  CALL AGSPOPB                                                        1067
      GO TO BLOCK_IECW, (5840 ,5850)                                      1067
C End of Local Procedure                                                  1067
C ----------------------------------------------------------------------  1068
C ------------ ICWT                                                       1068
C Begin Local Procedure ICWT                                              1069
C ICWT IS THE CFOAM ENDCAP OVERLAP BODY TOP BOX VOLUME                    1069
6370  CONTINUE                                                            1069
         AG_TITLE='ICWT'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1070
         CALL AGSRESET                                                    1070
         AG_TITLE = 'AIR'                                                 1070
         AG_EXNAME = 'MATERIAL'                                           1070
         AG_PARLIST = 'NONE'                                              1070
         CALL AXMATERIAL                                                  1070
C    *                                                                    1070
C    Geant ATTRIBUTE : ICWT ===> SEEN=1 COLO=1 <===                       1070
         CALL AGSRESET                                                    1071
         AG_SEEN = 1                                                      1071
         AG_COLO = 1                                                      1071
         AG_TITLE = 'ICWT'                                                1071
         AG_EXNAME = 'ATTRIBUTE'                                          1071
         AG_PARLIST = 'SEEN_COLO'                                         1071
         CALL AXATTRIBUTE                                                 1071
C    *                                                                    1071
C    Geant SHAPE : BOX ===> DX=HALFENDCAPMOMWDT DY=(HALFENDCAPMOMTHK-HAL  1071
C    FOVERLAPENDCAPSKINTHK)/2.0, DZ=CONNECTENDCAPSKINLEN/2.0 <===         1071
         CALL AGSRESET                                                    1073
         AG_DX = HALFENDCAPMOMWDT                                         1073
         AG_DY = (HALFENDCAPMOMTHK-HALFOVERLAPENDCAPSKINTHK)/2.0          1073
         AG_DZ = CONNECTENDCAPSKINLEN/2.0                                 1073
         AG_TITLE = 'BOX'                                                 1073
         AG_EXNAME = 'SHAPE'                                              1073
         AG_PARLIST = 'DX_DY_DZ'                                          1073
         CALL AXSHAPE                                                     1073
C    *                                                                    1073
6371  CALL AGSPOPB                                                        1074
      GO TO BLOCK_ICWT, (6370 ,6380)                                      1074
C End of Local Procedure                                                  1074
C ----------------------------------------------------------------------  1075
C ------------ ICWB                                                       1075
C Begin Local Procedure ICWB                                              1076
C ICWB IS THE CFOAM ENDCAP OVERLAP BODY BOTTOM BOX VOLUME                 1076
6390  CONTINUE                                                            1076
         AG_TITLE='ICWB'                                                  1076
         AG_MODULE=MODULE                                                 1076
         AG_MARK='V00'                                                    1076
         CALL AGSPUSH                                                     1076
         AG_MODULE=MODULE                                                 1076
C    *                                                                    1076
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1077
         CALL AGSRESET                                                    1077
         AG_TITLE = 'AIR'                                                 1077
         AG_EXNAME = 'MATERIAL'                                           1077
         AG_PARLIST = 'NONE'                                              1077
         CALL AXMATERIAL                                                  1077
C    *                                                                    1077
C    Geant ATTRIBUTE : ICWB ===> SEEN=1 COLO=1 <===                       1077
         CALL AGSRESET                                                    1078
         AG_SEEN = 1                                                      1078
         AG_COLO = 1                                                      1078
         AG_TITLE = 'ICWB'                                                1078
         AG_EXNAME = 'ATTRIBUTE'                                          1078
         AG_PARLIST = 'SEEN_COLO'                                         1078
         CALL AXATTRIBUTE                                                 1078
C    *                                                                    1078
C    Geant SHAPE : BOX ===> DX=HALFENDCAPMOMWDT DY=(HALFENDCAPMOMTHK-HAL  1078
C    FOVERLAPENDCAPSKINTHK)/2.0, DZ=CONNECTENDCAPSKINLEN/2.0 <===         1078
         CALL AGSRESET                                                    1080
         AG_DX = HALFENDCAPMOMWDT                                         1080
         AG_DY = (HALFENDCAPMOMTHK-HALFOVERLAPENDCAPSKINTHK)/2.0          1080
         AG_DZ = CONNECTENDCAPSKINLEN/2.0                                 1080
         AG_TITLE = 'BOX'                                                 1080
         AG_EXNAME = 'SHAPE'                                              1080
         AG_PARLIST = 'DX_DY_DZ'                                          1080
         CALL AXSHAPE                                                     1080
C    *                                                                    1080
6391  CALL AGSPOPB                                                        1081
      GO TO BLOCK_ICWB, (6390 ,6400)                                      1081
C End of Local Procedure                                                  1081
C ----------------------------------------------------------------------  1082
C ------------ IABW                                                       1082
C Begin Local Procedure IABW                                              1083
C IABW IS THE ENDCAP AIR GROOVE BOX AREA FOR TUBE PLACEMENT               1083
6410  CONTINUE                                                            1083
         AG_TITLE='IABW'                                                  1083
         AG_MODULE=MODULE                                                 1083
         AG_MARK='V00'                                                    1083
         CALL AGSPUSH                                                     1083
         AG_MODULE=MODULE                                                 1083
C    *                                                                    1083
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1084
         CALL AGSRESET                                                    1084
         AG_TITLE = 'AIR'                                                 1084
         AG_EXNAME = 'MATERIAL'                                           1084
         AG_PARLIST = 'NONE'                                              1084
         CALL AXMATERIAL                                                  1084
C    *                                                                    1084
C    Geant ATTRIBUTE : IABW ===> SEEN=1 COLO=1 <===                       1084
         CALL AGSRESET                                                    1085
         AG_SEEN = 1                                                      1085
         AG_COLO = 1                                                      1085
         AG_TITLE = 'IABW'                                                1085
         AG_EXNAME = 'ATTRIBUTE'                                          1085
         AG_PARLIST = 'SEEN_COLO'                                         1085
         CALL AXATTRIBUTE                                                 1085
C    *                                                                    1085
C    Geant SHAPE : BOX ===> DX=0.5065/2.0 DY=0.31/2.0 DZ=HALFENDCAPMOMLE  1085
C    N <===                                                               1085
         CALL AGSRESET                                                    1086
         AG_DX = 0.5065/2.0                                               1086
         AG_DY = 0.31/2.0                                                 1086
         AG_DZ = HALFENDCAPMOMLEN                                         1086
         AG_TITLE = 'BOX'                                                 1086
         AG_EXNAME = 'SHAPE'                                              1086
         AG_PARLIST = 'DX_DY_DZ'                                          1086
         CALL AXSHAPE                                                     1086
C    *                                                                    1086
6411  CALL AGSPOPB                                                        1087
      GO TO BLOCK_IABW, (6410 ,6420)                                      1087
C End of Local Procedure                                                  1087
C ----------------------------------------------------------------------  1088
C ------------ IAAW                                                       1088
C Begin Local Procedure IAAW                                              1089
C IAAW IS THE ENDCAP AIR GROOVE ARC AREA FOR TUBE PLACEMENT               1089
6430  CONTINUE                                                            1089
         AG_TITLE='IAAW'                                                  1089
         AG_MODULE=MODULE                                                 1089
         AG_MARK='V00'                                                    1089
         CALL AGSPUSH                                                     1089
         AG_MODULE=MODULE                                                 1089
C    *                                                                    1089
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1090
         CALL AGSRESET                                                    1090
         AG_TITLE = 'AIR'                                                 1090
         AG_EXNAME = 'MATERIAL'                                           1090
         AG_PARLIST = 'NONE'                                              1090
         CALL AXMATERIAL                                                  1090
C    *                                                                    1090
C    Geant ATTRIBUTE : IAAW ===> SEEN=1 COLO=1 <===                       1090
         CALL AGSRESET                                                    1091
         AG_SEEN = 1                                                      1091
         AG_COLO = 1                                                      1091
         AG_TITLE = 'IAAW'                                                1091
         AG_EXNAME = 'ATTRIBUTE'                                          1091
         AG_PARLIST = 'SEEN_COLO'                                         1091
         CALL AXATTRIBUTE                                                 1091
C    *                                                                    1091
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=0.5065/2.0 PHI1=180.0, PHI2=360  1091
C    .0 DZ=HALFENDCAPMOMLEN <===                                          1091
         CALL AGSRESET                                                    1093
         AG_RMIN = 0                                                      1093
         AG_RMAX = 0.5065/2.0                                             1093
         AG_PHI1 = 180.0                                                  1093
         AG_PHI2 = 360.0                                                  1093
         AG_DZ = HALFENDCAPMOMLEN                                         1093
         AG_TITLE = 'TUBS'                                                1093
         AG_EXNAME = 'SHAPE'                                              1093
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1093
         CALL AXSHAPE                                                     1093
C    *                                                                    1093
6431  CALL AGSPOPB                                                        1094
      GO TO BLOCK_IAAW, (6430 ,6440)                                      1094
C End of Local Procedure                                                  1094
C ----------------------------------------------------------------------  1095
C ------------ IESB                                                       1095
C Begin Local Procedure IESB                                              1096
C IESB IS THE ENDCAP AIR GROOVE BOX AREA FOR SCREW THREAD HOLE            1096
6450  CONTINUE                                                            1096
         AG_TITLE='IESB'                                                  1096
         AG_MODULE=MODULE                                                 1096
         AG_MARK='V00'                                                    1096
         CALL AGSPUSH                                                     1096
         AG_MODULE=MODULE                                                 1096
C    *                                                                    1096
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1097
         CALL AGSRESET                                                    1097
         AG_TITLE = 'AIR'                                                 1097
         AG_EXNAME = 'MATERIAL'                                           1097
         AG_PARLIST = 'NONE'                                              1097
         CALL AXMATERIAL                                                  1097
C    *                                                                    1097
C    Geant ATTRIBUTE : IESB ===> SEEN=1 COLO=1 <===                       1097
         CALL AGSRESET                                                    1098
         AG_SEEN = 1                                                      1098
         AG_COLO = 1                                                      1098
         AG_TITLE = 'IESB'                                                1098
         AG_EXNAME = 'ATTRIBUTE'                                          1098
         AG_PARLIST = 'SEEN_COLO'                                         1098
         CALL AXATTRIBUTE                                                 1098
C    *                                                                    1098
C    Geant SHAPE : BOX ===> DX=0.635/2.0 DY=HALFENDCAPMOMTHK, DZ=0.3/2.0  1098
C     <===                                                                1098
         CALL AGSRESET                                                    1100
         AG_DX = 0.635/2.0                                                1100
         AG_DY = HALFENDCAPMOMTHK                                         1100
         AG_DZ = 0.3/2.0                                                  1100
         AG_TITLE = 'BOX'                                                 1100
         AG_EXNAME = 'SHAPE'                                              1100
         AG_PARLIST = 'DX_DY_DZ'                                          1100
         CALL AXSHAPE                                                     1100
C    *                                                                    1100
6451  CALL AGSPOPB                                                        1101
      GO TO BLOCK_IESB, (6450 ,6460)                                      1101
C End of Local Procedure                                                  1101
C ----------------------------------------------------------------------  1102
C ------------ IESA                                                       1102
C Begin Local Procedure IESA                                              1103
C IESA IS THE ENDCAP AIR GROOVE ARC AREA FOR SCREW THREAD HOLE            1103
6470  CONTINUE                                                            1103
         AG_TITLE='IESA'                                                  1103
         AG_MODULE=MODULE                                                 1103
         AG_MARK='V00'                                                    1103
         CALL AGSPUSH                                                     1103
         AG_MODULE=MODULE                                                 1103
C    *                                                                    1103
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1104
         CALL AGSRESET                                                    1104
         AG_TITLE = 'AIR'                                                 1104
         AG_EXNAME = 'MATERIAL'                                           1104
         AG_PARLIST = 'NONE'                                              1104
         CALL AXMATERIAL                                                  1104
C    *                                                                    1104
C    Geant ATTRIBUTE : IESA ===> SEEN=1 COLO=1 <===                       1104
         CALL AGSRESET                                                    1105
         AG_SEEN = 1                                                      1105
         AG_COLO = 1                                                      1105
         AG_TITLE = 'IESA'                                                1105
         AG_EXNAME = 'ATTRIBUTE'                                          1105
         AG_PARLIST = 'SEEN_COLO'                                         1105
         CALL AXATTRIBUTE                                                 1105
C    *                                                                    1105
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=0.635/2.0 PHI1=180.0, PHI2=36  1105
C    0.0 DZ=HALFENDCAPMOMTHK <===                                         1105
         CALL AGSRESET                                                    1107
         AG_RMIN = 0.0                                                    1107
         AG_RMAX = 0.635/2.0                                              1107
         AG_PHI1 = 180.0                                                  1107
         AG_PHI2 = 360.0                                                  1107
         AG_DZ = HALFENDCAPMOMTHK                                         1107
         AG_TITLE = 'TUBS'                                                1107
         AG_EXNAME = 'SHAPE'                                              1107
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1107
         CALL AXSHAPE                                                     1107
C    *                                                                    1107
6471  CALL AGSPOPB                                                        1108
      GO TO BLOCK_IESA, (6470 ,6480)                                      1108
C End of Local Procedure                                                  1108
C ----------------------------------------------------------------------  1109
C ------------ IESC                                                       1109
C Begin Local Procedure IESC                                              1110
C IESC IS THE ENDCAP AIR GROOVE ARC AREA FOR SCREW THREAD HOLE            1110
6490  CONTINUE                                                            1110
         AG_TITLE='IESC'                                                  1110
         AG_MODULE=MODULE                                                 1110
         AG_MARK='V00'                                                    1110
         CALL AGSPUSH                                                     1110
         AG_MODULE=MODULE                                                 1110
C    *                                                                    1110
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1111
         CALL AGSRESET                                                    1111
         AG_TITLE = 'AIR'                                                 1111
         AG_EXNAME = 'MATERIAL'                                           1111
         AG_PARLIST = 'NONE'                                              1111
         CALL AXMATERIAL                                                  1111
C    *                                                                    1111
C    Geant ATTRIBUTE : IESC ===> SEEN=1 COLO=1 <===                       1111
         CALL AGSRESET                                                    1112
         AG_SEEN = 1                                                      1112
         AG_COLO = 1                                                      1112
         AG_TITLE = 'IESC'                                                1112
         AG_EXNAME = 'ATTRIBUTE'                                          1112
         AG_PARLIST = 'SEEN_COLO'                                         1112
         CALL AXATTRIBUTE                                                 1112
C    *                                                                    1112
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=0.635/2.0 PHI1=.0, PHI2=180.0  1112
C     DZ=HALFENDCAPMOMTHK <===                                            1112
         CALL AGSRESET                                                    1114
         AG_RMIN = 0.0                                                    1114
         AG_RMAX = 0.635/2.0                                              1114
         AG_PHI1 = .0                                                     1114
         AG_PHI2 = 180.0                                                  1114
         AG_DZ = HALFENDCAPMOMTHK                                         1114
         AG_TITLE = 'TUBS'                                                1114
         AG_EXNAME = 'SHAPE'                                              1114
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1114
         CALL AXSHAPE                                                     1114
C    *                                                                    1114
6491  CALL AGSPOPB                                                        1115
      GO TO BLOCK_IESC, (6490 ,6500)                                      1115
C End of Local Procedure                                                  1115
C ----------------------------------------------------------------------  1116
C ------------ ITNW                                                       1116
C Begin Local Procedure ITNW                                              1118
C ITNW IS THE ENDCAP AIR GROOVE FOR TRANSPORT PROTECTION SCREW THREAD, (  1118
C NORTHWEST)                                                              1118
6510  CONTINUE                                                            1118
         AG_TITLE='ITNW'                                                  1118
         AG_MODULE=MODULE                                                 1118
         AG_MARK='V00'                                                    1118
         CALL AGSPUSH                                                     1118
         AG_MODULE=MODULE                                                 1118
C    *                                                                    1118
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1119
         CALL AGSRESET                                                    1119
         AG_TITLE = 'AIR'                                                 1119
         AG_EXNAME = 'MATERIAL'                                           1119
         AG_PARLIST = 'NONE'                                              1119
         CALL AXMATERIAL                                                  1119
C    *                                                                    1119
C    Geant ATTRIBUTE : ITNW ===> SEEN=1 COLO=1 <===                       1119
         CALL AGSRESET                                                    1120
         AG_SEEN = 1                                                      1120
         AG_COLO = 1                                                      1120
         AG_TITLE = 'ITNW'                                                1120
         AG_EXNAME = 'ATTRIBUTE'                                          1120
         AG_PARLIST = 'SEEN_COLO'                                         1120
         CALL AXATTRIBUTE                                                 1120
C    *                                                                    1120
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.3/2.0 DZ=HALFENDCAPMOMTHK <  1120
C    ===                                                                  1120
         CALL AGSRESET                                                    1121
         AG_RMIN = 0.0                                                    1121
         AG_RMAX = 0.3/2.0                                                1121
         AG_DZ = HALFENDCAPMOMTHK                                         1121
         AG_TITLE = 'TUBE'                                                1121
         AG_EXNAME = 'SHAPE'                                              1121
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1121
         CALL AXSHAPE                                                     1121
C    *                                                                    1121
6511  CALL AGSPOPB                                                        1122
      GO TO BLOCK_ITNW, (6510 ,6520)                                      1122
C End of Local Procedure                                                  1122
C ----------------------------------------------------------------------  1123
C ------------ ITSW                                                       1123
C Begin Local Procedure ITSW                                              1125
C ITSW IS THE ENDCAP AIR GROOVE FOR TRANSPORT PROTECTION SCREW THREAD, (  1125
C SOUTHWEST)                                                              1125
6530  CONTINUE                                                            1125
         AG_TITLE='ITSW'                                                  1125
         AG_MODULE=MODULE                                                 1125
         AG_MARK='V00'                                                    1125
         CALL AGSPUSH                                                     1125
         AG_MODULE=MODULE                                                 1125
C    *                                                                    1125
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1126
         CALL AGSRESET                                                    1126
         AG_TITLE = 'AIR'                                                 1126
         AG_EXNAME = 'MATERIAL'                                           1126
         AG_PARLIST = 'NONE'                                              1126
         CALL AXMATERIAL                                                  1126
C    *                                                                    1126
C    Geant ATTRIBUTE : ITSW ===> SEEN=1 COLO=1 <===                       1126
         CALL AGSRESET                                                    1127
         AG_SEEN = 1                                                      1127
         AG_COLO = 1                                                      1127
         AG_TITLE = 'ITSW'                                                1127
         AG_EXNAME = 'ATTRIBUTE'                                          1127
         AG_PARLIST = 'SEEN_COLO'                                         1127
         CALL AXATTRIBUTE                                                 1127
C    *                                                                    1127
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.3/2.0 DZ=HALFENDCAPMOMTHK <  1127
C    ===                                                                  1127
         CALL AGSRESET                                                    1128
         AG_RMIN = 0.0                                                    1128
         AG_RMAX = 0.3/2.0                                                1128
         AG_DZ = HALFENDCAPMOMTHK                                         1128
         AG_TITLE = 'TUBE'                                                1128
         AG_EXNAME = 'SHAPE'                                              1128
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1128
         CALL AXSHAPE                                                     1128
C    *                                                                    1128
6531  CALL AGSPOPB                                                        1129
      GO TO BLOCK_ITSW, (6530 ,6540)                                      1129
C End of Local Procedure                                                  1129
C ----------------------------------------------------------------------  1130
C ------------ IECE                                                       1130
C Begin Local Procedure IECE                                              1131
C IECE MOTHER OF THE EAST-SIDE ENDCAP ASSEMBLY                            1131
5860  CONTINUE                                                            1131
         AG_TITLE='IECE'                                                  1131
         AG_MODULE=MODULE                                                 1131
         AG_MARK='V00'                                                    1131
         CALL AGSPUSH                                                     1131
         AG_MODULE=MODULE                                                 1131
C    *                                                                    1131
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1132
         CALL AGSRESET                                                    1132
         AG_TITLE = 'ALUMINIUM'                                           1132
         AG_EXNAME = 'MATERIAL'                                           1132
         AG_PARLIST = 'NONE'                                              1132
         CALL AXMATERIAL                                                  1132
C    *                                                                    1132
C    Geant ATTRIBUTE : IECE ===> SEEN=1 COLO=4 <===                       1132
         CALL AGSRESET                                                    1133
         AG_SEEN = 1                                                      1133
         AG_COLO = 4                                                      1133
         AG_TITLE = 'IECE'                                                1133
         AG_EXNAME = 'ATTRIBUTE'                                          1133
         AG_PARLIST = 'SEEN_COLO'                                         1133
         CALL AXATTRIBUTE                                                 1133
C    *                                                                    1133
C    Geant SHAPE : BOX ===> DX=HALFENDCAPMOMWDT DY=HALFENDCAPMOMTHK, DZ=  1133
C    HALFENDCAPMOMLEN <===                                                1133
         CALL AGSRESET                                                    1135
         AG_DX = HALFENDCAPMOMWDT                                         1135
         AG_DY = HALFENDCAPMOMTHK                                         1135
         AG_DZ = HALFENDCAPMOMLEN                                         1135
         AG_TITLE = 'BOX'                                                 1135
         AG_EXNAME = 'SHAPE'                                              1135
         AG_PARLIST = 'DX_DY_DZ'                                          1135
         CALL AXSHAPE                                                     1135
C    *                                                                    1135
C    Create ICET :                                                        1136
C    Call To Local Procedure ICET                                         1136
         ASSIGN 6560 TO BLOCK_ICET                                        1136
         GO TO  6550                                                      1136
6560     CONTINUE                                                         1136
C    *                                                                    1136
C    Geant POSITION : ICET ===> X=0.0 Y=HALFENDCAPMOMTHK-0.03435/2.0 Z=H  1138
C    ALFENDCAPMOMLEN-CONNECTENDCAPSKINLEN/2.0 <===                        1138
         CALL AGSRESET                                                    1138
         AG_X = 0.0                                                       1138
         AG_Y = HALFENDCAPMOMTHK-0.03435/2.0                              1138
         AG_Z = HALFENDCAPMOMLEN-CONNECTENDCAPSKINLEN/2.0                 1138
         AG_TITLE = 'ICET'                                                1138
         AG_EXNAME = 'POSITION'                                           1138
         AG_PARLIST = 'X_Y_Z'                                             1138
         CALL AXPOSITION                                                  1138
C    *                                                                    1138
C    Create ICEB :                                                        1139
C    Call To Local Procedure ICEB                                         1139
         ASSIGN 6580 TO BLOCK_ICEB                                        1139
         GO TO  6570                                                      1139
6580     CONTINUE                                                         1139
C    *                                                                    1139
C    Geant POSITION : ICEB ===> X=0.0 Y=-HALFENDCAPMOMTHK+0.03435/2.0 Z=  1141
C    HALFENDCAPMOMLEN-CONNECTENDCAPSKINLEN/2.0 <===                       1141
         CALL AGSRESET                                                    1141
         AG_X = 0.0                                                       1141
         AG_Y = -HALFENDCAPMOMTHK+0.03435/2.0                             1141
         AG_Z = HALFENDCAPMOMLEN-CONNECTENDCAPSKINLEN/2.0                 1141
         AG_TITLE = 'ICEB'                                                1141
         AG_EXNAME = 'POSITION'                                           1141
         AG_PARLIST = 'X_Y_Z'                                             1141
         CALL AXPOSITION                                                  1141
C    *                                                                    1141
C    Create IABE :                                                        1142
C    Call To Local Procedure IABE                                         1142
         ASSIGN 6600 TO BLOCK_IABE                                        1142
         GO TO  6590                                                      1142
6600     CONTINUE                                                         1142
C    *                                                                    1142
C    Geant POSITION : IABE ===> X=-HALFENDCAPMOMWDT+1.23485+0.5065/2.0 Y  1144
C    =HALFENDCAPMOMTHK-0.3031/2.0 Z=0.0 <===                              1144
         CALL AGSRESET                                                    1144
         AG_X = -HALFENDCAPMOMWDT+1.23485+0.5065/2.0                      1144
         AG_Y = HALFENDCAPMOMTHK-0.3031/2.0                               1144
         AG_Z = 0.0                                                       1144
         AG_TITLE = 'IABE'                                                1144
         AG_EXNAME = 'POSITION'                                           1144
         AG_PARLIST = 'X_Y_Z'                                             1144
         CALL AXPOSITION                                                  1144
C    *                                                                    1144
C    Create IAAE :                                                        1145
C    Call To Local Procedure IAAE                                         1145
         ASSIGN 6620 TO BLOCK_IAAE                                        1145
         GO TO  6610                                                      1145
6620     CONTINUE                                                         1145
C    *                                                                    1145
C    Geant POSITION : IAAE ===> X=-HALFENDCAPMOMWDT+1.23485+0.5065/2.0 Y  1147
C    =HALFENDCAPMOMTHK-0.3031 Z=0.0 <===                                  1147
         CALL AGSRESET                                                    1147
         AG_X = -HALFENDCAPMOMWDT+1.23485+0.5065/2.0                      1147
         AG_Y = HALFENDCAPMOMTHK-0.3031                                   1147
         AG_Z = 0.0                                                       1147
         AG_TITLE = 'IAAE'                                                1147
         AG_EXNAME = 'POSITION'                                           1147
         AG_PARLIST = 'X_Y_Z'                                             1147
         CALL AXPOSITION                                                  1147
C    *                                                                    1147
C    Create IESE :                                                        1148
C    Call To Local Procedure IESE                                         1148
         ASSIGN 6640 TO BLOCK_IESE                                        1148
         GO TO  6630                                                      1148
6640     CONTINUE                                                         1148
C    *                                                                    1148
C    Geant POSITION : IESE ===> X=0.0 Y=0.0 Z=-HALFENDCAPMOMLEN+0.75 ALP  1149
C    HAX=90.0 <===                                                        1149
         CALL AGSRESET                                                    1149
         AG_X = 0.0                                                       1149
         AG_Y = 0.0                                                       1149
         AG_Z = -HALFENDCAPMOMLEN+0.75                                    1149
         AG_ALPHAN=AG_ALPHAN+1                                            1149
         AG_ALPHAK(AG_ALPHAN)=1                                           1149
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1149
         AG_TITLE = 'IESE'                                                1149
         AG_EXNAME = 'POSITION'                                           1149
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1149
         CALL AXPOSITION                                                  1149
C    *                                                                    1149
C    Create IESS :                                                        1150
C    Call To Local Procedure IESS                                         1150
         ASSIGN 6660 TO BLOCK_IESS                                        1150
         GO TO  6650                                                      1150
6660     CONTINUE                                                         1150
C    *                                                                    1150
C    Geant POSITION : IESS ===> X=HALFENDCAPMOMWDT-1.8 Y=0.0 Z=-HALFENDC  1151
C    APMOMLEN+0.5 ALPHAX=90.0 <===                                        1151
         CALL AGSRESET                                                    1151
         AG_X = HALFENDCAPMOMWDT-1.8                                      1151
         AG_Y = 0.0                                                       1151
         AG_Z = -HALFENDCAPMOMLEN+0.5                                     1151
         AG_ALPHAN=AG_ALPHAN+1                                            1151
         AG_ALPHAK(AG_ALPHAN)=1                                           1151
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1151
         AG_TITLE = 'IESS'                                                1151
         AG_EXNAME = 'POSITION'                                           1151
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1151
         CALL AXPOSITION                                                  1151
C    *                                                                    1151
C    Create ITNE :                                                        1152
C    Call To Local Procedure ITNE                                         1152
         ASSIGN 6680 TO BLOCK_ITNE                                        1152
         GO TO  6670                                                      1152
6680     CONTINUE                                                         1152
C    *                                                                    1152
C    Geant POSITION : ITNE ===> X=-HALFENDCAPMOMWDT+0.55 Y=0.0 Z=-HALFEN  1154
C    DCAPMOMLEN+0.5 ALPHAX=90.0 <===                                      1154
         CALL AGSRESET                                                    1154
         AG_X = -HALFENDCAPMOMWDT+0.55                                    1154
         AG_Y = 0.0                                                       1154
         AG_Z = -HALFENDCAPMOMLEN+0.5                                     1154
         AG_ALPHAN=AG_ALPHAN+1                                            1154
         AG_ALPHAK(AG_ALPHAN)=1                                           1154
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1154
         AG_TITLE = 'ITNE'                                                1154
         AG_EXNAME = 'POSITION'                                           1154
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1154
         CALL AXPOSITION                                                  1154
C    *                                                                    1154
C    Create ITSE :                                                        1155
C    Call To Local Procedure ITSE                                         1155
         ASSIGN 6700 TO BLOCK_ITSE                                        1155
         GO TO  6690                                                      1155
6700     CONTINUE                                                         1155
C    *                                                                    1155
C    Geant POSITION : ITSE ===> X=HALFENDCAPMOMWDT-0.55 Y=0.0 Z=-HALFEND  1156
C    CAPMOMLEN+0.5 ALPHAX=90.0 <===                                       1156
         CALL AGSRESET                                                    1156
         AG_X = HALFENDCAPMOMWDT-0.55                                     1156
         AG_Y = 0.0                                                       1156
         AG_Z = -HALFENDCAPMOMLEN+0.5                                     1156
         AG_ALPHAN=AG_ALPHAN+1                                            1156
         AG_ALPHAK(AG_ALPHAN)=1                                           1156
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1156
         AG_TITLE = 'ITSE'                                                1156
         AG_EXNAME = 'POSITION'                                           1156
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1156
         CALL AXPOSITION                                                  1156
C    *                                                                    1156
5861  CALL AGSPOPB                                                        1157
      GO TO BLOCK_IECE, (5860 ,5870)                                      1157
C End of Local Procedure                                                  1157
C ----------------------------------------------------------------------  1158
C ------------ ICET                                                       1158
C Begin Local Procedure ICET                                              1159
C ICET IS THE ALUMINIUM ENDCAP OVERLAP BODY TOP BOX VOLUME                1159
6550  CONTINUE                                                            1159
         AG_TITLE='ICET'                                                  1159
         AG_MODULE=MODULE                                                 1159
         AG_MARK='V00'                                                    1159
         CALL AGSPUSH                                                     1159
         AG_MODULE=MODULE                                                 1159
C    *                                                                    1159
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1160
         CALL AGSRESET                                                    1160
         AG_TITLE = 'AIR'                                                 1160
         AG_EXNAME = 'MATERIAL'                                           1160
         AG_PARLIST = 'NONE'                                              1160
         CALL AXMATERIAL                                                  1160
C    *                                                                    1160
C    Geant ATTRIBUTE : ICET ===> SEEN=1 COLO=1 <===                       1160
         CALL AGSRESET                                                    1161
         AG_SEEN = 1                                                      1161
         AG_COLO = 1                                                      1161
         AG_TITLE = 'ICET'                                                1161
         AG_EXNAME = 'ATTRIBUTE'                                          1161
         AG_PARLIST = 'SEEN_COLO'                                         1161
         CALL AXATTRIBUTE                                                 1161
C    *                                                                    1161
C    Geant SHAPE : BOX ===> DX=HALFENDCAPMOMWDT DY=0.03435/2.0, DZ=CONNE  1161
C    CTENDCAPSKINLEN/2.0 <===                                             1161
         CALL AGSRESET                                                    1163
         AG_DX = HALFENDCAPMOMWDT                                         1163
         AG_DY = 0.03435/2.0                                              1163
         AG_DZ = CONNECTENDCAPSKINLEN/2.0                                 1163
         AG_TITLE = 'BOX'                                                 1163
         AG_EXNAME = 'SHAPE'                                              1163
         AG_PARLIST = 'DX_DY_DZ'                                          1163
         CALL AXSHAPE                                                     1163
C    *                                                                    1163
6551  CALL AGSPOPB                                                        1164
      GO TO BLOCK_ICET, (6550 ,6560)                                      1164
C End of Local Procedure                                                  1164
C ----------------------------------------------------------------------  1165
C ------------ ICEB                                                       1165
C Begin Local Procedure ICEB                                              1166
C ICEB IS THE ALUMINIUM ENDCAP OVERLAP BODY BOTTOM BOX VOLUME             1166
6570  CONTINUE                                                            1166
         AG_TITLE='ICEB'                                                  1166
         AG_MODULE=MODULE                                                 1166
         AG_MARK='V00'                                                    1166
         CALL AGSPUSH                                                     1166
         AG_MODULE=MODULE                                                 1166
C    *                                                                    1166
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1167
         CALL AGSRESET                                                    1167
         AG_TITLE = 'AIR'                                                 1167
         AG_EXNAME = 'MATERIAL'                                           1167
         AG_PARLIST = 'NONE'                                              1167
         CALL AXMATERIAL                                                  1167
C    *                                                                    1167
C    Geant ATTRIBUTE : ICEB ===> SEEN=1 COLO=1 <===                       1167
         CALL AGSRESET                                                    1168
         AG_SEEN = 1                                                      1168
         AG_COLO = 1                                                      1168
         AG_TITLE = 'ICEB'                                                1168
         AG_EXNAME = 'ATTRIBUTE'                                          1168
         AG_PARLIST = 'SEEN_COLO'                                         1168
         CALL AXATTRIBUTE                                                 1168
C    *                                                                    1168
C    Geant SHAPE : BOX ===> DX=HALFENDCAPMOMWDT DY=0.03435/2.0, DZ=CONNE  1168
C    CTENDCAPSKINLEN/2.0 <===                                             1168
         CALL AGSRESET                                                    1170
         AG_DX = HALFENDCAPMOMWDT                                         1170
         AG_DY = 0.03435/2.0                                              1170
         AG_DZ = CONNECTENDCAPSKINLEN/2.0                                 1170
         AG_TITLE = 'BOX'                                                 1170
         AG_EXNAME = 'SHAPE'                                              1170
         AG_PARLIST = 'DX_DY_DZ'                                          1170
         CALL AXSHAPE                                                     1170
C    *                                                                    1170
6571  CALL AGSPOPB                                                        1171
      GO TO BLOCK_ICEB, (6570 ,6580)                                      1171
C End of Local Procedure                                                  1171
C ----------------------------------------------------------------------  1172
C ------------ IABE                                                       1172
C Begin Local Procedure IABE                                              1173
C IABE IS THE ENDCAP AIR GROOVE BOX AREA FOR TUBE PLACEMENT               1173
6590  CONTINUE                                                            1173
         AG_TITLE='IABE'                                                  1173
         AG_MODULE=MODULE                                                 1173
         AG_MARK='V00'                                                    1173
         CALL AGSPUSH                                                     1173
         AG_MODULE=MODULE                                                 1173
C    *                                                                    1173
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1174
         CALL AGSRESET                                                    1174
         AG_TITLE = 'AIR'                                                 1174
         AG_EXNAME = 'MATERIAL'                                           1174
         AG_PARLIST = 'NONE'                                              1174
         CALL AXMATERIAL                                                  1174
C    *                                                                    1174
C    Geant ATTRIBUTE : IABE ===> SEEN=1 COLO=1 <===                       1174
         CALL AGSRESET                                                    1175
         AG_SEEN = 1                                                      1175
         AG_COLO = 1                                                      1175
         AG_TITLE = 'IABE'                                                1175
         AG_EXNAME = 'ATTRIBUTE'                                          1175
         AG_PARLIST = 'SEEN_COLO'                                         1175
         CALL AXATTRIBUTE                                                 1175
C    *                                                                    1175
C    Geant SHAPE : BOX ===> DX=0.5065/2.0 DY=0.3031/2.0 DZ=HALFENDCAPMOM  1175
C    LEN <===                                                             1175
         CALL AGSRESET                                                    1176
         AG_DX = 0.5065/2.0                                               1176
         AG_DY = 0.3031/2.0                                               1176
         AG_DZ = HALFENDCAPMOMLEN                                         1176
         AG_TITLE = 'BOX'                                                 1176
         AG_EXNAME = 'SHAPE'                                              1176
         AG_PARLIST = 'DX_DY_DZ'                                          1176
         CALL AXSHAPE                                                     1176
C    *                                                                    1176
6591  CALL AGSPOPB                                                        1177
      GO TO BLOCK_IABE, (6590 ,6600)                                      1177
C End of Local Procedure                                                  1177
C ----------------------------------------------------------------------  1178
C ------------ IAAE                                                       1178
C Begin Local Procedure IAAE                                              1179
C IAAE IS THE ENDCAP AIR GROOVE ARC AREA FOR TUBE PLACEMENT               1179
6610  CONTINUE                                                            1179
         AG_TITLE='IAAE'                                                  1179
         AG_MODULE=MODULE                                                 1179
         AG_MARK='V00'                                                    1179
         CALL AGSPUSH                                                     1179
         AG_MODULE=MODULE                                                 1179
C    *                                                                    1179
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1180
         CALL AGSRESET                                                    1180
         AG_TITLE = 'AIR'                                                 1180
         AG_EXNAME = 'MATERIAL'                                           1180
         AG_PARLIST = 'NONE'                                              1180
         CALL AXMATERIAL                                                  1180
C    *                                                                    1180
C    Geant ATTRIBUTE : IAAE ===> SEEN=1 COLO=1 <===                       1180
         CALL AGSRESET                                                    1181
         AG_SEEN = 1                                                      1181
         AG_COLO = 1                                                      1181
         AG_TITLE = 'IAAE'                                                1181
         AG_EXNAME = 'ATTRIBUTE'                                          1181
         AG_PARLIST = 'SEEN_COLO'                                         1181
         CALL AXATTRIBUTE                                                 1181
C    *                                                                    1181
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=0.5065/2.0 PHI1=180.0, PHI2=360  1181
C    .0 DZ=HALFENDCAPMOMLEN <===                                          1181
         CALL AGSRESET                                                    1183
         AG_RMIN = 0                                                      1183
         AG_RMAX = 0.5065/2.0                                             1183
         AG_PHI1 = 180.0                                                  1183
         AG_PHI2 = 360.0                                                  1183
         AG_DZ = HALFENDCAPMOMLEN                                         1183
         AG_TITLE = 'TUBS'                                                1183
         AG_EXNAME = 'SHAPE'                                              1183
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1183
         CALL AXSHAPE                                                     1183
C    *                                                                    1183
6611  CALL AGSPOPB                                                        1184
      GO TO BLOCK_IAAE, (6610 ,6620)                                      1184
C End of Local Procedure                                                  1184
C ----------------------------------------------------------------------  1185
C ------------ IESE                                                       1185
C Begin Local Procedure IESE                                              1186
C IESE IS THE SCREW THREAD AIR HOLE TUBE VOLUME IN EAST-SIDE ENDCAP       1186
6630  CONTINUE                                                            1186
         AG_TITLE='IESE'                                                  1186
         AG_MODULE=MODULE                                                 1186
         AG_MARK='V00'                                                    1186
         CALL AGSPUSH                                                     1186
         AG_MODULE=MODULE                                                 1186
C    *                                                                    1186
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1187
         CALL AGSRESET                                                    1187
         AG_TITLE = 'AIR'                                                 1187
         AG_EXNAME = 'MATERIAL'                                           1187
         AG_PARLIST = 'NONE'                                              1187
         CALL AXMATERIAL                                                  1187
C    *                                                                    1187
C    Geant ATTRIBUTE : IESE ===> SEEN=1 COLO=1 <===                       1187
         CALL AGSRESET                                                    1188
         AG_SEEN = 1                                                      1188
         AG_COLO = 1                                                      1188
         AG_TITLE = 'IESE'                                                1188
         AG_EXNAME = 'ATTRIBUTE'                                          1188
         AG_PARLIST = 'SEEN_COLO'                                         1188
         CALL AXATTRIBUTE                                                 1188
C    *                                                                    1188
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.635/2.0 DZ=HALFENDCAPMOMTHK  1188
C     <===                                                                1188
         CALL AGSRESET                                                    1189
         AG_RMIN = 0.0                                                    1189
         AG_RMAX = 0.635/2.0                                              1189
         AG_DZ = HALFENDCAPMOMTHK                                         1189
         AG_TITLE = 'TUBE'                                                1189
         AG_EXNAME = 'SHAPE'                                              1189
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1189
         CALL AXSHAPE                                                     1189
C    *                                                                    1189
6631  CALL AGSPOPB                                                        1190
      GO TO BLOCK_IESE, (6630 ,6640)                                      1190
C End of Local Procedure                                                  1190
C ----------------------------------------------------------------------  1191
C ------------ IESS                                                       1191
C Begin Local Procedure IESS                                              1192
C IESS IS THE SMALL THREAD AIR HOLE TUBE VOLUME IN EAST-SIDE ENDCAP       1192
6650  CONTINUE                                                            1192
         AG_TITLE='IESS'                                                  1192
         AG_MODULE=MODULE                                                 1192
         AG_MARK='V00'                                                    1192
         CALL AGSPUSH                                                     1192
         AG_MODULE=MODULE                                                 1192
C    *                                                                    1192
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1193
         CALL AGSRESET                                                    1193
         AG_TITLE = 'AIR'                                                 1193
         AG_EXNAME = 'MATERIAL'                                           1193
         AG_PARLIST = 'NONE'                                              1193
         CALL AXMATERIAL                                                  1193
C    *                                                                    1193
C    Geant ATTRIBUTE : IESS ===> SEEN=1 COLO=1 <===                       1193
         CALL AGSRESET                                                    1194
         AG_SEEN = 1                                                      1194
         AG_COLO = 1                                                      1194
         AG_TITLE = 'IESS'                                                1194
         AG_EXNAME = 'ATTRIBUTE'                                          1194
         AG_PARLIST = 'SEEN_COLO'                                         1194
         CALL AXATTRIBUTE                                                 1194
C    *                                                                    1194
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.205/2.0 DZ=HALFENDCAPMOMTHK  1194
C     <===                                                                1194
         CALL AGSRESET                                                    1195
         AG_RMIN = 0.0                                                    1195
         AG_RMAX = 0.205/2.0                                              1195
         AG_DZ = HALFENDCAPMOMTHK                                         1195
         AG_TITLE = 'TUBE'                                                1195
         AG_EXNAME = 'SHAPE'                                              1195
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1195
         CALL AXSHAPE                                                     1195
C    *                                                                    1195
6651  CALL AGSPOPB                                                        1196
      GO TO BLOCK_IESS, (6650 ,6660)                                      1196
C End of Local Procedure                                                  1196
C ----------------------------------------------------------------------  1197
C ------------ ITNE                                                       1197
C Begin Local Procedure ITNE                                              1199
C ITNE IS THE ENDCAP AIR GROOVE FOR TRANSPORT PROTECTION SCREW THREAD, (  1199
C NORTHEAST)                                                              1199
6670  CONTINUE                                                            1199
         AG_TITLE='ITNE'                                                  1199
         AG_MODULE=MODULE                                                 1199
         AG_MARK='V00'                                                    1199
         CALL AGSPUSH                                                     1199
         AG_MODULE=MODULE                                                 1199
C    *                                                                    1199
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1200
         CALL AGSRESET                                                    1200
         AG_TITLE = 'AIR'                                                 1200
         AG_EXNAME = 'MATERIAL'                                           1200
         AG_PARLIST = 'NONE'                                              1200
         CALL AXMATERIAL                                                  1200
C    *                                                                    1200
C    Geant ATTRIBUTE : ITNE ===> SEEN=1 COLO=1 <===                       1200
         CALL AGSRESET                                                    1201
         AG_SEEN = 1                                                      1201
         AG_COLO = 1                                                      1201
         AG_TITLE = 'ITNE'                                                1201
         AG_EXNAME = 'ATTRIBUTE'                                          1201
         AG_PARLIST = 'SEEN_COLO'                                         1201
         CALL AXATTRIBUTE                                                 1201
C    *                                                                    1201
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.3/2.0 DZ=HALFENDCAPMOMTHK <  1201
C    ===                                                                  1201
         CALL AGSRESET                                                    1202
         AG_RMIN = 0.0                                                    1202
         AG_RMAX = 0.3/2.0                                                1202
         AG_DZ = HALFENDCAPMOMTHK                                         1202
         AG_TITLE = 'TUBE'                                                1202
         AG_EXNAME = 'SHAPE'                                              1202
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1202
         CALL AXSHAPE                                                     1202
C    *                                                                    1202
6671  CALL AGSPOPB                                                        1203
      GO TO BLOCK_ITNE, (6670 ,6680)                                      1203
C End of Local Procedure                                                  1203
C ----------------------------------------------------------------------  1204
C ------------ ITSE                                                       1204
C Begin Local Procedure ITSE                                              1206
C ITSE IS THE ENDCAP AIR GROOVE FOR TRANSPORT PROTECTION SCREW THREAD, (  1206
C SOUTHEAST)                                                              1206
6690  CONTINUE                                                            1206
         AG_TITLE='ITSE'                                                  1206
         AG_MODULE=MODULE                                                 1206
         AG_MARK='V00'                                                    1206
         CALL AGSPUSH                                                     1206
         AG_MODULE=MODULE                                                 1206
C    *                                                                    1206
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1207
         CALL AGSRESET                                                    1207
         AG_TITLE = 'AIR'                                                 1207
         AG_EXNAME = 'MATERIAL'                                           1207
         AG_PARLIST = 'NONE'                                              1207
         CALL AXMATERIAL                                                  1207
C    *                                                                    1207
C    Geant ATTRIBUTE : ITSE ===> SEEN=1 COLO=1 <===                       1207
         CALL AGSRESET                                                    1208
         AG_SEEN = 1                                                      1208
         AG_COLO = 1                                                      1208
         AG_TITLE = 'ITSE'                                                1208
         AG_EXNAME = 'ATTRIBUTE'                                          1208
         AG_PARLIST = 'SEEN_COLO'                                         1208
         CALL AXATTRIBUTE                                                 1208
C    *                                                                    1208
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.3/2.0 DZ=HALFENDCAPMOMTHK <  1208
C    ===                                                                  1208
         CALL AGSRESET                                                    1209
         AG_RMIN = 0.0                                                    1209
         AG_RMAX = 0.3/2.0                                                1209
         AG_DZ = HALFENDCAPMOMTHK                                         1209
         AG_TITLE = 'TUBE'                                                1209
         AG_EXNAME = 'SHAPE'                                              1209
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1209
         CALL AXSHAPE                                                     1209
C    *                                                                    1209
6691  CALL AGSPOPB                                                        1210
      GO TO BLOCK_ITSE, (6690 ,6700)                                      1210
C End of Local Procedure                                                  1210
C ----------------------------------------------------------------------  1211
C ------------ ISCM                                                       1211
C Begin Local Procedure ISCM                                              1212
C ISCM MOTHER OF THE SCREW ASSEMBLY                                       1212
5960  CONTINUE                                                            1212
         AG_TITLE='ISCM'                                                  1212
         AG_MODULE=MODULE                                                 1212
         AG_MARK='V00'                                                    1212
         CALL AGSPUSH                                                     1212
         AG_MODULE=MODULE                                                 1212
C    *                                                                    1212
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1213
         CALL AGSRESET                                                    1213
         AG_TITLE = 'AIR'                                                 1213
         AG_EXNAME = 'MATERIAL'                                           1213
         AG_PARLIST = 'NONE'                                              1213
         CALL AXMATERIAL                                                  1213
C    *                                                                    1213
C    Geant ATTRIBUTE : ISCM ===> SEEN=0 COLO=1 <===                       1213
         CALL AGSRESET                                                    1214
         AG_SEEN = 0                                                      1214
         AG_COLO = 1                                                      1214
         AG_TITLE = 'ISCM'                                                1214
         AG_EXNAME = 'ATTRIBUTE'                                          1214
         AG_PARLIST = 'SEEN_COLO'                                         1214
         CALL AXATTRIBUTE                                                 1214
C    *                                                                    1214
C    Geant SHAPE : BOX ===> DX=0.5*1.25 DY=0.5*1.651 DZ=0.5*1.25 <===     1214
         CALL AGSRESET                                                    1215
         AG_DX = 0.5*1.25                                                 1215
         AG_DY = 0.5*1.651                                                1215
         AG_DZ = 0.5*1.25                                                 1215
         AG_TITLE = 'BOX'                                                 1215
         AG_EXNAME = 'SHAPE'                                              1215
         AG_PARLIST = 'DX_DY_DZ'                                          1215
         CALL AXSHAPE                                                     1215
C    *                                                                    1215
C    Create ISCH :                                                        1216
C    Call To Local Procedure ISCH                                         1216
         ASSIGN 6720 TO BLOCK_ISCH                                        1216
         GO TO  6710                                                      1216
6720     CONTINUE                                                         1216
C    *                                                                    1216
C    Geant POSITION : ISCH ===> X=0.0 Y=0.5*1.651-0.5*0.381 Z=0.0 ALPHAX  1217
C    =90.0 <===                                                           1217
         CALL AGSRESET                                                    1217
         AG_X = 0.0                                                       1217
         AG_Y = 0.5*1.651-0.5*0.381                                       1217
         AG_Z = 0.0                                                       1217
         AG_ALPHAN=AG_ALPHAN+1                                            1217
         AG_ALPHAK(AG_ALPHAN)=1                                           1217
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1217
         AG_TITLE = 'ISCH'                                                1217
         AG_EXNAME = 'POSITION'                                           1217
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1217
         CALL AXPOSITION                                                  1217
C    *                                                                    1217
C    Create ISCS :                                                        1218
C    Call To Local Procedure ISCS                                         1218
         ASSIGN 6740 TO BLOCK_ISCS                                        1218
         GO TO  6730                                                      1218
6740     CONTINUE                                                         1218
C    *                                                                    1218
C    Geant POSITION : ISCS ===> X=0.0 Y=0.5*1.651-0.381-0.5*0.635 Z=0.0   1219
C    ALPHAX=90.0 <===                                                     1219
         CALL AGSRESET                                                    1219
         AG_X = 0.0                                                       1219
         AG_Y = 0.5*1.651-0.381-0.5*0.635                                 1219
         AG_Z = 0.0                                                       1219
         AG_ALPHAN=AG_ALPHAN+1                                            1219
         AG_ALPHAK(AG_ALPHAN)=1                                           1219
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1219
         AG_TITLE = 'ISCS'                                                1219
         AG_EXNAME = 'POSITION'                                           1219
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1219
         CALL AXPOSITION                                                  1219
C    *                                                                    1219
C    Create ISCR :                                                        1220
C    Call To Local Procedure ISCR                                         1220
         ASSIGN 6760 TO BLOCK_ISCR                                        1220
         GO TO  6750                                                      1220
6760     CONTINUE                                                         1220
C    *                                                                    1220
C    Geant POSITION : ISCR ===> X=0.0 Y=-0.5*1.651+0.5*0.635 Z=0.0 ALPHA  1221
C    X=90.0 <===                                                          1221
         CALL AGSRESET                                                    1221
         AG_X = 0.0                                                       1221
         AG_Y = -0.5*1.651+0.5*0.635                                      1221
         AG_Z = 0.0                                                       1221
         AG_ALPHAN=AG_ALPHAN+1                                            1221
         AG_ALPHAK(AG_ALPHAN)=1                                           1221
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1221
         AG_TITLE = 'ISCR'                                                1221
         AG_EXNAME = 'POSITION'                                           1221
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1221
         CALL AXPOSITION                                                  1221
C    *                                                                    1221
C    Create ISWS :                                                        1222
C    Call To Local Procedure ISWS                                         1222
         ASSIGN 6780 TO BLOCK_ISWS                                        1222
         GO TO  6770                                                      1222
6780     CONTINUE                                                         1222
C    *                                                                    1222
C    Geant POSITION : ISWS ===> X=0.0 Y=0.5*1.651-0.381-0.5*0.16 Z=0.0 A  1223
C    LPHAX=90.0 <===                                                      1223
         CALL AGSRESET                                                    1223
         AG_X = 0.0                                                       1223
         AG_Y = 0.5*1.651-0.381-0.5*0.16                                  1223
         AG_Z = 0.0                                                       1223
         AG_ALPHAN=AG_ALPHAN+1                                            1223
         AG_ALPHAK(AG_ALPHAN)=1                                           1223
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1223
         AG_TITLE = 'ISWS'                                                1223
         AG_EXNAME = 'POSITION'                                           1223
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1223
         CALL AXPOSITION                                                  1223
C    *                                                                    1223
5961  CALL AGSPOPB                                                        1224
      GO TO BLOCK_ISCM, (5960 ,5970)                                      1224
C End of Local Procedure                                                  1224
C ----------------------------------------------------------------------  1225
C ------------ ISCH                                                       1225
C Begin Local Procedure ISCH                                              1226
C ISCH IS THE SCREW HAT                                                   1226
6710  CONTINUE                                                            1226
         AG_TITLE='ISCH'                                                  1226
         AG_MODULE=MODULE                                                 1226
         AG_MARK='V00'                                                    1226
         CALL AGSPUSH                                                     1226
         AG_MODULE=MODULE                                                 1226
C    *                                                                    1226
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1227
         CALL AGSRESET                                                    1227
         AG_TITLE = 'ALUMINIUM'                                           1227
         AG_EXNAME = 'MATERIAL'                                           1227
         AG_PARLIST = 'NONE'                                              1227
         CALL AXMATERIAL                                                  1227
C    *                                                                    1227
C    Geant ATTRIBUTE : ISCH ===> SEEN=1 COLO=4 <===                       1227
         CALL AGSRESET                                                    1228
         AG_SEEN = 1                                                      1228
         AG_COLO = 4                                                      1228
         AG_TITLE = 'ISCH'                                                1228
         AG_EXNAME = 'ATTRIBUTE'                                          1228
         AG_PARLIST = 'SEEN_COLO'                                         1228
         CALL AXATTRIBUTE                                                 1228
C    *                                                                    1228
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.5*1.1 DZ=0.5*0.381 <===      1228
         CALL AGSRESET                                                    1229
         AG_RMIN = 0.0                                                    1229
         AG_RMAX = 0.5*1.1                                                1229
         AG_DZ = 0.5*0.381                                                1229
         AG_TITLE = 'TUBE'                                                1229
         AG_EXNAME = 'SHAPE'                                              1229
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1229
         CALL AXSHAPE                                                     1229
C    *                                                                    1229
6711  CALL AGSPOPB                                                        1230
      GO TO BLOCK_ISCH, (6710 ,6720)                                      1230
C End of Local Procedure                                                  1230
C ----------------------------------------------------------------------  1231
C ------------ ISCS                                                       1231
C Begin Local Procedure ISCS                                              1232
C ISCS IS THE SCREW MIDDLE CYLINDER STUDE                                 1232
6730  CONTINUE                                                            1232
         AG_TITLE='ISCS'                                                  1232
         AG_MODULE=MODULE                                                 1232
         AG_MARK='V00'                                                    1232
         CALL AGSPUSH                                                     1232
         AG_MODULE=MODULE                                                 1232
C    *                                                                    1232
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1233
         CALL AGSRESET                                                    1233
         AG_TITLE = 'ALUMINIUM'                                           1233
         AG_EXNAME = 'MATERIAL'                                           1233
         AG_PARLIST = 'NONE'                                              1233
         CALL AXMATERIAL                                                  1233
C    *                                                                    1233
C    Geant ATTRIBUTE : ISCS ===> SEEN=1 COLO=4 <===                       1233
         CALL AGSRESET                                                    1234
         AG_SEEN = 1                                                      1234
         AG_COLO = 4                                                      1234
         AG_TITLE = 'ISCS'                                                1234
         AG_EXNAME = 'ATTRIBUTE'                                          1234
         AG_PARLIST = 'SEEN_COLO'                                         1234
         CALL AXATTRIBUTE                                                 1234
C    *                                                                    1234
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.5*0.635 DZ=0.5*0.635 <===    1234
         CALL AGSRESET                                                    1235
         AG_RMIN = 0.0                                                    1235
         AG_RMAX = 0.5*0.635                                              1235
         AG_DZ = 0.5*0.635                                                1235
         AG_TITLE = 'TUBE'                                                1235
         AG_EXNAME = 'SHAPE'                                              1235
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1235
         CALL AXSHAPE                                                     1235
C    *                                                                    1235
6731  CALL AGSPOPB                                                        1236
      GO TO BLOCK_ISCS, (6730 ,6740)                                      1236
C End of Local Procedure                                                  1236
C ----------------------------------------------------------------------  1237
C ------------ ISCR                                                       1237
C Begin Local Procedure ISCR                                              1238
C ISCR IS THE SCREW STUD                                                  1238
6750  CONTINUE                                                            1238
         AG_TITLE='ISCR'                                                  1238
         AG_MODULE=MODULE                                                 1238
         AG_MARK='V00'                                                    1238
         CALL AGSPUSH                                                     1238
         AG_MODULE=MODULE                                                 1238
C    *                                                                    1238
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1239
         CALL AGSRESET                                                    1239
         AG_TITLE = 'ALUMINIUM'                                           1239
         AG_EXNAME = 'MATERIAL'                                           1239
         AG_PARLIST = 'NONE'                                              1239
         CALL AXMATERIAL                                                  1239
C    *                                                                    1239
C    Geant ATTRIBUTE : ISCR ===> SEEN=1 COLO=4 <===                       1239
         CALL AGSRESET                                                    1240
         AG_SEEN = 1                                                      1240
         AG_COLO = 4                                                      1240
         AG_TITLE = 'ISCR'                                                1240
         AG_EXNAME = 'ATTRIBUTE'                                          1240
         AG_PARLIST = 'SEEN_COLO'                                         1240
         CALL AXATTRIBUTE                                                 1240
C    *                                                                    1240
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.5*0.3928 DZ=0.5*0.635 <===   1240
         CALL AGSRESET                                                    1241
         AG_RMIN = 0.0                                                    1241
         AG_RMAX = 0.5*0.3928                                             1241
         AG_DZ = 0.5*0.635                                                1241
         AG_TITLE = 'TUBE'                                                1241
         AG_EXNAME = 'SHAPE'                                              1241
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1241
         CALL AXSHAPE                                                     1241
C    *                                                                    1241
6751  CALL AGSPOPB                                                        1242
      GO TO BLOCK_ISCR, (6750 ,6760)                                      1242
C End of Local Procedure                                                  1242
C ----------------------------------------------------------------------  1243
C ------------ ISWS                                                       1243
C Begin Local Procedure ISWS                                              1244
C ISWS IS THE SCREW WASHER                                                1244
6770  CONTINUE                                                            1244
         AG_TITLE='ISWS'                                                  1244
         AG_MODULE=MODULE                                                 1244
         AG_MARK='V00'                                                    1244
         CALL AGSPUSH                                                     1244
         AG_MODULE=MODULE                                                 1244
C    *                                                                    1244
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1245
         CALL AGSRESET                                                    1245
         AG_TITLE = 'ALUMINIUM'                                           1245
         AG_EXNAME = 'MATERIAL'                                           1245
         AG_PARLIST = 'NONE'                                              1245
         CALL AXMATERIAL                                                  1245
C    *                                                                    1245
C    Geant ATTRIBUTE : ISWS ===> SEEN=1 COLO=4 <===                       1245
         CALL AGSRESET                                                    1246
         AG_SEEN = 1                                                      1246
         AG_COLO = 4                                                      1246
         AG_TITLE = 'ISWS'                                                1246
         AG_EXNAME = 'ATTRIBUTE'                                          1246
         AG_PARLIST = 'SEEN_COLO'                                         1246
         CALL AXATTRIBUTE                                                 1246
C    *                                                                    1246
C    Geant SHAPE : TUBE ===> RMIN=0.5*0.64 RMAX=0.5*1.25 DZ=0.5*0.16 <==  1246
C    =                                                                    1246
         CALL AGSRESET                                                    1247
         AG_RMIN = 0.5*0.64                                               1247
         AG_RMAX = 0.5*1.25                                               1247
         AG_DZ = 0.5*0.16                                                 1247
         AG_TITLE = 'TUBE'                                                1247
         AG_EXNAME = 'SHAPE'                                              1247
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1247
         CALL AXSHAPE                                                     1247
C    *                                                                    1247
6771  CALL AGSPOPB                                                        1248
      GO TO BLOCK_ISWS, (6770 ,6780)                                      1248
C End of Local Procedure                                                  1248
C ----------------------------------------------------------------------  1249
C ------------ IBSR                                                       1249
C Begin Local Procedure IBSR                                              1250
C IBSR IS THE SUPPORT STRUCTURE MOTHER VOLUME                             1250
5990  CONTINUE                                                            1250
         AG_TITLE='IBSR'                                                  1250
         AG_MODULE=MODULE                                                 1250
         AG_MARK='V00'                                                    1250
         CALL AGSPUSH                                                     1250
         AG_MODULE=MODULE                                                 1250
C    *                                                                    1250
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1251
         CALL AGSRESET                                                    1251
         AG_TITLE = 'AIR'                                                 1251
         AG_EXNAME = 'MATERIAL'                                           1251
         AG_PARLIST = 'NONE'                                              1251
         CALL AXMATERIAL                                                  1251
C    *                                                                    1251
C    Geant ATTRIBUTE : IBSR ===> SEEN=0 COLO=1 <===                       1251
         CALL AGSRESET                                                    1252
         AG_SEEN = 0                                                      1252
         AG_COLO = 1                                                      1252
         AG_TITLE = 'IBSR'                                                1252
         AG_EXNAME = 'ATTRIBUTE'                                          1252
         AG_PARLIST = 'SEEN_COLO'                                         1252
         CALL AXATTRIBUTE                                                 1252
C    *                                                                    1252
C    Geant SHAPE : BOX ===> DX=HALFSRMOMWDT DY=HALFSRMOMTHK, DZ=HALFSRMO  1252
C    MLEN <===                                                            1252
         CALL AGSRESET                                                    1254
         AG_DX = HALFSRMOMWDT                                             1254
         AG_DY = HALFSRMOMTHK                                             1254
         AG_DZ = HALFSRMOMLEN                                             1254
         AG_TITLE = 'BOX'                                                 1254
         AG_EXNAME = 'SHAPE'                                              1254
         AG_PARLIST = 'DX_DY_DZ'                                          1254
         CALL AXSHAPE                                                     1254
C    *                                                                    1254
C    Create ISRR :                                                        1255
C    Call To Local Procedure ISRR                                         1255
         ASSIGN 6800 TO BLOCK_ISRR                                        1255
         GO TO  6790                                                      1255
6800     CONTINUE                                                         1255
C    *                                                                    1255
C    Geant POSITION : ISRR ===> X=HALFSRMOMWDT-0.15-0.5*0.207 Y=HALFSRMO  1257
C    MTHK-0.7148-0.3-0.5*1.3553 Z=-HALFSRMOMLEN+0.5*0.9525 <===           1257
         CALL AGSRESET                                                    1257
         AG_X = HALFSRMOMWDT-0.15-0.5*0.207                               1257
         AG_Y = HALFSRMOMTHK-0.7148-0.3-0.5*1.3553                        1257
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1257
         AG_TITLE = 'ISRR'                                                1257
         AG_EXNAME = 'POSITION'                                           1257
         AG_PARLIST = 'X_Y_Z'                                             1257
         CALL AXPOSITION                                                  1257
C    *                                                                    1257
C    Create ISRB :                                                        1258
C    Call To Local Procedure ISRB                                         1258
         ASSIGN 6820 TO BLOCK_ISRB                                        1258
         GO TO  6810                                                      1258
6820     CONTINUE                                                         1258
C    *                                                                    1258
C    Geant POSITION : ISRB ===> X=0.0 Y=HALFSRMOMTHK-0.7148-0.5*0.3 Z=-H  1260
C    ALFSRMOMLEN+0.5*0.9525 KONLY='MANY' <===                             1260
         CALL AGSRESET                                                    1260
         AG_X = 0.0                                                       1260
         AG_Y = HALFSRMOMTHK-0.7148-0.5*0.3                               1260
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1260
         AG_KONLY = 'MANY'                                                1260
         AG_TITLE = 'ISRB'                                                1260
         AG_EXNAME = 'POSITION'                                           1260
         AG_PARLIST = 'X_Y_Z_KONLY'                                       1260
         CALL AXPOSITION                                                  1260
C    *                                                                    1260
C    Create IRBA :                                                        1261
C    Call To Local Procedure IRBA                                         1261
         ASSIGN 6840 TO BLOCK_IRBA                                        1261
         GO TO  6830                                                      1261
6840     CONTINUE                                                         1261
C    *                                                                    1261
C    Geant POSITION : IRBA ===> X=HALFSRMOMWDT-0.15-0.5*0.3 Y=HALFSRMOMT  1263
C    HK-0.7148 Z=-HALFSRMOMLEN+0.5*0.9525 <===                            1263
         CALL AGSRESET                                                    1263
         AG_X = HALFSRMOMWDT-0.15-0.5*0.3                                 1263
         AG_Y = HALFSRMOMTHK-0.7148                                       1263
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1263
         AG_TITLE = 'IRBA'                                                1263
         AG_EXNAME = 'POSITION'                                           1263
         AG_PARLIST = 'X_Y_Z'                                             1263
         CALL AXPOSITION                                                  1263
C    *                                                                    1263
C    Create ISRL :                                                        1264
C    Call To Local Procedure ISRL                                         1264
         ASSIGN 6860 TO BLOCK_ISRL                                        1264
         GO TO  6850                                                      1264
6860     CONTINUE                                                         1264
C    *                                                                    1264
C    Geant POSITION : ISRL ===> X=-HALFSRMOMWDT+0.5*0.7452+0.23755 Y=HAL  1267
C    FSRMOMTHK-0.7148-0.3-0.1105-0.88655 Z=-HALFSRMOMLEN+0.5*0.9525 ALPH  1267
C    AZ=-75.0 KONLY='MANY' <===                                           1267
         CALL AGSRESET                                                    1267
         AG_X = -HALFSRMOMWDT+0.5*0.7452+0.23755                          1267
         AG_Y = HALFSRMOMTHK-0.7148-0.3-0.1105-0.88655                    1267
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1267
         AG_ALPHAN=AG_ALPHAN+1                                            1267
         AG_ALPHAK(AG_ALPHAN)=3                                           1267
         AG_ALPHAL(AG_ALPHAN)=-75.0                                       1267
         AG_KONLY = 'MANY'                                                1267
         AG_TITLE = 'ISRL'                                                1267
         AG_EXNAME = 'POSITION'                                           1267
         AG_PARLIST = 'X_Y_Z_ALPHAZ_KONLY'                                1267
         CALL AXPOSITION                                                  1267
C    *                                                                    1267
C    Create ISRC :                                                        1268
C    Call To Local Procedure ISRC                                         1268
         ASSIGN 6880 TO BLOCK_ISRC                                        1268
         GO TO  6870                                                      1268
6880     CONTINUE                                                         1268
C    *                                                                    1268
C    Geant POSITION : ISRC ===> X=-HALFSRMOMWDT+0.3652 Y=HALFSRMOMTHK-0.  1270
C    7148-0.3-0.5*0.1105 Z=-HALFSRMOMLEN+0.5*0.9525 <===                  1270
         CALL AGSRESET                                                    1270
         AG_X = -HALFSRMOMWDT+0.3652                                      1270
         AG_Y = HALFSRMOMTHK-0.7148-0.3-0.5*0.1105                        1270
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1270
         AG_TITLE = 'ISRC'                                                1270
         AG_EXNAME = 'POSITION'                                           1270
         AG_PARLIST = 'X_Y_Z'                                             1270
         CALL AXPOSITION                                                  1270
C    *                                                                    1270
C    Create ISRA :                                                        1271
C    Call To Local Procedure ISRA                                         1271
         ASSIGN 6900 TO BLOCK_ISRA                                        1271
         GO TO  6890                                                      1271
6900     CONTINUE                                                         1271
C    *                                                                    1271
C    Geant POSITION : ISRA ===> X=HALFSRMOMWDT-0.15+0.0882*MODULEINNERRA  1273
C    DIUS Y=HALFSRMOMTHK-0.7148-0.3-1.3553-0.153-0.9961*MODULEINNERRADIU  1273
C    S Z=0.0 <===                                                         1273
         CALL AGSRESET                                                    1273
         AG_X = HALFSRMOMWDT-0.15+0.0882*MODULEINNERRADIUS                1273
         AG_Y =                                                           1273
     *   HALFSRMOMTHK-0.7148-0.3-1.3553-0.153-0.9961*MODULEINNERRADIUS    1273
         AG_Z = 0.0                                                       1273
         AG_TITLE = 'ISRA'                                                1273
         AG_EXNAME = 'POSITION'                                           1273
         AG_PARLIST = 'X_Y_Z'                                             1273
         CALL AXPOSITION                                                  1273
C    *                                                                    1273
C    Create ISRO :                                                        1274
C    Call To Local Procedure ISRO                                         1274
         ASSIGN 6920 TO BLOCK_ISRO                                        1274
         GO TO  6910                                                      1274
6920     CONTINUE                                                         1274
C    *                                                                    1274
C    Geant POSITION : ISRO ===> X=HALFSRMOMWDT-0.5*0.15 Y=HALFSRMOMTHK-0  1276
C    .5*0.7148 Z=-HALFSRMOMLEN+0.5*0.9525 <===                            1276
         CALL AGSRESET                                                    1276
         AG_X = HALFSRMOMWDT-0.5*0.15                                     1276
         AG_Y = HALFSRMOMTHK-0.5*0.7148                                   1276
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1276
         AG_TITLE = 'ISRO'                                                1276
         AG_EXNAME = 'POSITION'                                           1276
         AG_PARLIST = 'X_Y_Z'                                             1276
         CALL AXPOSITION                                                  1276
C    *                                                                    1276
C    Create IROA :                                                        1277
C    Call To Local Procedure IROA                                         1277
         ASSIGN 6940 TO BLOCK_IROA                                        1277
         GO TO  6930                                                      1277
6940     CONTINUE                                                         1277
C    *                                                                    1277
C    Geant POSITION : IROA ===> X=HALFSRMOMWDT-0.15 Y=HALFSRMOMTHK-0.1-0  1279
C    .5*0.2 Z=-HALFSRMOMLEN+0.5*0.9525 ALPHAZ=90.0 <===                   1279
         CALL AGSRESET                                                    1279
         AG_X = HALFSRMOMWDT-0.15                                         1279
         AG_Y = HALFSRMOMTHK-0.1-0.5*0.2                                  1279
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1279
         AG_ALPHAN=AG_ALPHAN+1                                            1279
         AG_ALPHAK(AG_ALPHAN)=3                                           1279
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1279
         AG_TITLE = 'IROA'                                                1279
         AG_EXNAME = 'POSITION'                                           1279
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1279
         CALL AXPOSITION                                                  1279
C    *                                                                    1279
C    Create ISRS :                                                        1280
C    Call To Local Procedure ISRS                                         1280
         ASSIGN 6960 TO BLOCK_ISRS                                        1280
         GO TO  6950                                                      1280
6960     CONTINUE                                                         1280
C    *                                                                    1280
C    Geant POSITION : ISRS ===> X=HALFSRMOMWDT-0.15-0.5*0.3116 Y=HALFSRM  1282
C    OMTHK-0.5*0.1 Z=-HALFSRMOMLEN+0.5*0.9525 <===                        1282
         CALL AGSRESET                                                    1282
         AG_X = HALFSRMOMWDT-0.15-0.5*0.3116                              1282
         AG_Y = HALFSRMOMTHK-0.5*0.1                                      1282
         AG_Z = -HALFSRMOMLEN+0.5*0.9525                                  1282
         AG_TITLE = 'ISRS'                                                1282
         AG_EXNAME = 'POSITION'                                           1282
         AG_PARLIST = 'X_Y_Z'                                             1282
         CALL AXPOSITION                                                  1282
C    *                                                                    1282
C    Create ISRH :                                                        1283
C    Call To Local Procedure ISRH                                         1283
         ASSIGN 6980 TO BLOCK_ISRH                                        1283
         GO TO  6970                                                      1283
6980     CONTINUE                                                         1283
C    *                                                                    1283
C    Geant POSITION : ISRH ===> X=-HALFSRMOMWDT+0.3572 Y=HALFSRMOMTHK-0.  1285
C    7148-0.5*1.2438 Z=-HALFSRMOMLEN+0.47625 ALPHAX=90.0 <===             1285
         CALL AGSRESET                                                    1285
         AG_X = -HALFSRMOMWDT+0.3572                                      1285
         AG_Y = HALFSRMOMTHK-0.7148-0.5*1.2438                            1285
         AG_Z = -HALFSRMOMLEN+0.47625                                     1285
         AG_ALPHAN=AG_ALPHAN+1                                            1285
         AG_ALPHAK(AG_ALPHAN)=1                                           1285
         AG_ALPHAL(AG_ALPHAN)=90.0                                        1285
         AG_TITLE = 'ISRH'                                                1285
         AG_EXNAME = 'POSITION'                                           1285
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      1285
         CALL AXPOSITION                                                  1285
C    *                                                                    1285
5991  CALL AGSPOPB                                                        1286
      GO TO BLOCK_IBSR, (5990 ,6000)                                      1286
C End of Local Procedure                                                  1286
C ----------------------------------------------------------------------  1287
C ------------ ISRR                                                       1287
C Begin Local Procedure ISRR                                              1288
C ISRR IS THE SUPPORT RING STRUCTURE BOX SUB-VOLUME SOUTH SIDE            1288
6790  CONTINUE                                                            1288
         AG_TITLE='ISRR'                                                  1288
         AG_MODULE=MODULE                                                 1288
         AG_MARK='V00'                                                    1288
         CALL AGSPUSH                                                     1288
         AG_MODULE=MODULE                                                 1288
C    *                                                                    1288
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1289
         CALL AGSRESET                                                    1289
         AG_TITLE = 'PEEKCF30'                                            1289
         AG_EXNAME = 'MATERIAL'                                           1289
         AG_PARLIST = 'NONE'                                              1289
         CALL AXMATERIAL                                                  1289
C    *                                                                    1289
C    Geant ATTRIBUTE : ISRR ===> SEEN=1 COLO=3 <===                       1289
         CALL AGSRESET                                                    1290
         AG_SEEN = 1                                                      1290
         AG_COLO = 3                                                      1290
         AG_TITLE = 'ISRR'                                                1290
         AG_EXNAME = 'ATTRIBUTE'                                          1290
         AG_PARLIST = 'SEEN_COLO'                                         1290
         CALL AXATTRIBUTE                                                 1290
C    *                                                                    1290
C    Geant SHAPE : BOX ===> DX=0.5*0.207 DY=0.5*1.3553 DZ=0.5*0.9525 <==  1290
C    =                                                                    1290
         CALL AGSRESET                                                    1291
         AG_DX = 0.5*0.207                                                1291
         AG_DY = 0.5*1.3553                                               1291
         AG_DZ = 0.5*0.9525                                               1291
         AG_TITLE = 'BOX'                                                 1291
         AG_EXNAME = 'SHAPE'                                              1291
         AG_PARLIST = 'DX_DY_DZ'                                          1291
         CALL AXSHAPE                                                     1291
C    *                                                                    1291
6791  CALL AGSPOPB                                                        1292
      GO TO BLOCK_ISRR, (6790 ,6800)                                      1292
C End of Local Procedure                                                  1292
C ----------------------------------------------------------------------  1293
C ------------ ISRB                                                       1293
C Begin Local Procedure ISRB                                              1294
C ISRB IS THE SUPPORT RING STRUCTURE BOX SUB-VOLUME TOP SIDE              1294
6810  CONTINUE                                                            1294
         AG_TITLE='ISRB'                                                  1294
         AG_MODULE=MODULE                                                 1294
         AG_MARK='V00'                                                    1294
         CALL AGSPUSH                                                     1294
         AG_MODULE=MODULE                                                 1294
C    *                                                                    1294
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1295
         CALL AGSRESET                                                    1295
         AG_TITLE = 'PEEKCF30'                                            1295
         AG_EXNAME = 'MATERIAL'                                           1295
         AG_PARLIST = 'NONE'                                              1295
         CALL AXMATERIAL                                                  1295
C    *                                                                    1295
C    Geant ATTRIBUTE : ISRB ===> SEEN=1 COLO=3 <===                       1295
         CALL AGSRESET                                                    1296
         AG_SEEN = 1                                                      1296
         AG_COLO = 3                                                      1296
         AG_TITLE = 'ISRB'                                                1296
         AG_EXNAME = 'ATTRIBUTE'                                          1296
         AG_PARLIST = 'SEEN_COLO'                                         1296
         CALL AXATTRIBUTE                                                 1296
C    *                                                                    1296
C    Geant SHAPE : BOX ===> DX=0.5*3.5575 DY=0.5*0.3 DZ=0.5*0.9525 <===   1296
         CALL AGSRESET                                                    1297
         AG_DX = 0.5*3.5575                                               1297
         AG_DY = 0.5*0.3                                                  1297
         AG_DZ = 0.5*0.9525                                               1297
         AG_TITLE = 'BOX'                                                 1297
         AG_EXNAME = 'SHAPE'                                              1297
         AG_PARLIST = 'DX_DY_DZ'                                          1297
         CALL AXSHAPE                                                     1297
C    *                                                                    1297
6811  CALL AGSPOPB                                                        1298
      GO TO BLOCK_ISRB, (6810 ,6820)                                      1298
C End of Local Procedure                                                  1298
C ----------------------------------------------------------------------  1299
C ------------ IRBA                                                       1299
C Begin Local Procedure IRBA                                              1300
C IRBA IS THE SUPPORT RING STRUCTURE AIR GROOVE                           1300
6830  CONTINUE                                                            1300
         AG_TITLE='IRBA'                                                  1300
         AG_MODULE=MODULE                                                 1300
         AG_MARK='V00'                                                    1300
         CALL AGSPUSH                                                     1300
         AG_MODULE=MODULE                                                 1300
C    *                                                                    1300
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1301
         CALL AGSRESET                                                    1301
         AG_TITLE = 'AIR'                                                 1301
         AG_EXNAME = 'MATERIAL'                                           1301
         AG_PARLIST = 'NONE'                                              1301
         CALL AXMATERIAL                                                  1301
C    *                                                                    1301
C    Geant ATTRIBUTE : ISRA ===> SEEN=1 COLO=3 <===                       1301
         CALL AGSRESET                                                    1302
         AG_SEEN = 1                                                      1302
         AG_COLO = 3                                                      1302
         AG_TITLE = 'ISRA'                                                1302
         AG_EXNAME = 'ATTRIBUTE'                                          1302
         AG_PARLIST = 'SEEN_COLO'                                         1302
         CALL AXATTRIBUTE                                                 1302
C    *                                                                    1302
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=0.15 PHI1=180.0 PHI2=360.0, DZ=  1302
C    0.5*0.9525 <===                                                      1302
         CALL AGSRESET                                                    1304
         AG_RMIN = 0                                                      1304
         AG_RMAX = 0.15                                                   1304
         AG_PHI1 = 180.0                                                  1304
         AG_PHI2 = 360.0                                                  1304
         AG_DZ = 0.5*0.9525                                               1304
         AG_TITLE = 'TUBS'                                                1304
         AG_EXNAME = 'SHAPE'                                              1304
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1304
         CALL AXSHAPE                                                     1304
C    *                                                                    1304
6831  CALL AGSPOPB                                                        1305
      GO TO BLOCK_IRBA, (6830 ,6840)                                      1305
C End of Local Procedure                                                  1305
C ----------------------------------------------------------------------  1306
C ------------ ISRL                                                       1306
C Begin Local Procedure ISRL                                              1307
C ISRL IS THE SUPPORT RING STRUCTURE TRAPEZOID SUB-VOLUME NORTH SIDE      1307
6850  CONTINUE                                                            1307
         AG_TITLE='ISRL'                                                  1307
         AG_MODULE=MODULE                                                 1307
         AG_MARK='V00'                                                    1307
         CALL AGSPUSH                                                     1307
         AG_MODULE=MODULE                                                 1307
C    *                                                                    1307
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1308
         CALL AGSRESET                                                    1308
         AG_TITLE = 'PEEKCF30'                                            1308
         AG_EXNAME = 'MATERIAL'                                           1308
         AG_PARLIST = 'NONE'                                              1308
         CALL AXMATERIAL                                                  1308
C    *                                                                    1308
C    Geant ATTRIBUTE : ISRL ===> SEEN=1 COLO=3 <===                       1308
         CALL AGSRESET                                                    1309
         AG_SEEN = 1                                                      1309
         AG_COLO = 3                                                      1309
         AG_TITLE = 'ISRL'                                                1309
         AG_EXNAME = 'ATTRIBUTE'                                          1309
         AG_PARLIST = 'SEEN_COLO'                                         1309
         CALL AXATTRIBUTE                                                 1309
C    *                                                                    1309
C    Geant SHAPE : TRAP ===> DZ=0.5*0.9525 THET=0 PHI=0 H1=0.5*0.7198, B  1309
C    L1=0.5*1.9321 TL1=0.5*1.7392 ALP1=7.63192, H2=0.5*0.7198 BL2=0.5*1.  1309
C    9321 TL2=0.5*1.7392, ALP2=7.63192 <===                               1309
         CALL AGSRESET                                                    1313
         AG_DZ = 0.5*0.9525                                               1313
         AG_THET = 0                                                      1313
         AG_PHI = 0                                                       1313
         AG_H1 = 0.5*0.7198                                               1313
         AG_BL1 = 0.5*1.9321                                              1313
         AG_TL1 = 0.5*1.7392                                              1313
         AG_ALP1 = 7.63192                                                1313
         AG_H2 = 0.5*0.7198                                               1313
         AG_BL2 = 0.5*1.9321                                              1313
         AG_TL2 = 0.5*1.7392                                              1313
         AG_ALP2 = 7.63192                                                1313
         AG_TITLE = 'TRAP'                                                1313
         AG_EXNAME = 'SHAPE'                                              1313
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1313
         CALL AXSHAPE                                                     1313
C    *                                                                    1313
6851  CALL AGSPOPB                                                        1314
      GO TO BLOCK_ISRL, (6850 ,6860)                                      1314
C End of Local Procedure                                                  1314
C ----------------------------------------------------------------------  1315
C ------------ ISRC                                                       1315
C Begin Local Procedure ISRC                                              1316
C ISRC IS THE SUPPORT RING STRUCTURE CONNECT AREA                         1316
6870  CONTINUE                                                            1316
         AG_TITLE='ISRC'                                                  1316
         AG_MODULE=MODULE                                                 1316
         AG_MARK='V00'                                                    1316
         CALL AGSPUSH                                                     1316
         AG_MODULE=MODULE                                                 1316
C    *                                                                    1316
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1317
         CALL AGSRESET                                                    1317
         AG_TITLE = 'PEEKCF30'                                            1317
         AG_EXNAME = 'MATERIAL'                                           1317
         AG_PARLIST = 'NONE'                                              1317
         CALL AXMATERIAL                                                  1317
C    *                                                                    1317
C    Geant ATTRIBUTE : ISRC ===> SEEN=1 COLO=3 <===                       1317
         CALL AGSRESET                                                    1318
         AG_SEEN = 1                                                      1318
         AG_COLO = 3                                                      1318
         AG_TITLE = 'ISRC'                                                1318
         AG_EXNAME = 'ATTRIBUTE'                                          1318
         AG_PARLIST = 'SEEN_COLO'                                         1318
         CALL AXATTRIBUTE                                                 1318
C    *                                                                    1318
C    Geant SHAPE : TRAP ===> DZ=0.5*0.9525 THET=0 PHI=0 H1=0.5*0.1105, B  1318
C    L1=0.5*0.7452 TL1=0.5*0.7156 ALP1=-7.6286, H2=0.5*0.1105 BL2=0.5*0.  1318
C    7452 TL2=0.5*0.7156, ALP2=-7.6286 <===                               1318
         CALL AGSRESET                                                    1322
         AG_DZ = 0.5*0.9525                                               1322
         AG_THET = 0                                                      1322
         AG_PHI = 0                                                       1322
         AG_H1 = 0.5*0.1105                                               1322
         AG_BL1 = 0.5*0.7452                                              1322
         AG_TL1 = 0.5*0.7156                                              1322
         AG_ALP1 = -7.6286                                                1322
         AG_H2 = 0.5*0.1105                                               1322
         AG_BL2 = 0.5*0.7452                                              1322
         AG_TL2 = 0.5*0.7156                                              1322
         AG_ALP2 = -7.6286                                                1322
         AG_TITLE = 'TRAP'                                                1322
         AG_EXNAME = 'SHAPE'                                              1322
         AG_PARLIST = 'DZ_THET_PHI_H1_BL1_TL1_ALP1_H2_BL2_TL2_ALP2'       1322
         CALL AXSHAPE                                                     1322
C    *                                                                    1322
6871  CALL AGSPOPB                                                        1323
      GO TO BLOCK_ISRC, (6870 ,6880)                                      1323
C End of Local Procedure                                                  1323
C ----------------------------------------------------------------------  1324
C ------------ ISRA                                                       1324
C Begin Local Procedure ISRA                                              1325
C ISRA IS THE SUPPORT RING STRUCTURE ARC AREA                             1325
6890  CONTINUE                                                            1325
         AG_TITLE='ISRA'                                                  1325
         AG_MODULE=MODULE                                                 1325
         AG_MARK='V00'                                                    1325
         CALL AGSPUSH                                                     1325
         AG_MODULE=MODULE                                                 1325
C    *                                                                    1325
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1326
         CALL AGSRESET                                                    1326
         AG_TITLE = 'PEEKCF30'                                            1326
         AG_EXNAME = 'MATERIAL'                                           1326
         AG_PARLIST = 'NONE'                                              1326
         CALL AXMATERIAL                                                  1326
C    *                                                                    1326
C    Geant ATTRIBUTE : ISRA ===> SEEN=1 COLO=3 <===                       1326
         CALL AGSRESET                                                    1327
         AG_SEEN = 1                                                      1327
         AG_COLO = 3                                                      1327
         AG_TITLE = 'ISRA'                                                1327
         AG_EXNAME = 'ATTRIBUTE'                                          1327
         AG_PARLIST = 'SEEN_COLO'                                         1327
         CALL AXATTRIBUTE                                                 1327
C    *                                                                    1327
C    Geant SHAPE : TUBS ===> RMIN=0.5*23.8506 RMAX=0.5*24.1506, PHI1=90.  1327
C    0+5.06 PHI2=104.129+5.06 DZ=0.5*1.27 <===                            1327
         CALL AGSRESET                                                    1329
         AG_RMIN = 0.5*23.8506                                            1329
         AG_RMAX = 0.5*24.1506                                            1329
         AG_PHI1 = 90.0+5.06                                              1329
         AG_PHI2 = 104.129+5.06                                           1329
         AG_DZ = 0.5*1.27                                                 1329
         AG_TITLE = 'TUBS'                                                1329
         AG_EXNAME = 'SHAPE'                                              1329
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1329
         CALL AXSHAPE                                                     1329
C    *                                                                    1329
6891  CALL AGSPOPB                                                        1330
      GO TO BLOCK_ISRA, (6890 ,6900)                                      1330
C End of Local Procedure                                                  1330
C ----------------------------------------------------------------------  1331
C ------------ ISRO                                                       1331
C Begin Local Procedure ISRO                                              1332
C ISRO IS THE SUPPORT RING STRUCTURE TOP EDGE                             1332
6910  CONTINUE                                                            1332
         AG_TITLE='ISRO'                                                  1332
         AG_MODULE=MODULE                                                 1332
         AG_MARK='V00'                                                    1332
         CALL AGSPUSH                                                     1332
         AG_MODULE=MODULE                                                 1332
C    *                                                                    1332
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1333
         CALL AGSRESET                                                    1333
         AG_TITLE = 'PEEKCF30'                                            1333
         AG_EXNAME = 'MATERIAL'                                           1333
         AG_PARLIST = 'NONE'                                              1333
         CALL AXMATERIAL                                                  1333
C    *                                                                    1333
C    Geant ATTRIBUTE : ISRO ===> SEEN=1 COLO=3 <===                       1333
         CALL AGSRESET                                                    1334
         AG_SEEN = 1                                                      1334
         AG_COLO = 3                                                      1334
         AG_TITLE = 'ISRO'                                                1334
         AG_EXNAME = 'ATTRIBUTE'                                          1334
         AG_PARLIST = 'SEEN_COLO'                                         1334
         CALL AXATTRIBUTE                                                 1334
C    *                                                                    1334
C    Geant SHAPE : BOX ===> DX=0.5*0.15 DY=0.5*0.7148 DZ=0.5*0.9525 <===  1334
         CALL AGSRESET                                                    1335
         AG_DX = 0.5*0.15                                                 1335
         AG_DY = 0.5*0.7148                                               1335
         AG_DZ = 0.5*0.9525                                               1335
         AG_TITLE = 'BOX'                                                 1335
         AG_EXNAME = 'SHAPE'                                              1335
         AG_PARLIST = 'DX_DY_DZ'                                          1335
         CALL AXSHAPE                                                     1335
C    *                                                                    1335
6911  CALL AGSPOPB                                                        1336
      GO TO BLOCK_ISRO, (6910 ,6920)                                      1336
C End of Local Procedure                                                  1336
C ----------------------------------------------------------------------  1337
C ------------ IROA                                                       1337
C Begin Local Procedure IROA                                              1338
C IROA IS THE SUPPORT RING STRUCTURE AIR GROOVE IN ISRO                   1338
6930  CONTINUE                                                            1338
         AG_TITLE='IROA'                                                  1338
         AG_MODULE=MODULE                                                 1338
         AG_MARK='V00'                                                    1338
         CALL AGSPUSH                                                     1338
         AG_MODULE=MODULE                                                 1338
C    *                                                                    1338
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1339
         CALL AGSRESET                                                    1339
         AG_TITLE = 'PEEKCF30'                                            1339
         AG_EXNAME = 'MATERIAL'                                           1339
         AG_PARLIST = 'NONE'                                              1339
         CALL AXMATERIAL                                                  1339
C    *                                                                    1339
C    Geant ATTRIBUTE : IROA ===> SEEN=1 COLO=3 <===                       1339
         CALL AGSRESET                                                    1340
         AG_SEEN = 1                                                      1340
         AG_COLO = 3                                                      1340
         AG_TITLE = 'IROA'                                                1340
         AG_EXNAME = 'ATTRIBUTE'                                          1340
         AG_PARLIST = 'SEEN_COLO'                                         1340
         CALL AXATTRIBUTE                                                 1340
C    *                                                                    1340
C    Geant SHAPE : TUBS ===> RMIN=0.0 RMAX=0.1 PHI1=180.0, PHI2=360.0 DZ  1340
C    =0.5*0.9525 <===                                                     1340
         CALL AGSRESET                                                    1342
         AG_RMIN = 0.0                                                    1342
         AG_RMAX = 0.1                                                    1342
         AG_PHI1 = 180.0                                                  1342
         AG_PHI2 = 360.0                                                  1342
         AG_DZ = 0.5*0.9525                                               1342
         AG_TITLE = 'TUBS'                                                1342
         AG_EXNAME = 'SHAPE'                                              1342
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            1342
         CALL AXSHAPE                                                     1342
C    *                                                                    1342
6931  CALL AGSPOPB                                                        1343
      GO TO BLOCK_IROA, (6930 ,6940)                                      1343
C End of Local Procedure                                                  1343
C ----------------------------------------------------------------------  1344
C ------------ ISRS                                                       1344
C Begin Local Procedure ISRS                                              1345
C ISRS IS THE SUPPORT SMALL BOX SUB-VOLUME TOP SIDE                       1345
6950  CONTINUE                                                            1345
         AG_TITLE='ISRS'                                                  1345
         AG_MODULE=MODULE                                                 1345
         AG_MARK='V00'                                                    1345
         CALL AGSPUSH                                                     1345
         AG_MODULE=MODULE                                                 1345
C    *                                                                    1345
C    Geant MATERIAL : PEEKCF30 ===> inherited arguments used <===         1346
         CALL AGSRESET                                                    1346
         AG_TITLE = 'PEEKCF30'                                            1346
         AG_EXNAME = 'MATERIAL'                                           1346
         AG_PARLIST = 'NONE'                                              1346
         CALL AXMATERIAL                                                  1346
C    *                                                                    1346
C    Geant ATTRIBUTE : ISRS ===> SEEN=1 COLO=3 <===                       1346
         CALL AGSRESET                                                    1347
         AG_SEEN = 1                                                      1347
         AG_COLO = 3                                                      1347
         AG_TITLE = 'ISRS'                                                1347
         AG_EXNAME = 'ATTRIBUTE'                                          1347
         AG_PARLIST = 'SEEN_COLO'                                         1347
         CALL AXATTRIBUTE                                                 1347
C    *                                                                    1347
C    Geant SHAPE : BOX ===> DX=0.5*0.3116 DY=0.5*0.1 DZ=0.5*0.9525 <===   1347
         CALL AGSRESET                                                    1348
         AG_DX = 0.5*0.3116                                               1348
         AG_DY = 0.5*0.1                                                  1348
         AG_DZ = 0.5*0.9525                                               1348
         AG_TITLE = 'BOX'                                                 1348
         AG_EXNAME = 'SHAPE'                                              1348
         AG_PARLIST = 'DX_DY_DZ'                                          1348
         CALL AXSHAPE                                                     1348
C    *                                                                    1348
6951  CALL AGSPOPB                                                        1349
      GO TO BLOCK_ISRS, (6950 ,6960)                                      1349
C End of Local Procedure                                                  1349
C ----------------------------------------------------------------------  1350
C ------------ ISRH                                                       1350
C Begin Local Procedure ISRH                                              1351
C ISRH IS THE AIR HOLE FOR SCREWING STAVE                                 1351
6970  CONTINUE                                                            1351
         AG_TITLE='ISRH'                                                  1351
         AG_MODULE=MODULE                                                 1351
         AG_MARK='V00'                                                    1351
         CALL AGSPUSH                                                     1351
         AG_MODULE=MODULE                                                 1351
C    *                                                                    1351
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1352
         CALL AGSRESET                                                    1352
         AG_TITLE = 'AIR'                                                 1352
         AG_EXNAME = 'MATERIAL'                                           1352
         AG_PARLIST = 'NONE'                                              1352
         CALL AXMATERIAL                                                  1352
C    *                                                                    1352
C    Geant ATTRIBUTE : ISRH ===> SEEN=0 COLO=1 <===                       1352
         CALL AGSRESET                                                    1353
         AG_SEEN = 0                                                      1353
         AG_COLO = 1                                                      1353
         AG_TITLE = 'ISRH'                                                1353
         AG_EXNAME = 'ATTRIBUTE'                                          1353
         AG_PARLIST = 'SEEN_COLO'                                         1353
         CALL AXATTRIBUTE                                                 1353
C    *                                                                    1353
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=0.5*0.3454 DZ=0.5*1.2438 <===  1353
         CALL AGSRESET                                                    1354
         AG_RMIN = 0.0                                                    1354
         AG_RMAX = 0.5*0.3454                                             1354
         AG_DZ = 0.5*1.2438                                               1354
         AG_TITLE = 'TUBE'                                                1354
         AG_EXNAME = 'SHAPE'                                              1354
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1354
         CALL AXSHAPE                                                     1354
C    *                                                                    1354
6971  CALL AGSPOPB                                                        1355
      GO TO BLOCK_ISRH, (6970 ,6980)                                      1355
C End of Local Procedure                                                  1355
C ----------------------------------------------------------------------  1356
C ------------ IBLM                                                       1356
C Begin Local Procedure IBLM                                              1357
C IBLM MOTHER OF THE SENSOR ASSEMBLY                                      1357
5890  CONTINUE                                                            1357
         AG_TITLE='IBLM'                                                  1357
         AG_MODULE=MODULE                                                 1357
         AG_MARK='V00'                                                    1357
         CALL AGSPUSH                                                     1357
         AG_MODULE=MODULE                                                 1357
C    *                                                                    1357
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1358
         CALL AGSRESET                                                    1358
         AG_TITLE = 'AIR'                                                 1358
         AG_EXNAME = 'MATERIAL'                                           1358
         AG_PARLIST = 'NONE'                                              1358
         CALL AXMATERIAL                                                  1358
C    *                                                                    1358
C    Geant ATTRIBUTE : IBLM ===> SEEN=0 COLO=1 <===                       1358
         CALL AGSRESET                                                    1359
         AG_SEEN = 0                                                      1359
         AG_COLO = 1                                                      1359
         AG_TITLE = 'IBLM'                                                1359
         AG_EXNAME = 'ATTRIBUTE'                                          1359
         AG_PARLIST = 'SEEN_COLO'                                         1359
         CALL AXATTRIBUTE                                                 1359
C    *                                                                    1359
C    Geant SHAPE : BOX ===> DX=HALFSENMOMWDT DY=HALFSENMOMTHK, DZ=HALFSE  1359
C    NMOMLEN <===                                                         1359
         CALL AGSRESET                                                    1361
         AG_DX = HALFSENMOMWDT                                            1361
         AG_DY = HALFSENMOMTHK                                            1361
         AG_DZ = HALFSENMOMLEN                                            1361
         AG_TITLE = 'BOX'                                                 1361
         AG_EXNAME = 'SHAPE'                                              1361
         AG_PARLIST = 'DX_DY_DZ'                                          1361
         CALL AXSHAPE                                                     1361
C    *                                                                    1361
C    Create IBSS :                                                        1362
C    Call To Local Procedure IBSS                                         1362
         ASSIGN 7000 TO BLOCK_IBSS                                        1362
         GO TO  6990                                                      1362
7000     CONTINUE                                                         1362
C    *                                                                    1362
C    Geant POSITION : IBSS ===> X=HALFSENMOMWDT-HALFSENSORWDT-2*HALFSENS  1364
C    OREDGET Y=0.0 Z=0.0 ALPHAZ=180 <===                                  1364
         CALL AGSRESET                                                    1364
         AG_X = HALFSENMOMWDT-HALFSENSORWDT-2*HALFSENSOREDGET             1364
         AG_Y = 0.0                                                       1364
         AG_Z = 0.0                                                       1364
         AG_ALPHAN=AG_ALPHAN+1                                            1364
         AG_ALPHAK(AG_ALPHAN)=3                                           1364
         AG_ALPHAL(AG_ALPHAN)=180                                         1364
         AG_TITLE = 'IBSS'                                                1364
         AG_EXNAME = 'POSITION'                                           1364
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      1364
         CALL AXPOSITION                                                  1364
C    *                                                                    1364
C    Create IBTE :                                                        1365
C    Call To Local Procedure IBTE                                         1365
         ASSIGN 7020 TO BLOCK_IBTE                                        1365
         GO TO  7010                                                      1365
7020     CONTINUE                                                         1365
C    *                                                                    1365
C    Geant POSITION : IBTE ===> X=HALFSENMOMWDT-HALFSENSOREDGET Y=0.0 Z=  1366
C    0.0 <===                                                             1366
         CALL AGSRESET                                                    1366
         AG_X = HALFSENMOMWDT-HALFSENSOREDGET                             1366
         AG_Y = 0.0                                                       1366
         AG_Z = 0.0                                                       1366
         AG_TITLE = 'IBTE'                                                1366
         AG_EXNAME = 'POSITION'                                           1366
         AG_PARLIST = 'X_Y_Z'                                             1366
         CALL AXPOSITION                                                  1366
C    *                                                                    1366
C    Create IBBE :                                                        1367
C    Call To Local Procedure IBBE                                         1367
         ASSIGN 7040 TO BLOCK_IBBE                                        1367
         GO TO  7030                                                      1367
7040     CONTINUE                                                         1367
C    *                                                                    1367
C    Geant POSITION : IBBE ===> X=2*HALFAPVWDT+APVSENSORGAPSIZE+HALFSENS  1369
C    OREDGEB-HALFSENMOMWDT Y=0.0 Z=0.0 <===                               1369
         CALL AGSRESET                                                    1369
         AG_X =                                                           1369
     *   2*HALFAPVWDT+APVSENSORGAPSIZE+HALFSENSOREDGEB-HALFSENMOMWDT      1369
         AG_Y = 0.0                                                       1369
         AG_Z = 0.0                                                       1369
         AG_TITLE = 'IBBE'                                                1369
         AG_EXNAME = 'POSITION'                                           1369
         AG_PARLIST = 'X_Y_Z'                                             1369
         CALL AXPOSITION                                                  1369
C    *                                                                    1369
C    Create IBLE :                                                        1370
C    Call To Local Procedure IBLE                                         1370
         ASSIGN 7060 TO BLOCK_IBLE                                        1370
         GO TO  7050                                                      1370
7060     CONTINUE                                                         1370
C    *                                                                    1370
C    Geant POSITION : IBLE ===> X=HALFSENMOMWDT-2.0 Y=0.0 Z=HALFSENMOMLE  1372
C    N-HALFSENSOREDGELR-0.5*SENSORGAPSIZE <===                            1372
         CALL AGSRESET                                                    1372
         AG_X = HALFSENMOMWDT-2.0                                         1372
         AG_Y = 0.0                                                       1372
         AG_Z = HALFSENMOMLEN-HALFSENSOREDGELR-0.5*SENSORGAPSIZE          1372
         AG_TITLE = 'IBLE'                                                1372
         AG_EXNAME = 'POSITION'                                           1372
         AG_PARLIST = 'X_Y_Z'                                             1372
         CALL AXPOSITION                                                  1372
C    *                                                                    1372
C    Create IBRE :                                                        1373
C    Call To Local Procedure IBRE                                         1373
         ASSIGN 7080 TO BLOCK_IBRE                                        1373
         GO TO  7070                                                      1373
7080     CONTINUE                                                         1373
C    *                                                                    1373
C    Geant POSITION : IBRE ===> X=HALFSENMOMWDT-2.0 Y=0.0 Z=HALFSENSORED  1375
C    GELR+0.5*SENSORGAPSIZE-HALFSENMOMLEN <===                            1375
         CALL AGSRESET                                                    1375
         AG_X = HALFSENMOMWDT-2.0                                         1375
         AG_Y = 0.0                                                       1375
         AG_Z = HALFSENSOREDGELR+0.5*SENSORGAPSIZE-HALFSENMOMLEN          1375
         AG_TITLE = 'IBRE'                                                1375
         AG_EXNAME = 'POSITION'                                           1375
         AG_PARLIST = 'X_Y_Z'                                             1375
         CALL AXPOSITION                                                  1375
C    *                                                                    1375
C    *                                                                    1375
         NUM=1                                                            1376
            GO TO 7093                                                    1376
7091        NUM=NUM+1                                                     1376
7093        IF(NUM-(NAPVUNIT).GT.0) GO TO 7092                            1376
C       Create IBAP :                                                     1377
C       Call To Local Procedure IBAP                                      1377
            ASSIGN 7110 TO BLOCK_IBAP                                     1377
            GO TO  7100                                                   1377
7110        CONTINUE                                                      1377
C       *                                                                 1377
C       Geant POSITION : IBAP ===> MOTHER='IBLM' X=-HALFSENMOMWDT+HALFAP  1379
C       VWDT Y=0.0 Z=-HALFSENMOMLEN+HALFAPVLEN*(2*NUM-1)+APVGAPSIZE*(NUM  1379
C       -1)+0.36875 <===                                                  1379
            CALL AGSRESET                                                 1379
            AG_MOTHER = 'IBLM'                                            1379
            AG_X = -HALFSENMOMWDT+HALFAPVWDT                              1379
            AG_Y = 0.0                                                    1379
            AG_Z =                                                        1379
     *      -HALFSENMOMLEN+HALFAPVLEN*(2*NUM-1)+APVGAPSIZE*(NUM-1)+0.     1379
     *      36875                                                         1379
            AG_TITLE = 'IBAP'                                             1379
            AG_EXNAME = 'POSITION'                                        1379
            AG_PARLIST = 'MOTHER_X_Y_Z'                                   1379
            CALL AXPOSITION                                               1379
C       *                                                                 1379
         GO TO 7091                                                       1380
7092     CONTINUE                                                         1380
5891  CALL AGSPOPB                                                        1381
      GO TO BLOCK_IBLM, (5890 ,5900)                                      1381
C End of Local Procedure                                                  1381
C ----------------------------------------------------------------------  1382
C ------------ IBTE                                                       1382
C Begin Local Procedure IBTE                                              1383
C IBTE IS THE SILICON SENSOR TOP DEAD AREA                                1383
7010  CONTINUE                                                            1383
         AG_TITLE='IBTE'                                                  1383
         AG_MODULE=MODULE                                                 1383
         AG_MARK='V00'                                                    1383
         CALL AGSPUSH                                                     1383
         AG_MODULE=MODULE                                                 1383
C    *                                                                    1383
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1384
         CALL AGSRESET                                                    1384
         AG_TITLE = 'SILICON'                                             1384
         AG_EXNAME = 'MATERIAL'                                           1384
         AG_PARLIST = 'NONE'                                              1384
         CALL AXMATERIAL                                                  1384
C    *                                                                    1384
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    1384
         CALL AGSRESET                                                    1385
         AG_ISVOL = 0                                                     1385
         AG_TITLE = 'PASSIVESILICON'                                      1385
         AG_EXNAME = 'MATERIAL'                                           1385
         AG_PARLIST = 'ISVOL'                                             1385
         CALL AXMATERIAL                                                  1385
C    *                                                                    1385
C    Geant ATTRIBUTE : IBTE ===> SEEN=1 COLO=2 <===                       1385
         CALL AGSRESET                                                    1386
         AG_SEEN = 1                                                      1386
         AG_COLO = 2                                                      1386
         AG_TITLE = 'IBTE'                                                1386
         AG_EXNAME = 'ATTRIBUTE'                                          1386
         AG_PARLIST = 'SEEN_COLO'                                         1386
         CALL AXATTRIBUTE                                                 1386
C    *                                                                    1386
C    Geant SHAPE : BOX ===> DX=HALFSENSOREDGET DY=HALFSENSORTHK, DZ=HALF  1386
C    SENSORLEN <===                                                       1386
         CALL AGSRESET                                                    1388
         AG_DX = HALFSENSOREDGET                                          1388
         AG_DY = HALFSENSORTHK                                            1388
         AG_DZ = HALFSENSORLEN                                            1388
         AG_TITLE = 'BOX'                                                 1388
         AG_EXNAME = 'SHAPE'                                              1388
         AG_PARLIST = 'DX_DY_DZ'                                          1388
         CALL AXSHAPE                                                     1388
C    *                                                                    1388
7011  CALL AGSPOPB                                                        1389
      GO TO BLOCK_IBTE, (7010 ,7020)                                      1389
C End of Local Procedure                                                  1389
C ----------------------------------------------------------------------  1390
C ------------ IBBE                                                       1390
C Begin Local Procedure IBBE                                              1391
C IBBE IS THE SILICON SENSOR BOTTOM DEAD AREA                             1391
7030  CONTINUE                                                            1391
         AG_TITLE='IBBE'                                                  1391
         AG_MODULE=MODULE                                                 1391
         AG_MARK='V00'                                                    1391
         CALL AGSPUSH                                                     1391
         AG_MODULE=MODULE                                                 1391
C    *                                                                    1391
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1392
         CALL AGSRESET                                                    1392
         AG_TITLE = 'SILICON'                                             1392
         AG_EXNAME = 'MATERIAL'                                           1392
         AG_PARLIST = 'NONE'                                              1392
         CALL AXMATERIAL                                                  1392
C    *                                                                    1392
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    1392
         CALL AGSRESET                                                    1393
         AG_ISVOL = 0                                                     1393
         AG_TITLE = 'PASSIVESILICON'                                      1393
         AG_EXNAME = 'MATERIAL'                                           1393
         AG_PARLIST = 'ISVOL'                                             1393
         CALL AXMATERIAL                                                  1393
C    *                                                                    1393
C    Geant ATTRIBUTE : IBBE ===> SEEN=1 COLO=2 <===                       1393
         CALL AGSRESET                                                    1394
         AG_SEEN = 1                                                      1394
         AG_COLO = 2                                                      1394
         AG_TITLE = 'IBBE'                                                1394
         AG_EXNAME = 'ATTRIBUTE'                                          1394
         AG_PARLIST = 'SEEN_COLO'                                         1394
         CALL AXATTRIBUTE                                                 1394
C    *                                                                    1394
C    Geant SHAPE : BOX ===> DX=HALFSENSOREDGEB DY=HALFSENSORTHK, DZ=HALF  1394
C    SENSORLEN <===                                                       1394
         CALL AGSRESET                                                    1396
         AG_DX = HALFSENSOREDGEB                                          1396
         AG_DY = HALFSENSORTHK                                            1396
         AG_DZ = HALFSENSORLEN                                            1396
         AG_TITLE = 'BOX'                                                 1396
         AG_EXNAME = 'SHAPE'                                              1396
         AG_PARLIST = 'DX_DY_DZ'                                          1396
         CALL AXSHAPE                                                     1396
C    *                                                                    1396
7031  CALL AGSPOPB                                                        1397
      GO TO BLOCK_IBBE, (7030 ,7040)                                      1397
C End of Local Procedure                                                  1397
C ----------------------------------------------------------------------  1398
C ------------ IBLE                                                       1398
C Begin Local Procedure IBLE                                              1399
C IBLE IS THE SILICON SENSOR LEFT DEAD AREA                               1399
7050  CONTINUE                                                            1399
         AG_TITLE='IBLE'                                                  1399
         AG_MODULE=MODULE                                                 1399
         AG_MARK='V00'                                                    1399
         CALL AGSPUSH                                                     1399
         AG_MODULE=MODULE                                                 1399
C    *                                                                    1399
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1400
         CALL AGSRESET                                                    1400
         AG_TITLE = 'SILICON'                                             1400
         AG_EXNAME = 'MATERIAL'                                           1400
         AG_PARLIST = 'NONE'                                              1400
         CALL AXMATERIAL                                                  1400
C    *                                                                    1400
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    1400
         CALL AGSRESET                                                    1401
         AG_ISVOL = 0                                                     1401
         AG_TITLE = 'PASSIVESILICON'                                      1401
         AG_EXNAME = 'MATERIAL'                                           1401
         AG_PARLIST = 'ISVOL'                                             1401
         CALL AXMATERIAL                                                  1401
C    *                                                                    1401
C    Geant ATTRIBUTE : IBLE ===> SEEN=1 COLO=2 <===                       1401
         CALL AGSRESET                                                    1402
         AG_SEEN = 1                                                      1402
         AG_COLO = 2                                                      1402
         AG_TITLE = 'IBLE'                                                1402
         AG_EXNAME = 'ATTRIBUTE'                                          1402
         AG_PARLIST = 'SEEN_COLO'                                         1402
         CALL AXATTRIBUTE                                                 1402
C    *                                                                    1402
C    Geant SHAPE : BOX ===> DX=2.0 DY=HALFSENSORTHK DZ=HALFSENSOREDGELR   1402
C    <===                                                                 1402
         CALL AGSRESET                                                    1403
         AG_DX = 2.0                                                      1403
         AG_DY = HALFSENSORTHK                                            1403
         AG_DZ = HALFSENSOREDGELR                                         1403
         AG_TITLE = 'BOX'                                                 1403
         AG_EXNAME = 'SHAPE'                                              1403
         AG_PARLIST = 'DX_DY_DZ'                                          1403
         CALL AXSHAPE                                                     1403
C    *                                                                    1403
7051  CALL AGSPOPB                                                        1404
      GO TO BLOCK_IBLE, (7050 ,7060)                                      1404
C End of Local Procedure                                                  1404
C ----------------------------------------------------------------------  1405
C ------------ IBRE                                                       1405
C Begin Local Procedure IBRE                                              1406
C IBRE IS THE SILICON SENSOR RIGHT DEAD AREA                              1406
7070  CONTINUE                                                            1406
         AG_TITLE='IBRE'                                                  1406
         AG_MODULE=MODULE                                                 1406
         AG_MARK='V00'                                                    1406
         CALL AGSPUSH                                                     1406
         AG_MODULE=MODULE                                                 1406
C    *                                                                    1406
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1407
         CALL AGSRESET                                                    1407
         AG_TITLE = 'SILICON'                                             1407
         AG_EXNAME = 'MATERIAL'                                           1407
         AG_PARLIST = 'NONE'                                              1407
         CALL AXMATERIAL                                                  1407
C    *                                                                    1407
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    1407
         CALL AGSRESET                                                    1408
         AG_ISVOL = 0                                                     1408
         AG_TITLE = 'PASSIVESILICON'                                      1408
         AG_EXNAME = 'MATERIAL'                                           1408
         AG_PARLIST = 'ISVOL'                                             1408
         CALL AXMATERIAL                                                  1408
C    *                                                                    1408
C    Geant ATTRIBUTE : IBRE ===> SEEN=1 COLO=2 <===                       1408
         CALL AGSRESET                                                    1409
         AG_SEEN = 1                                                      1409
         AG_COLO = 2                                                      1409
         AG_TITLE = 'IBRE'                                                1409
         AG_EXNAME = 'ATTRIBUTE'                                          1409
         AG_PARLIST = 'SEEN_COLO'                                         1409
         CALL AXATTRIBUTE                                                 1409
C    *                                                                    1409
C    Geant SHAPE : BOX ===> DX=2.0 DY=HALFSENSORTHK DZ=HALFSENSOREDGELR   1409
C    <===                                                                 1409
         CALL AGSRESET                                                    1410
         AG_DX = 2.0                                                      1410
         AG_DY = HALFSENSORTHK                                            1410
         AG_DZ = HALFSENSOREDGELR                                         1410
         AG_TITLE = 'BOX'                                                 1410
         AG_EXNAME = 'SHAPE'                                              1410
         AG_PARLIST = 'DX_DY_DZ'                                          1410
         CALL AXSHAPE                                                     1410
C    *                                                                    1410
7071  CALL AGSPOPB                                                        1411
      GO TO BLOCK_IBRE, (7070 ,7080)                                      1411
C End of Local Procedure                                                  1411
C ----------------------------------------------------------------------  1412
C ------------ IBAP                                                       1412
C Begin Local Procedure IBAP                                              1413
C IBAP IS THE APV CHIP                                                    1413
7100  CONTINUE                                                            1413
         AG_TITLE='IBAP'                                                  1413
         AG_MODULE=MODULE                                                 1413
         AG_MARK='V00'                                                    1413
         CALL AGSPUSH                                                     1413
         AG_MODULE=MODULE                                                 1413
C    *                                                                    1413
C    Geant MATERIAL : APVMIX ===> inherited arguments used <===           1414
         CALL AGSRESET                                                    1414
         AG_TITLE = 'APVMIX'                                              1414
         AG_EXNAME = 'MATERIAL'                                           1414
         AG_PARLIST = 'NONE'                                              1414
         CALL AXMATERIAL                                                  1414
C    *                                                                    1414
C    Geant ATTRIBUTE : IBAP ===> SEEN=1 COLO=2 <===                       1414
         CALL AGSRESET                                                    1415
         AG_SEEN = 1                                                      1415
         AG_COLO = 2                                                      1415
         AG_TITLE = 'IBAP'                                                1415
         AG_EXNAME = 'ATTRIBUTE'                                          1415
         AG_PARLIST = 'SEEN_COLO'                                         1415
         CALL AXATTRIBUTE                                                 1415
C    *                                                                    1415
C    Geant SHAPE : BOX ===> DX=HALFAPVWDT DY=HALFAPVTHK DZ=HALFAPVLEN <=  1415
C    ==                                                                   1415
         CALL AGSRESET                                                    1416
         AG_DX = HALFAPVWDT                                               1416
         AG_DY = HALFAPVTHK                                               1416
         AG_DZ = HALFAPVLEN                                               1416
         AG_TITLE = 'BOX'                                                 1416
         AG_EXNAME = 'SHAPE'                                              1416
         AG_PARLIST = 'DX_DY_DZ'                                          1416
         CALL AXSHAPE                                                     1416
C    *                                                                    1416
7101  CALL AGSPOPB                                                        1417
      GO TO BLOCK_IBAP, (7100 ,7110)                                      1417
C End of Local Procedure                                                  1417
C ----------------------------------------------------------------------  1418
C ------------ IBSS                                                       1418
C Begin Local Procedure IBSS                                              1419
C IBSS IS THE SILICON SENSOR ACTIVE AREA                                  1419
6990  CONTINUE                                                            1419
         AG_TITLE='IBSS'                                                  1419
         AG_MODULE=MODULE                                                 1419
         AG_MARK='V00'                                                    1419
         CALL AGSPUSH                                                     1419
         AG_MODULE=MODULE                                                 1419
C    *                                                                    1419
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          1420
         CALL AGSRESET                                                    1420
         AG_TITLE = 'SILICON'                                             1420
         AG_EXNAME = 'MATERIAL'                                           1420
         AG_PARLIST = 'NONE'                                              1420
         CALL AXMATERIAL                                                  1420
C    *                                                                    1420
C    Geant MATERIAL : ISTSSENSITIVE ===> ISVOL=1 <===                     1420
         CALL AGSRESET                                                    1421
         AG_ISVOL = 1                                                     1421
         AG_TITLE = 'ISTSSENSITIVE'                                       1421
         AG_EXNAME = 'MATERIAL'                                           1421
         AG_PARLIST = 'ISVOL'                                             1421
         CALL AXMATERIAL                                                  1421
C    *                                                                    1421
C    Geant ATTRIBUTE : IBSS ===> SEEN=1 COLO=2 <===                       1421
         CALL AGSRESET                                                    1422
         AG_SEEN = 1                                                      1422
         AG_COLO = 2                                                      1422
         AG_TITLE = 'IBSS'                                                1422
         AG_EXNAME = 'ATTRIBUTE'                                          1422
         AG_PARLIST = 'SEEN_COLO'                                         1422
         CALL AXATTRIBUTE                                                 1422
C    *                                                                    1422
C    Geant SHAPE : BOX ===> DX=HALFSENSORWDT DY=HALFSENSORTHK, DZ=HALFSE  1422
C    NSORLEN <===                                                         1422
         CALL AGSRESET                                                    1424
         AG_DX = HALFSENSORWDT                                            1424
         AG_DY = HALFSENSORTHK                                            1424
         AG_DZ = HALFSENSORLEN                                            1424
         AG_TITLE = 'BOX'                                                 1424
         AG_EXNAME = 'SHAPE'                                              1424
         AG_PARLIST = 'DX_DY_DZ'                                          1424
         CALL AXSHAPE                                                     1424
C    *                                                                    1424
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100) CX:10: CY:1  1424
C    0: CZ:10: SLENG:16:(0,500), TOF:16: STEP:.01: ELOSS:16:              1424
         AG_TITLE = 'IBSS'                                                1426
         AG_EXNAME = 'HITS'                                               1426
         AG_TYPE=AG_MAGIC                                                 1426
         CALL AGSDETV('IBSS')                                             1426
C    *                                                                    1426
         AG_PARLIST='Z'                                                   1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = AG_MAGIC                                              1426
         AG_BIN=.001                                                      1426
         AG_OPTION='S'                                                    1426
         AG_FHMIN = AG_MAGIC                                              1426
         AG_FHMAX = AG_MAGIC                                              1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='Y'                                                   1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = AG_MAGIC                                              1426
         AG_BIN=.001                                                      1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='X'                                                   1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = AG_MAGIC                                              1426
         AG_BIN=.001                                                      1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='PTOT'                                                1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = 16                                                    1426
         AG_BIN=AG_MAGIC                                                  1426
         AG_OPTION='*'                                                    1426
         AG_FHMIN = 0                                                     1426
         AG_FHMAX = 100                                                   1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='CX'                                                  1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = 10                                                    1426
         AG_BIN=AG_MAGIC                                                  1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='CY'                                                  1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = 10                                                    1426
         AG_BIN=AG_MAGIC                                                  1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='CZ'                                                  1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = 10                                                    1426
         AG_BIN=AG_MAGIC                                                  1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='SLENG'                                               1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = 16                                                    1426
         AG_BIN=AG_MAGIC                                                  1426
         AG_OPTION='*'                                                    1426
         AG_FHMIN = 0                                                     1426
         AG_FHMAX = 500                                                   1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='TOF'                                                 1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = 16                                                    1426
         AG_BIN=AG_MAGIC                                                  1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='STEP'                                                1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = AG_MAGIC                                              1426
         AG_BIN=.01                                                       1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         AG_PARLIST='ELOSS'                                               1426
         AG_TITLE = 'DETH'                                                1426
         AG_NBITS = 16                                                    1426
         AG_BIN=AG_MAGIC                                                  1426
         AG_FHMIN=AG_MAGIC                                                1426
         AG_FHMAX=AG_MAGIC                                                1426
         AG_OPTION='*'                                                    1426
         CALL AGSDETH (0)                                                 1426
C    *                                                                    1426
         CALL AGSDETU('IBSS')                                             1426
C    *                                                                    1426
6991  CALL AGSPOPB                                                        1427
      GO TO BLOCK_IBSS, (6990 ,7000)                                      1427
C End of Local Procedure                                                  1427
C ----------------------------------------------------------------------  1428
C ------------ ICCT                                                       1428
C Begin Local Procedure ICCT                                              1429
C ICCT AVERAGE VOLUME OF AVERAGE CABLES/COOLING LINE IN TRANSITION AREA   1429
5010  CONTINUE                                                            1429
         AG_TITLE='ICCT'                                                  1429
         AG_MODULE=MODULE                                                 1429
         AG_MARK='V00'                                                    1429
         CALL AGSPUSH                                                     1429
         AG_MODULE=MODULE                                                 1429
C    *                                                                    1429
C    Geant MATERIAL : CUALNOVECMIX ===> inherited arguments used <===     1430
         CALL AGSRESET                                                    1430
         AG_TITLE = 'CUALNOVECMIX'                                        1430
         AG_EXNAME = 'MATERIAL'                                           1430
         AG_PARLIST = 'NONE'                                              1430
         CALL AXMATERIAL                                                  1430
C    *                                                                    1430
C    Geant ATTRIBUTE : ICCT ===> SEEN=1 COLO=629 <===                     1430
         CALL AGSRESET                                                    1431
         AG_SEEN = 1                                                      1431
         AG_COLO = 629                                                    1431
         AG_TITLE = 'ICCT'                                                1431
         AG_EXNAME = 'ATTRIBUTE'                                          1431
         AG_PARLIST = 'SEEN_COLO'                                         1431
         CALL AXATTRIBUTE                                                 1431
C    *                                                                    1431
C    Geant SHAPE : CONE ===> DZ=0.5*8.7144 RMN1=24.068 RMX1=24.8745, RMN  1431
C    2=14.7727 RMX2=17.4338 <===                                          1431
         CALL AGSRESET                                                    1433
         AG_DZ = 0.5*8.7144                                               1433
         AG_RMN1 = 24.068                                                 1433
         AG_RMX1 = 24.8745                                                1433
         AG_RMN2 = 14.7727                                                1433
         AG_RMX2 = 17.4338                                                1433
         AG_TITLE = 'CONE'                                                1433
         AG_EXNAME = 'SHAPE'                                              1433
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            1433
         CALL AXSHAPE                                                     1433
C    *                                                                    1433
5011  CALL AGSPOPB                                                        1434
      GO TO BLOCK_ICCT, (5010 ,5020)                                      1434
C End of Local Procedure                                                  1434
C ----------------------------------------------------------------------  1435
C ------------ ICCA                                                       1435
C Begin Local Procedure ICCA                                              1436
C ICCA IST PURPLE CABLE A ON PIT                                          1436
5040  CONTINUE                                                            1436
         AG_TITLE='ICCA'                                                  1436
         AG_MODULE=MODULE                                                 1436
         AG_MARK='V00'                                                    1436
         CALL AGSPUSH                                                     1436
         AG_MODULE=MODULE                                                 1436
C    *                                                                    1436
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           1437
         CALL AGSRESET                                                    1437
         AG_TITLE = 'COPPER'                                              1437
         AG_EXNAME = 'MATERIAL'                                           1437
         AG_PARLIST = 'NONE'                                              1437
         CALL AXMATERIAL                                                  1437
C    *                                                                    1437
C    Geant ATTRIBUTE : ICCA ===> SEEN=1 COLO=629 <===                     1437
         CALL AGSRESET                                                    1438
         AG_SEEN = 1                                                      1438
         AG_COLO = 629                                                    1438
         AG_TITLE = 'ICCA'                                                1438
         AG_EXNAME = 'ATTRIBUTE'                                          1438
         AG_PARLIST = 'SEEN_COLO'                                         1438
         CALL AXATTRIBUTE                                                 1438
C    *                                                                    1438
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*CABLEDIAMETER, DZ=0.5*170.1  1438
C    8 <===                                                               1438
         CALL AGSRESET                                                    1440
         AG_RMIN = 0                                                      1440
         AG_RMAX = 0.5*CABLEDIAMETER                                      1440
         AG_DZ = 0.5*170.18                                               1440
         AG_TITLE = 'TUBE'                                                1440
         AG_EXNAME = 'SHAPE'                                              1440
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1440
         CALL AXSHAPE                                                     1440
C    *                                                                    1440
5041  CALL AGSPOPB                                                        1441
      GO TO BLOCK_ICCA, (5040 ,5050)                                      1441
C End of Local Procedure                                                  1441
C ----------------------------------------------------------------------  1442
C ------------ ICCL                                                       1442
C Begin Local Procedure ICCL                                              1443
C ICCL IST COOLING LINE ON PIT                                            1443
5060  CONTINUE                                                            1443
         AG_TITLE='ICCL'                                                  1443
         AG_MODULE=MODULE                                                 1443
         AG_MARK='V00'                                                    1443
         CALL AGSPUSH                                                     1443
         AG_MODULE=MODULE                                                 1443
C    *                                                                    1443
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1444
         CALL AGSRESET                                                    1444
         AG_TITLE = 'ALUMINIUM'                                           1444
         AG_EXNAME = 'MATERIAL'                                           1444
         AG_PARLIST = 'NONE'                                              1444
         CALL AXMATERIAL                                                  1444
C    *                                                                    1444
C    Geant ATTRIBUTE : ICCL ===> SEEN=1 COLO=4 <===                       1444
         CALL AGSRESET                                                    1445
         AG_SEEN = 1                                                      1445
         AG_COLO = 4                                                      1445
         AG_TITLE = 'ICCL'                                                1445
         AG_EXNAME = 'ATTRIBUTE'                                          1445
         AG_PARLIST = 'SEEN_COLO'                                         1445
         CALL AXATTRIBUTE                                                 1445
C    *                                                                    1445
C    Geant SHAPE : TUBE ===> RMIN=0.5*0.5334 RMAX=0.5*0.635, DZ=0.5*170.  1445
C    18 <===                                                              1445
         CALL AGSRESET                                                    1447
         AG_RMIN = 0.5*0.5334                                             1447
         AG_RMAX = 0.5*0.635                                              1447
         AG_DZ = 0.5*170.18                                               1447
         AG_TITLE = 'TUBE'                                                1447
         AG_EXNAME = 'SHAPE'                                              1447
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1447
         CALL AXSHAPE                                                     1447
C    *                                                                    1447
5061  CALL AGSPOPB                                                        1448
      GO TO BLOCK_ICCL, (5060 ,5070)                                      1448
C End of Local Procedure                                                  1448
C ----------------------------------------------------------------------  1449
C ------------ ICLP                                                       1449
C Begin Local Procedure ICLP                                              1450
C ICLP COOLING LIQUID IN ICCL VOLUME ON PIT AREA                          1450
5080  CONTINUE                                                            1450
         AG_TITLE='ICLP'                                                  1450
         AG_MODULE=MODULE                                                 1450
         AG_MARK='V00'                                                    1450
         CALL AGSPUSH                                                     1450
         AG_MODULE=MODULE                                                 1450
C    *                                                                    1450
C    Geant ATTRIBUTE : ICLP ===> SEEN=1 COLO=920 <===                     1451
         CALL AGSRESET                                                    1451
         AG_SEEN = 1                                                      1451
         AG_COLO = 920                                                    1451
         AG_TITLE = 'ICLP'                                                1451
         AG_EXNAME = 'ATTRIBUTE'                                          1451
         AG_PARLIST = 'SEEN_COLO'                                         1451
         CALL AXATTRIBUTE                                                 1451
C    *                                                                    1451
C    Geant MATERIAL : NOVEC7200 ===> inherited arguments used <===        1451
         CALL AGSRESET                                                    1452
         AG_TITLE = 'NOVEC7200'                                           1452
         AG_EXNAME = 'MATERIAL'                                           1452
         AG_PARLIST = 'NONE'                                              1452
         CALL AXMATERIAL                                                  1452
C    *                                                                    1452
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=0.5*0.5334 DZ=0.5*170.18 <===    1452
         CALL AGSRESET                                                    1453
         AG_RMIN = 0                                                      1453
         AG_RMAX = 0.5*0.5334                                             1453
         AG_DZ = 0.5*170.18                                               1453
         AG_TITLE = 'TUBE'                                                1453
         AG_EXNAME = 'SHAPE'                                              1453
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1453
         CALL AXSHAPE                                                     1453
C    *                                                                    1453
5081  CALL AGSPOPB                                                        1454
      GO TO BLOCK_ICLP, (5080 ,5090)                                      1454
C End of Local Procedure                                                  1454
      WRITE(LOUT,7120)'===>>  [End AgML Module IstdGeo1]  <<==='          1454
7120  FORMAT(A32,/,/)                                                     1454
C MODULE ISTDGEO1                                                         1456
      END                                                                 1456
C ----------------------------------------------------------------------  1457
C ------------ TRANS_TO_IBAM                                              1457
      SUBROUTINE TRANS_TO_IBAM(X, Y,TILT, XPOS, YPOS)                     1458
C INTENT=IN                                                               1459
      REAL X                                                              1459
C INTENT=IN                                                               1460
      REAL Y                                                              1460
C INTENT=IN                                                               1461
      REAL TILT                                                           1461
C INTENT=INOUT                                                            1462
      REAL XPOS                                                           1462
C INTENT=INOUT                                                            1463
      REAL YPOS                                                           1463
      REAL *8 SINT, COST, DEGRAD, RADIUS/13.9254/                         1464
      DEGRAD=3.1415926/180.0                                              1466
      SINT=SIN(DEGRAD*TILT)                                               1467
      COST=COS(DEGRAD*TILT)                                               1468
      XPOS = 0.0 + COST*X +SINT*Y                                         1469
      YPOS = RADIUS - SINT*X +COST*Y                                      1470
      RETURN                                                              1471
C SUBROUTINE TRANS_TO_IBAM                                                1472
      END                                                                 1472
