*-----------------------------------------------------------------------------
* <Rating>187</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE MXMB.LINE.MVMT(LINE.MVMT.ID)
*************************************************************************
* This program extracts the REPORT.NAME, REPORT.LINE his BALANCE LINE
* The key comprises of the following elements :-
* 1. Report Name * 2. '-' * 3. Line Number * 4. '-' * 5. Period End Date
* 6. '-'
* 7. Any of "A" - Account (STMT.ENTRY)
*           "P" - P&L (CATEG.ENTRY)
*        or "R" - Spec (RE.CONSOL.SPEC.ENTRY)
* 8. '-'
* 9. Sequence Number beginning with 1 E.g. GLSTD-1410-GBP-19970331-A-1
*------------------------------------------------------------------------------------------------------------------
*Modification Details:
*=====================
*28/07/2014
*				TAFJ optimization
*************************************************************************************************
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.STMT.ENTRY
    $INSERT I_F.CATEG.ENTRY
    $INSERT I_F.RE.CONSOL.SPEC.ENTRY
    $INSERT I_F.RE.STAT.LINE.MVMT
    $INSERT I_F.RE.STAT.REP.LINE
    $INSERT I_F.LD.LOANS.AND.DEPOSITS
    $INSERT I_BATCH.FILES
    $INSERT I_F.DATES
*   $INSERT MXMB.BP I_MXMB.LINE.MVMT.COMMON 		;* TAJF opt
*   $INSERT MXMB.BP I_F.MXMB.JOURNAL.LINE           ;* TAJF opt
*   $INSERT MXMB.BP I_F.MXMB.GL.INTERFACE.PARAMETER ;* TAJF opt
*   $INSERT MXMB.BP I_F.MXMB.ACCT.INTERFACE.PARAM   ;* TAJF opt
    $INSERT I_MXMB.LINE.MVMT.COMMON
    $INSERT I_F.MXMB.JOURNAL.LINE
    $INSERT I_F.MXMB.GL.INTERFACE.PARAMETER
    $INSERT I_F.MXMB.ACCT.INTERFACE.PARAM
*
*************************************************
*

    GOSUB INITIALIZE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
*
*    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
*    END
*
    RETURN
*
************************************************
*-------
PROCESS:
*-------
*
*   Build of ID of RE.STAT.REP.LINE File
* Boo - Debug Purpose
    RAD.LOG.ID = "CG01.JOURNAL"
    INFO.LOG = "~":LINE.MVMT.ID

    Y.REPORT.NAME = FIELD(LINE.MVMT.ID,"-",1)
    Y.LINE.NUMBER = FIELD(LINE.MVMT.ID,"-",2)
	CALL SLV.UPDATE.LOG(RAD.LOG.ID,'Y.REPORT.NAME: ':Y.REPORT.NAME:'Y.LINE.NUMBER: ':Y.LINE.NUMBER)
	CALL SLV.UPDATE.LOG(RAD.LOG.ID,'LINE.MVMT.ID: ':LINE.MVMT.ID)
	
*SAP20070704-S
    Y.CURRENCY = FIELD(LINE.MVMT.ID,"-",3)
*SAP20070704-E
*
    Y.LINE.NO.CNT = DCOUNT(Y.PL.LINE,VM)
    Y.LINE.FLAG = ''

*  FOR Y.CNT = 1 TO Y.LINE.NO.CNT
    Y.CNT = '1'
    LOOP
    WHILE Y.CNT LE Y.LINE.NO.CNT
        * TAM_START
        * PACS00119556 -s

        *        IF (Y.REPORT.NAME EQ Y.ASSET.LIAB.HEAD) AND (Y.LINE.NUMBER = FIELD(Y.PL.LINE<1,Y.CNT>,".",2)) THEN
        LOCATE Y.REPORT.NAME IN Y.ASSET.LIAB.HEAD<1,1> SETTING POS THEN
        IF Y.LINE.NUMBER = FIELD(Y.PL.LINE<1,Y.CNT>,".",2) THEN
            RETURN
        END
    END
* PACS00119556 -e
* TAM_END
*  NEXT Y.CNT
    Y.CNT++
    REPEAT


    Y.LINE.ID = Y.REPORT.NAME:".":Y.LINE.NUMBER
* TAM_START
    R.LINE.REC = ''
    CALL F.READ(FN.RE.STAT.REP.LINE,Y.LINE.ID,R.LINE.REC,F.RE.STAT.REP.LINE,STAT.REP.ERR)
    IF R.LINE.REC THEN
        * TAM_END
        GOSUB PROCESS.REP.LINE
    END ELSE
        INFO.LOG<-1> = " RECORD Y.LINE.ID : ":Y.LINE.ID: " DOESN'T EXIST IN ":FN.RE.STAT.REP.LINE
        PRINT " RECORD Y.LINE.ID : ":Y.LINE.ID: " DOESN'T EXIST IN ":FN.RE.STAT.REP.LINE
    END
* Boo - Debug Purpose
    CALL SLV.UPDATE.LOG(RAD.LOG.ID,INFO.LOG)
*
    RETURN
*
************************************************************
READ.ENT.FILE:

* ONE RECORD WILL EXIST FOR THE RECORD ID WITHOUT ANY SUFFIX
    YCK.KEY = Y.ID.MVMT
    R.REC.ENT.FILE = ''
    CALL F.READ(FN.ENT.FILE,YCK.KEY,R.REC.ENT.FILE,F.ENT.FILE,ENT.FILE.ERR)
    IF R.REC.ENT.FILE THEN
        GOSUB PROCESS.ENT.FILE
    END ELSE
        INFO.LOG<-1> = "No se encontraron registros en ":FN.FILE:" para clave : ":Y.ID.MVMT
        PRINT "No se encontraron registros en ":FN.FILE:" para clave : ":Y.ID.MVMT
        RETURN
    END

    ID.CNTR = '0' ; LOOP.CONTINUE = "TRUE"

    LOOP
        ID.CNTR += 1
    WHILE LOOP.CONTINUE EQ "TRUE"
        YCK.KEY = Y.ID.MVMT : FILE.ID.SUFFIX : ID.CNTR

        R.REC.ENT.FILE = ''
        CALL F.READ(FN.ENT.FILE,YCK.KEY,R.REC.ENT.FILE,F.ENT.FILE,ENT.FILE.ERR)
        IF R.REC.ENT.FILE THEN
            GOSUB PROCESS.ENT.FILE
        END ELSE
            LOOP.CONTINUE = "FALSE"
        END

    REPEAT

    RETURN
****************************************************************************************
PROCESS.ENT.FILE:
*
* THIS IS INSIDE THE CONSOL KEY RECORD

    MVMT.ID = R.REC.ENT.FILE
    NUM.ID = DCOUNT(MVMT.ID,FM)

* WW = 1
* LOOP
* WHILE WW LE NUM.ID
    FOR WW = 1 TO NUM.ID
        Y.ID.ENTRY = R.REC.ENT.FILE<WW>
        * Banorte -s
        Y.FOUND = 0
        Y.FOUND = INDEX(Y.ID.ENTRY,"!",1)

        IF NOT(FIELD(Y.ID.ENTRY,'.',2)) AND Y.FOUND EQ 0 THEN

* The below logic is done since there are some entries are generating similar to 172570351711127.00
* specifically the second part as 00. And This causes to skip that entry updation.

	        Y.DOT.COUNT = 0
	        Y.DOT.COUNT = INDEX(Y.ID.ENTRY,'.',1)
	        IF Y.DOT.COUNT ELSE
*				INFO.LOG<-1> = "CONTINUED : ":Y.ID.ENTRY: " OF ":YCK.KEY: " IN " :LINE.MVMT.ID
            	CONTINUE
            END
            
* i updated the Y.FOUND as 1 to enter into the case defined below.            
            Y.FOUND = 1
        END
        * Banorte -e
        BEGIN CASE
            CASE Y.FILE EQ "P" AND Y.FOUND NE 0
                GOSUB UPDATE.NETTING.ENTRY.RECORD
                GOSUB PROCESS.CATEG.ENTRY.CASE
            CASE Y.FILE EQ "A" AND Y.FOUND NE 0
                GOSUB UPDATE.NETTING.ENTRY.RECORD
                GOSUB PROCESS.STMT.ENTRY.CASE
            CASE Y.FILE EQ "R" AND Y.FOUND NE 0
                GOSUB UPDATE.NETTING.ENTRY.RECORD
                GOSUB PROCESS.SPEC.ENTRY.CASE
            CASE 1
                YMOV.FILE.ID = Y.ID.ENTRY
                GOSUB UPDATE.FILE
        END CASE
        * WW++
        * REPEAT
    NEXT WW

    RETURN
*
*****************************************************************************
*    For CATEG.ENTRY records read CATEG.ENTRY.DETAIL.XREF table and get
*    CATEG.ENTRY ID
*****************************************************************************
*
PROCESS.CATEG.ENTRY.CASE:
*
    CE.XREF.ID.PREFIX = '-'
    CE.XREF.KEY = "TRUE"
    CE.ID.CNTR = 0
    LOOP
        CE.ID.CNTR+=1
    WHILE CE.XREF.KEY EQ 'TRUE'
        R.CATEG.ENTRY.DETAIL.XREF = ''
        * Banorte -s
        Y.ID.ENTRY.CURR = Y.ID.ENTRY:CE.XREF.ID.PREFIX:CE.ID.CNTR
        CALL F.READ(FN.CATEG.ENTRY.DETAIL.XREF,Y.ID.ENTRY.CURR,R.CATEG.ENTRY.DETAIL.XREF,F.CATEG.ENTRY.DETAIL.XREF,CATG.DET.ERR)
        * Banorte -e
        IF R.CATEG.ENTRY.DETAIL.XREF THEN
            CE.MVMT.ID = R.CATEG.ENTRY.DETAIL.XREF
            CE.NUM.ID  = DCOUNT(CE.MVMT.ID,FM)
            DETAIL.FILE = 'CATEG'
            YWW = 1
            LOOP
            WHILE YWW LE CE.NUM.ID
                YCE.ID.ENTRY = R.CATEG.ENTRY.DETAIL.XREF<YWW>
                YMOV.FILE.ID = YCE.ID.ENTRY
                GOSUB UPDATE.FILE
                YWW++
            REPEAT
            DETAIL.FILE = ''
        END ELSE
            CE.XREF.KEY = "FALSE"
        END
    REPEAT
*
    RETURN

*********************************************************************************
*
PROCESS.STMT.ENTRY.CASE:
*

    SE.XREF.ID.PREFIX = '-'
    SE.XREF.KEY = "TRUE"
    SE.ID.CNTR = 0
    LOOP
        SE.ID.CNTR+=1
    WHILE SE.XREF.KEY EQ 'TRUE'
        R.STMT.ENTRY.DETAIL.XREF = ''
        * Banorte -s
        Y.ID.ENTRY.CURR = Y.ID.ENTRY:SE.XREF.ID.PREFIX:SE.ID.CNTR
        CALL F.READ(FN.STMT.ENTRY.DETAIL.XREF,Y.ID.ENTRY.CURR,R.STMT.ENTRY.DETAIL.XREF,F.STMT.ENTRY.DETAIL.XREF,STMT.DET.ERR)
        * Banorte -e
        IF R.STMT.ENTRY.DETAIL.XREF THEN
            SE.MVMT.ID = R.STMT.ENTRY.DETAIL.XREF
            SE.NUM.ID  = DCOUNT(SE.MVMT.ID,FM)
            DETAIL.FILE = 'STMT'
            YWW = 1
            LOOP
            WHILE YWW LE SE.NUM.ID
                YSE.ID.ENTRY = R.STMT.ENTRY.DETAIL.XREF<YWW>
                YMOV.FILE.ID = YSE.ID.ENTRY
                GOSUB UPDATE.FILE
                YWW++
            REPEAT
            DETAIL.FILE = ''
        END ELSE
            SE.XREF.KEY = "FALSE"
        END
    REPEAT
*
    RETURN
*
*********************************************************************************
*
PROCESS.SPEC.ENTRY.CASE:
*
    SP.XREF.ID.PREFIX = '-'
    SP.XREF.KEY = "TRUE"
    SP.ID.CNTR = 0
    LOOP
        SP.ID.CNTR+=1
    WHILE SP.XREF.KEY EQ 'TRUE'
        R.SPEC.ENTRY.DETAIL.XREF = ''
        * Banorte -s
        Y.ID.ENTRY.CURR = Y.ID.ENTRY:SP.XREF.ID.PREFIX:SP.ID.CNTR
        CALL F.READ(FN.RE.SPEC.ENTRY.XREF,Y.ID.ENTRY.CURR,R.SPEC.ENTRY.DETAIL.XREF,F.RE.SPEC.ENTRY.XREF,SPEC.DET.ERR)
        * Banorte -e
        IF R.SPEC.ENTRY.DETAIL.XREF THEN
            RE.MVMT.ID = R.SPEC.ENTRY.DETAIL.XREF
            RE.NUM.ID  = DCOUNT(RE.MVMT.ID,FM)
            DETAIL.FILE = 'SPEC'
            YWW = 1
            LOOP
            WHILE YWW LE RE.NUM.ID
                YRE.ID.ENTRY = R.SPEC.ENTRY.DETAIL.XREF<YWW>
                YMOV.FILE.ID = YRE.ID.ENTRY
                GOSUB UPDATE.FILE
                YWW++
            REPEAT
            DETAIL.FILE = ''
        END ELSE
            SP.XREF.KEY = "FALSE"
        END
    REPEAT
*
    RETURN
****************************************************************************************
* Read and Write the record Of STTM.ENTRY or CATEG.ENTRY or RE.CONSOL.SPEC.ENTRY file
* update the local field with the description of RE.STAT.REP.LINE file.
****************************************************************************************
*
UPDATE.FILE:
*

    BEGIN CASE
        CASE DETAIL.FILE = 'CATEG'
            R.MVMT.FILE = ''
            CALL F.READ(FN.CATEG.ENTRY.DETAIL,YMOV.FILE.ID,R.MVMT.FILE,F.CATEG.ENTRY.DETAIL,CATEG.DETAIL.ERR)
        CASE DETAIL.FILE ='STMT'
            R.MVMT.FILE = ''
            CALL F.READ(FN.STMT.ENTRY.DETAIL,YMOV.FILE.ID,R.MVMT.FILE,F.STMT.ENTRY.DETAIL,STMT.DETAIL.ERR)
        CASE DETAIL.FILE = 'SPEC'
            R.MVMT.FILE = ''
            CALL F.READ(FN.RE.SPEC.ENTRY.DETAIL,YMOV.FILE.ID,R.MVMT.FILE,F.RE.SPEC.ENTRY.DETAIL,SPEC.DETAIL.ERR)
        CASE 1
            R.MVMT.FILE = ''
            CALL F.READ(FN.MVMT.FILE,YMOV.FILE.ID,R.MVMT.FILE,F.MVMT.FILE,MVMT.FILE.ERR)
    END CASE
    IF R.MVMT.FILE THEN

        R.MVMT.FILE<YLOCAL.REF, LRF.POS.BLINE> = Y.BALANCE.LINE
		CALL SLV.UPDATE.LOG(RAD.LOG.ID,'Y.BALANCE.LINE: ':Y.BALANCE.LINE)
        Y.AMOUNT.LCY = R.MVMT.FILE<YAMOUNT.LCY>
        YFCY = R.MVMT.FILE<Y.FCY>

        IF YFCY EQ '' THEN
            YFCY = LCCY
        END
        YCATEGORY = R.MVMT.FILE<Y.CATEGORY>
        YPL.CATEGORY = R.MVMT.FILE<Y.PL.CATEGORY>
        Y.TRANSCODE =  R.MVMT.FILE<YTRANS.CODE>

        BEGIN CASE
            CASE YCATEGORY = '' AND YPL.CATEGORY NE ''
                YCATEGORY = YPL.CATEGORY
            CASE YCATEGORY = '' AND YPL.CATEGORY EQ ''
                YCATEGORY = Y.TRANSCODE
        END CASE

        Y.AMOUNT.FCY = R.MVMT.FILE<YAMOUNT.FCY>
        Y.TXN.REF = R.MVMT.FILE<Y.TXN>
        IF Y.AMOUNT.LCY GE 0 THEN
            R.MVMT.FILE<YLOCAL.REF, LRF.POS.SIGN> = 'C'
        END ELSE
            R.MVMT.FILE<YLOCAL.REF, LRF.POS.SIGN> = 'D'
        END
        IF LOG.ACCT.INTERFACE.ONLY.FLAG NE 'TRUE' THEN
            * HD1035755 -s
            * Anand
            * The record R.MVMT.FILE is written to corresponding file(i.e CATEG.ENTRY.DETAIL,STMT.ENTRY.DETAIL,RE.SPEC.ENTRY.DETAIL,CATEG.ENTRY,STMT.ENTRY,SPEC ENTRY)
            BEGIN CASE
                CASE DETAIL.FILE = 'CATEG'
                    WRITE R.MVMT.FILE TO F.CATEG.ENTRY.DETAIL, YMOV.FILE.ID ON ERROR
                        INFO.LOG<-1> = " ERROR WRITING IN":F.CATEG.ENTRY.DETAIL:" ID : ":YMOV.FILE.ID
                        PRINT " ERROR WRITING IN":F.CATEG.ENTRY.DETAIL:" ID : ":YMOV.FILE.ID
                    END

                    PRINT 'LINE ID PROCESSED IS : ':YMOV.FILE.ID
                CASE DETAIL.FILE ='STMT'
                    WRITE R.MVMT.FILE TO F.STMT.ENTRY.DETAIL, YMOV.FILE.ID ON ERROR
                        INFO.LOG<-1> =" ERROR WRITING IN":F.STMT.ENTRY.DETAIL:" ID : ":YMOV.FILE.ID
                        PRINT " ERROR WRITING IN":F.STMT.ENTRY.DETAIL:" ID : ":YMOV.FILE.ID
                    END

                    PRINT 'LINE ID PROCESSED IS : ':YMOV.FILE.ID
                CASE DETAIL.FILE = 'SPEC'
                    WRITE R.MVMT.FILE TO F.RE.SPEC.ENTRY.DETAIL, YMOV.FILE.ID ON ERROR
                        INFO.LOG<-1> =" ERROR WRITING IN":F.RE.SPEC.ENTRY.DETAIL:" ID : ":YMOV.FILE.ID
                        PRINT " ERROR WRITING IN":F.RE.SPEC.ENTRY.DETAIL:" ID : ":YMOV.FILE.ID
                    END

                    PRINT 'LINE ID PROCESSED IS : ':YMOV.FILE.ID
                CASE 1
                    WRITE R.MVMT.FILE TO F.MVMT.FILE, YMOV.FILE.ID ON ERROR
                        INFO.LOG<-1> =" ERROR WRITING IN":F.MVMT.FILE:" ID : ":YMOV.FILE.ID
                        PRINT " ERROR WRITING IN":F.MVMT.FILE:" ID : ":YMOV.FILE.ID
                    END
                    PRINT 'LINE ID PROCESSED IS : ':YMOV.FILE.ID
            END CASE
            * Anand
            * HD1035755 -e
        END
    END ELSE
        INFO.LOG<-1> = "Al actualizar tabla ":FN.FILE:", no se encuentra el registro ":YMOV.FILE.ID
        PRINT "Al actualizar tabla ":FN.FILE:", no se encuentra el registro ":YMOV.FILE.ID

    END

*
    R.CONC.FILE = ''
    TEMP.LINE = Y.BALANCE.LINE
    CHANGE "-" TO "" IN TEMP.LINE       ;*SAP20070705-LENGTH-S-E
    Y.CONC.FILE.ID = Y.CURRENCY:'-':TEMP.LINE[1,Y.SAP.LENGTH]         ;*SAP20070705-LENGTH-S-E
    IF Y.AMOUNT.LCY AND NOT(Y.AMOUNT.FCY) THEN
        Y.CONC.FILE.ID = LCCY:'-':TEMP.LINE[1,Y.SAP.LENGTH] ;*SAP20070705-LENGTH-S-E
    END

    BEGIN CASE
        CASE Y.FILE EQ "A" AND DETAIL.FILE EQ ''
            Y.FILE.IDENT = "SE"

        CASE Y.FILE EQ "P" AND DETAIL.FILE EQ ''
            Y.FILE.IDENT = "CE"

        CASE Y.FILE EQ "R" AND DETAIL.FILE EQ ''
            Y.FILE.IDENT = "RE"

        CASE  DETAIL.FILE EQ 'CATEG'
            Y.FILE.IDENT = "CD"

        CASE  DETAIL.FILE EQ 'STMT'
            Y.FILE.IDENT = "SD"

        CASE  DETAIL.FILE EQ 'SPEC'
            Y.FILE.IDENT = "RD"
    END CASE

    IF UPDATE.ACCT.INTERFACE = 'YES' THEN
        GOSUB UPDATE.ACCT.INTERFACE
    END
    IF LOG.ACCT.INTERFACE.ONLY.FLAG NE "TRUE" THEN
        IF Y.UPDATE.GL.CONC1 EQ 'YES' THEN
            R.CONC.FILE = ''
            CONC1.ERR = ''
            CALL F.READ(FN.MXMB.GL.CONC1,Y.CONC.FILE.ID,R.CONC.FILE,F.MXMB.GL.CONC1,CONC1.ERR)

            * READ R.CONC.FILE FROM F.MXMB.GL.CONC1, Y.CONC.FILE.ID THEN         ;*SAP20070704
            IF R.CONC.FILE THEN
                R.CONC.FILE<-1> = Y.FILE.IDENT:YMOV.FILE.ID
            END ELSE
                R.CONC.FILE = Y.FILE.IDENT:YMOV.FILE.ID     ;*SAP20070824-VW-End
            END
            CALL F.WRITE(FN.MXMB.GL.CONC1,Y.CONC.FILE.ID,R.CONC.FILE)

            *  WRITE R.CONC.FILE TO F.MXMB.GL.CONC1, Y.CONC.FILE.ID ON ERROR      ;*SAP20070704
            *      PRINT "ERROR WRITING IN":F.MXMB.GL.CONC1:" ID : ":TEMP.LINE
            *  END
        END
        *

        Y.LEN.LD.LINE = LEN(Y.LINE.LD)

        IF (Y.UPDATE.GL.CONC3 EQ 'YES') AND (Y.TXN.REF[1,2] EQ 'LD') AND (NUM(Y.TXN.REF[3,12])) THEN
            R.LD = ''
            CALL F.READ(FN.LD.LOANS.AND.DEPOSITS,Y.TXN.REF,R.LD,F.LD.LOANS.AND.DEPOSITS,Y.LD.ERR)
            Y.VALUE.DATE = R.LD<LD.VALUE.DATE>
            Y.CONC.FILE3.ID = ""
            * PACS00200527 -s
            * If the accounting entry falls on the holiday for LD contract, the system should update the MXMB.GL.CONC3 file.
            * Hence the condition Value date equal to last working day was removed
            * IF Y.ALL.MVMT EQ "NO" THEN
            * IF Y.VALUE.DATE EQ R.DATES(EB.DAT.LAST.WORKING.DAY) THEN
            * Y.CONC.FILE3.ID = Y.TXN.REF
            * END
            * END ELSE

            * Y.CONC.FILE3.ID = Y.TXN.REF
            * END
            Y.CONC.FILE3.ID = Y.TXN.REF
            * PACS00200527 -e
            IF Y.CONC.FILE3.ID THEN
                R.CONC.FILE3 = ''
                CONC3.ERR = ''
                RETRY = ''
                CALL F.READU(FN.MXMB.GL.CONC3,Y.CONC.FILE3.ID,R.CONC.FILE3,F.MXMB.GL.CONC3,CONC3.ERR,RETRY)
                * READ R.CONC.FILE3 FROM F.MXMB.GL.CONC3, Y.CONC.FILE3.ID THEN
                IF R.CONC.FILE3 THEN
                    R.CONC.FILE3<-1> = Y.FILE.IDENT:YMOV.FILE.ID      ;*SAP20070824-VW-End
                    * END
                END ELSE
                    R.CONC.FILE3 = Y.FILE.IDENT:YMOV.FILE.ID          ;*SAP20070824-VW-End
                END

                CALL F.WRITE(FN.MXMB.GL.CONC3,Y.CONC.FILE3.ID,R.CONC.FILE3)

                * WRITE R.CONC.FILE3 TO F.MXMB.GL.CONC3, Y.CONC.FILE3.ID ON ERROR
                * PRINT "ERROR WRITING IN":F.MXMB.GL.CONC3:" ID : ":TEMP.LINE
                * END
            END
        END
    END


    RETURN
*
*----------
INITIALIZE:
*----------
    LOG.ACCT.INTERFACE.ONLY.FLAG = ''
    RETURN
***********************************************
*----------
OPEN.FILES:
*----------

    RETURN
***********************************************
*-----------------------
CHECK.PRELIM.CONDITIONS:
*-----------------------

    RETURN
************************************************
WRITE.LINE:
***********
    OPENSEQ Y.IN.PATH,Y.FL.NM TO Y.FL.PTH ELSE
        CREATE Y.FL.PTH THEN
        OPENSEQ Y.IN.PATH,Y.FL.NM TO Y.FL.PTH ELSE
            NULL
        END
    END ELSE
        CALL FATAL.ERROR('Unable to open the LINE ID Log path')
    END
    END
    WRITESEQ LINE.ID.AR APPEND TO Y.FL.PTH THEN
    CLOSESEQ Y.FL.PTH
    END ELSE
    CALL FATAL.ERROR('Unable to write the LINE ID log msg')
    END
    RETURN
*************************************************
UPDATE.ACCT.INTERFACE:

* PACS00202956 -s
*    ACCT.INT.ID = YCATEGORY:"-":YFCY:"-":SESSION.NO
*  READ R.ACCT.INTERFACE FROM F.MXMB.ACCT.INTERFACE,ACCT.INT.ID THEN
*      R.ACCT.INTERFACE<-1> = Y.FILE.IDENT:YMOV.FILE.ID
*  END ELSE
*      R.ACCT.INTERFACE = Y.FILE.IDENT:YMOV.FILE.ID
*  END
* PACS00202956 -e
    ACCT.INT.ID = YCATEGORY:"-":YFCY:"-":Y.FILE.IDENT:YMOV.FILE.ID
    R.ACCT.INTERFACE = Y.FILE.IDENT:YMOV.FILE.ID
    WRITE R.ACCT.INTERFACE TO F.MXMB.ACCT.INTERFACE, ACCT.INT.ID ON ERROR
        INFO.LOG<-1> = "ERROR WRITING IN":F.MXMB.ACCT.INTERFACE:" ID : ":ACCT.INT.ID
        PRINT "ERROR WRITING IN":F.MXMB.ACCT.INTERFACE:" ID : ":ACCT.INT.ID
    END
    YCATEGORY = ''
    YFCY = ''
    ACCT.INT.ID = ''
    RETURN
*************************************************
PROCESS.REP.LINE:

    IF R.LINE.REC<RE.SRL.DESC> THEN

        Y.BALANCE.LINE = R.LINE.REC<RE.SRL.DESC>
        Y.TEMP.VAL = ''
        Y.BALANCE.LINE.TEMP = ''
        CHANGE SM TO VM IN Y.BALANCE.LINE
        Y.CNTR.BAL.LINE = DCOUNT(Y.BALANCE.LINE,VM)
        CNTR = 1
        LOOP
        WHILE CNTR LE Y.CNTR.BAL.LINE
            Y.TEMP.VAL = Y.BALANCE.LINE<1,CNTR>

            IF INDEX(Y.TEMP.VAL,'-',1) THEN

                Y.CR.AC = Y.TEMP.VAL
                CHANGE '-' TO '' IN Y.CR.AC
                IF NUM(Y.CR.AC) THEN
                    Y.BALANCE.LINE.TEMP = Y.TEMP.VAL
                    BREAK
                END
                * HD1012260 -s
            END ELSE
                IF NUM(Y.TEMP.VAL) THEN
                    Y.BALANCE.LINE.TEMP = Y.TEMP.VAL
                    BREAK
                END
                * HD1012260 -e
            END
            CNTR++
        REPEAT

        IF Y.BALANCE.LINE.TEMP THEN
            Y.BALANCE.LINE = Y.BALANCE.LINE.TEMP
        END ELSE

            *--LINE IDs with blank description are excluded & written in a file--*
            LINE.ID.AR = "LINE-":Y.LINE.ID:"LINE MVMT ID-":LINE.MVMT.ID
            GOSUB WRITE.LINE
            LOG.ACCT.INTERFACE.ONLY.FLAG = "TRUE"

            *--Pratosh Dwivedi - 29 Sep'07 --*

        END

**********************************************************************

    END ELSE

        *--LINE IDs with blank description are excluded & written in a file--*
        LINE.ID.AR = "LINE-":Y.LINE.ID:"LINE MVMT ID-":LINE.MVMT.ID
        GOSUB WRITE.LINE

        LOG.ACCT.INTERFACE.ONLY.FLAG = "TRUE"
        *--Pratosh Dwivedi - 29 Sep'07 --*

    END
* TAM_START

    R.LINE.MVMT.REC = ''
    CALL F.READ(FN.RE.STAT.LINE.MVMT,LINE.MVMT.ID,R.LINE.MVMT.REC,F.RE.STAT.LINE.MVMT,LINE.MVMMT.ERR)
    IF R.LINE.MVMT.REC THEN
        * TAM_END

        * Boo - Debug Purpose

        INFO.LOG<-1> =  R.LINE.MVMT.REC

        Y.NRO.ENTRIES = DCOUNT(R.LINE.MVMT.REC,FM)
        *

        * Identify the name of the file to process
        Y.FILE = FIELD(LINE.MVMT.ID,"-",5)

        LOOP
            Y.ID.MVMT = R.LINE.MVMT.REC<Y.NRO.ENTRIES>
            CONC.FILE = F.MXMB.GL.CONC1


        WHILE Y.NRO.ENTRIES > 0
            BEGIN CASE
                CASE Y.FILE EQ 'A'
                    FN.ENT.FILE         = FN.RE.CONSOL.STMT.ENT.KEY
                    F.ENT.FILE         = F.RE.CONSOL.STMT.ENT.KEY
                    FN.FILE          = FN.RE.CONSOL.STMT.ENT.KEY
                    FN.MVMT.FILE        = FN.STMT.ENTRY
                    F.MVMT.FILE        = F.STMT.ENTRY
                    YLOCAL.REF       = AC.STE.LOCAL.REF
                    YAMOUNT.LCY      = AC.STE.AMOUNT.LCY
                    YTRANS.CODE      = AC.STE.TRANSACTION.CODE
                    Y.FCY = AC.STE.CURRENCY
                    Y.CATEGORY = AC.STE.PRODUCT.CATEGORY
                    Y.PL.CATEGORY = AC.STE.PL.CATEGORY
                    YAMOUNT.FCY = AC.STE.AMOUNT.FCY
                    Y.TXN = AC.STE.TRANS.REFERENCE
                    LRF.POS.BLINE    = STMT.Y.POS1
                    LRF.POS.SIGN     = STMT.Y.POS2
                    FILE.ID.SUFFIX = '.'

                    GOSUB READ.ENT.FILE

                CASE Y.FILE EQ 'P'
                    FN.ENT.FILE         = FN.RE.CONSOL.PROFIT
                    F.ENT.FILE         = F.RE.CONSOL.PROFIT
                    FN.FILE          = FN.RE.CONSOL.PROFIT
                    FN.MVMT.FILE        = FN.CATEG.ENTRY
                    F.MVMT.FILE        = F.CATEG.ENTRY
                    YLOCAL.REF       = AC.CAT.LOCAL.REF
                    YAMOUNT.LCY      = AC.CAT.AMOUNT.LCY
                    YTRANS.CODE      = AC.CAT.TRANSACTION.CODE
                    Y.FCY = AC.CAT.CURRENCY
                    Y.CATEGORY = AC.CAT.PRODUCT.CATEGORY
                    Y.PL.CATEGORY = AC.CAT.PL.CATEGORY
                    YAMOUNT.FCY = AC.CAT.AMOUNT.FCY
                    Y.TXN = AC.CAT.TRANS.REFERENCE
                    LRF.POS.BLINE    = CATEG.Y.POS1
                    LRF.POS.SIGN     = CATEG.Y.POS2
                    FILE.ID.SUFFIX = ';'

                    GOSUB READ.ENT.FILE

                CASE Y.FILE EQ 'R'
                    FN.ENT.FILE        = FN.RE.CONSOL.SPEC.ENT.KEY
                    F.ENT.FILE         = F.RE.CONSOL.SPEC.ENT.KEY
                    FN.FILE          = FN.RE.CONSOL.SPEC.ENT.KEY
                    FN.MVMT.FILE        = FN.RE.CONSOL.SPEC.ENTRY
                    F.MVMT.FILE        = F.RE.CONSOL.SPEC.ENTRY
                    YLOCAL.REF       = RE.CSE.LOCAL.REF
                    YAMOUNT.LCY      = RE.CSE.AMOUNT.LCY
                    YTRANS.CODE      = RE.CSE.TRANSACTION.CODE
                    Y.FCY            = RE.CSE.CURRENCY
                    Y.CATEGORY = RE.CSE.PRODUCT.CATEGORY
                    Y.PL.CATEGORY = RE.CSE.PL.CATEGORY
                    YAMOUNT.FCY = RE.CSE.AMOUNT.FCY
                    Y.TXN = RE.CSE.TRANS.REFERENCE
                    LRF.POS.BLINE    = CONSOL.Y.POS1
                    LRF.POS.SIGN     = CONSOL.Y.POS2
                    FILE.ID.SUFFIX = '.'

                    GOSUB READ.ENT.FILE
            END CASE
            *
            Y.NRO.ENTRIES -= 1
        REPEAT

    END ELSE
        INFO.LOG<-1> =  " RECORD LINE.MVMT.ID: ":LINE.MVMT.ID:" DOESN'T EXIST IN :":FN.RE.STAT.LINE.MVMT
        PRINT " RECORD LINE.MVMT.ID: ":LINE.MVMT.ID:" DOESN'T EXIST IN :":FN.RE.STAT.LINE.MVMT
    END

    RETURN
*************************************************
UPDATE.NETTING.ENTRY.RECORD:
*****************************
* Banorte -s
    YMOV.FILE.ID = Y.ID.ENTRY
    R.MVMT.FILE = ''
    CALL F.READ(FN.MVMT.FILE,YMOV.FILE.ID,R.MVMT.FILE,F.MVMT.FILE,MVMT.FILE.ERR)
    IF R.MVMT.FILE THEN
        R.MVMT.FILE<YLOCAL.REF, LRF.POS.BLINE> = Y.BALANCE.LINE
        Y.AMOUNT.LCY = R.MVMT.FILE<YAMOUNT.LCY>

        IF Y.AMOUNT.LCY GE 0 THEN
            R.MVMT.FILE<YLOCAL.REF, LRF.POS.SIGN> = 'C'
        END ELSE
            R.MVMT.FILE<YLOCAL.REF, LRF.POS.SIGN> = 'D'
        END
        WRITE R.MVMT.FILE TO F.MVMT.FILE, YMOV.FILE.ID ON ERROR
            INFO.LOG<-1> = " ERROR WRITING IN":F.MVMT.FILE:" ID : ":YMOV.FILE.ID
            PRINT " ERROR WRITING IN":F.MVMT.FILE:" ID : ":YMOV.FILE.ID
        END
        INFO.LOG<-1> = 'NETTING KEY UPDATED : ':YMOV.FILE.ID
        PRINT 'NETTING KEY UPDATED : ':YMOV.FILE.ID

    END

    RETURN
* Banorte -e
    END
*************************************************
