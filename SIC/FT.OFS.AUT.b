*-----------------------------------------------------------------------------
* <Rating>-25</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE FT.OFS.AUT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.OFS.SOURCE

    GOSUB INIT
    GOSUB SELECT.PROCESS
    RETURN

INIT:
    APP.NAME = 'FUNDS.TRANSFER'
    OFSFUNCT='A'
    PROCESS='PROCESS'
    OFSVERSION='FUNDS.TRANSFER,'
    GTSMODE='1'

    NO.OF.AUTH=''
    R.RECORD=''
    R.OFS.RECORD=''
    R.OFS.SOURCE=OFS$SOURCE.REC
    IN.QUEUE.PATH = R.OFS.SOURCE<OFS.SRC.IN.QUEUE.DIR>
    V.ERROR=''
    FN.FT = 'F.FUNDS.TRANSFER$NAU'
    SEL.LIST =''
    NO.OF.REC = 0
    RET.CODE =''
    Y.FT.ID =''
    R.FT =''
    F.FT =''
    Y.FT.ERR = ''
    REC='R1'
    INDIR='INDIR1'
    F.BP=''
    MSG.ID= ''

    RETURN

SELECT.PROCESS:
    CALL OPF(FN.FT,F.FT)
*TO OPEN INAU FT RECORDS FROM $NAUFILES

    SEL.CMD= "SELECT ":FN.FT:" WITH RECORD.STATUS='INAU'"
    CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,RET.CODE)
    LOOP
        REMOVE Y.FT.ID FROM SEL.LIST SETTING POS
    WHILE Y.FT.ID
        CALL F.READU(FN.FT,Y.FT.ID,R.FT,F.FT,Y.FT.ERR,'')
        CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCT,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,Y.FT.ID,R.RECORD,R.OFS.RECORD)

*TO OPEN THE IN QUEUE
        CALL ALLOCATE.UNIQUE.TIME(MSG.ID)
        OPENSEQ IN.QUEUE.PATH,MSG.ID TO  F.BP ELSE
            CREATE F.BP ELSE
                ETEXT = "UNABLE TO CREATE MsgDetails.txt"
            END
        END
*TO WRITE TO THE IN QUEUE
        WRITESEQ R.OFS.RECORD TO F.BP ELSE
            ETEXT = "UNABLE TO WRITE"
        END
*TO CLOSE THE INQUEUE AFTER WRITING
        CLOSESEQ F.BP

    REPEAT
    RETURN
END




