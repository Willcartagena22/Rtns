*-----------------------------------------------------------------------------
* <Rating>-59</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.A.TCIB.ADD.SIMP

* Modification History
* 1.0		calvarado	19.09.2019	Rutina encargada de anadir automaticamente al ISA cuenta simplificada
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_AA.APP.COMMON
    $INSERT I_F.EB.EXTERNAL.USER
    $INSERT I_F.AA.PRODUCT.ACCESS
    $INSERT I_F.AA.ARRANGEMENT
    $INSERT I_F.AA.ARRANGEMENT.ACTIVITY
    $INSERT I_F.AA.CUSTOMER

        GOSUB INIT
        GOSUB EVALUAR


INIT:
    FN.EXT.USER	 = 'F.EB.EXTERNAL.USER'
    F.EXT.USER	 = ''
    CALL OPF(FN.EXT.USER, F.EXT.USER)

    FN.AA.CUS	 = 'F.AA.ARR.CUSTOMER$NAU'
    F.AA.CUS	 = ''
    CALL OPF(FN.AA.CUS, F.AA.CUS)

    FN.CUS				='F.CUSTOMER'
    F.CUS				=''
    CALL OPF(FN.CUS, F.CUS)

    FN.ARR 	= 'F.AA.ARRANGEMENT'
    F.ARR  	= ''
    CALL OPF(FN.ARR, F.ARR)


    EQU PERSONAL  TO 'PERSONAL'
    EQU ACTIVE    TO 'ACTIVE'
    EQU OFS.SOURCE TO 'SLVOFSPS'
    EQU ACCOUNTS TO 'ACCOUNTS'
    EQU DEPOSITS TO 'DEPOSITS'
    EQU LENDING TO 'LENDING'

;*Leyendo variables necesarias para la adicion automatica de cuenta.
    Y.CUSTOMER		 = AA$R.ARRANGEMENT<AA.ARR.CUSTOMER>
    Y.NEW.ACC		 =  AA$R.ARRANGEMENT<AA.ARR.LINKED.APPL.ID>
    Y.NEW.ARR.ID 	 = R.NEW(AA.ARR.ACT.ARRANGEMENT)
    Y.ARRGMNT=Y.NEW.ARR.ID

    Y.TXT='INIT Y.NEW.ACC: ':Y.NEW.ACC:','
    GOSUB WRITE_LOG_FILE

;*Debug
*	Y.CUSTOMER	= '139796'
*	Y.NEW.ACC	= '10000000556171'
*	Y.NEW.ARR.ID = 'AA190256PWMJ'
*	Y.ARRGMNT=Y.NEW.ARR.ID
*	Y.PRODUCT.LINE	 = 'ACCOUNTS'
*	Y.PRODUCT.LINE	 = 'DEPOSITS'
*	Y.PRODUCT.LINE	 = 'LENDING'
    RETURN

EVALUAR:
    IF Y.ARRGMNT THEN
        CALL F.READ(FN.ARR, Y.ARRGMNT, R.ACCOUNT, F.ARR, Y.ERR.AC)
        SIMPLIFICADA=R.ACCOUNT<AA.ARR.PRODUCT>

        IF SIMPLIFICADA EQ 'AHORRO.SIM' THEN
            IF Y.NEW.ACC THEN
                Y.CUSTOMER	=R.ACCOUNT<AA.ARR.CUSTOMER>
                Y.PRODUCT.LINE=R.ACCOUNT<AA.ARR.PRODUCT.LINE>
                Y.TXT='IF Y.NEW.ACC THEN ':Y.NEW.ACC
                GOSUB WRITE_LOG_FILE
                GOSUB PROCESS
            END
            ELSE
            Y.NEW.ACC =  R.NEW(AA.ARR.LINKED.APPL.ID)
            Y.CUSTOMER	=R.ACCOUNT<AA.ARR.CUSTOMER>
            Y.PRODUCT.LINE=R.ACCOUNT<AA.ARR.PRODUCT.LINE>
            Y.TXT='IF Y.NEW.ACC THEN ':Y.NEW.ACC
            GOSUB WRITE_LOG_FILE
            GOSUB PROCESS
        END

        IF Y.NEW.ACC EQ '' THEN
            Y.NEW.ACC =  R.NEW(AA.ARR.LINKED.APPL.ID)<1,1>
            Y.TXT='IF Y.NEW.ACC THEN R.NEW<>':Y.NEW.ACC
            GOSUB WRITE_LOG_FILE
            GOSUB PROCESS
        END

        IF Y.NEW.ACC EQ '' THEN
            Y.NEW.ACC =R.ACCOUNT<AA.ARR.LINKED.APPL.ID><1,1>
            Y.TXT='IF Y.NEW.ACC THEN R.ACC <>':Y.NEW.ACC
            GOSUB WRITE_LOG_FILE
            GOSUB PROCESS
        END





    END
    END

    RETURN


PROCESS:
;*Realizar adicion si y solo si ya cuenta con un usuario de banca en linea
    SELECT.EXTERNAL	 = "SELECT " : FN.EXT.USER : " WITH CUSTOMER EQ " : "'" : Y.CUSTOMER : "'"
    SELECT.EXTERNAL := " AND USER.TYPE EQ " :  "'"  : PERSONAL : "'"
    SELECT.EXTERNAL := " AND STATUS EQ " : "'" : ACTIVE : "'"
    CALL EB.READLIST (SELECT.EXTERNAL, KEYS.EXT.USERS, '', NO.OF.EXT.USERS, ERR.EXT.USERS)
    Y.TXT='PROCESS'
    GOSUB WRITE_LOG_FILE
    IF KEYS.EXT.USERS THEN
        CALL F.READ(FN.EXT.USER, KEYS.EXT.USERS<1>, REC.EXTERNAL.USER, F.EXT.USER, ERR.EXTERNAL.USER)
        Y.ID.ISA = REC.EXTERNAL.USER<EB.XU.ARRANGEMENT>

        ;*Lectura de cuentas configurado actualmente en el product access
        CALL AA.GET.ARRANGEMENT.CONDITIONS(Y.ID.ISA, 'PRODUCT.ACCESS', '', TODAY, R.ID.PROD.ACC, R.PRODUCT.ACCESS , ERR.PRODUCT.ACCESS)
        REC.PRODUCT.ACCESS = RAISE(R.PRODUCT.ACCESS)

        ;*Se anade la nueva cuenta.
        Y.CAMPO.OFS	= ""
        IF Y.PRODUCT.LINE EQ ACCOUNTS THEN ;*Cuenta se anade para txn si no es mancomunada. Si es mancomunada, si es tipo Y: a visualizar, O: a txn
            ;*Validar si la cuenta que esta creando es mancomunada
            Y.TXT='PRODUCT LINE'
            GOSUB WRITE_LOG_FILE
            Y.CAMPO.ISA	= "ACCT.TRANS:"
            Y.CANT.ACCT.TRANS = DCOUNT(REC.PRODUCT.ACCESS<AA.PRODA.ACCT.TRANS>, @VM)
            Y.NEW.POS.ISA = Y.CANT.ACCT.TRANS + 1
            Y.NEW.VAL.ISA = ":1,FIELD.VALUE:1:1=" : Y.NEW.ACC
            Y.TXT='Y.FLAG.MANCOMUNADA EQ 0 '
            GOSUB WRITE_LOG_FILE
            GOSUB SEND.OFS

        END
    END
    RETURN

SEND.OFS:
    Y.CAMPO.OFS	= Y.CAMPO.ISA : Y.NEW.POS.ISA : Y.NEW.VAL.ISA
    Y.TXT='Y.CAMPO.OFS: ':Y.CAMPO.OFS
    GOSUB WRITE_LOG_FILE
    STRING.OFS.ADD.PRD	 = ""
    STRING.OFS.ADD.PRD	:= "AA.ARRANGEMENT.ACTIVITY,SLV.TCIB.ADD.PRDS/I/PROCESS//0,/"
    STRING.OFS.ADD.PRD	:= ",,ARRANGEMENT:1:1=" : Y.ID.ISA : ",ACTIVITY:1:1=INTERNET.SERVICES-UPDATE-PRODACCESS,CURRENCY:1:1=USD,PROPERTY:1:1=PRODACCESS,"
    STRING.OFS.ADD.PRD	:= "FIELD.NAME:1:1=" : Y.CAMPO.OFS
;*Envio de OFS
    Y.TXT='STRING.OFS.ADD.PRD: ':STRING.OFS.ADD.PRD
    GOSUB WRITE_LOG_FILE
    CALL OFS.POST.MESSAGE(STRING.OFS.ADD.PRD,'', OFS.SOURCE,'')

;*Debug
*	Y.TXT = "OFS FINAL -> " : STRING.OFS.ADD.PRD
*  	GOSUB WRITE_LOG_FILE
    RETURN



WRITE_LOG_FILE:
    DIR.NAME = 'SIMPLIFICADA'
    R.ID = 'CREACION_AC' : '_' : TODAY : '.txt'
    OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
    WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
