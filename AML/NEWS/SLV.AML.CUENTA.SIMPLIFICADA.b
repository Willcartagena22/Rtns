*-----------------------------------------------------------------------------
* <Rating>638</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.AML.CUENTA.SIMPLIFICADA

*-----------------------------------------------------------------------------
* Modification History :

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.AML.CUSTOMER.SIMP
    $INSERT I_F.CUSTOMER
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
    $INSERT I_F.CUSTOMER.ACCOUNT
    $INSERT I_F.ACCOUNT
    $INSERT I_F.DISPO.ITEMS
    $INSERT I_F.AA.ACCOUNT
    $INSERT I_F.AA.ARRANGEMENT

*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB CAT.SIMP
    GOSUB CONSULTAR
    GOSUB ENVIAR
    GOSUB ACTUALIZAR
    RETURN

INIT:
    FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = ''
    FN.CUS				='F.CUSTOMER'
    F.CUS				=''
    FN.CUS.AC			='F.CUSTOMER.ACCOUNT'
    F.CUS.AC			=''
    FN.ACC 	= 'F.ACCOUNT'
    F.ACC  	= ''
    FN.AML='FBNK.EB.SLV.AML.CUSTOMER.SIMP'
    F.AML=''
    FN.DISPO.ITEMS.H = "F.DISPO.ITEMS$HIS"
    F.DISPO.ITEMS.H = ""
    FN.DISPO.ITEMS = "F.DISPO.ITEMS"
    F.DISPO.ITEMS = ""
    CALL OPF(FN.DISPO.ITEMS,F.DISPO.ITEMS)
    CALL OPF(FN.DISPO.ITEMS.H,F.DISPO.ITEMS.H)
    CALL OPF(FN.TABLE.PA, F.TABLE.PA)
    CALL OPF(FN.CUS, F.CUS)
    CALL OPF(FN.CUS.AC, F.CUS.AC)
    CALL OPF(FN.AML, F.AML)
    CALL OPF(FN.ACC, F.ACC)
    RETURN

CONSULTAR:
    PARAM.LOG='SLV.PARAM.LOG.SIMP'
    CALL F.READ(FN.TABLE.PA, PARAM.LOG, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    LOGS=R.TABLE.PA<EB.SLV39.VALOR.PARAM>
    FECHA=TODAY

    SELECT.CUST.ENVIAR = "SELECT ": FN.AML:" WITH DATE.C EQ '": FECHA:"' AND PROCESSING EQ 'NO'"
    CALL EB.READLIST(SELECT.CUST.ENVIAR, LIST.CUST.ENV, '', NO.OF.CUST.ENV, ERR.CUST)
    SELECT.CUST.UPDATE = "SELECT ": FN.AML:" WITH DATE.C EQ '": FECHA:"' AND PROCESSING EQ 'SI'"
    CALL EB.READLIST(SELECT.CUST.UPDATE, LIST.CUST.UPDATE, '', NO.OF.CUST.UPDATE, ERR.CUST)
    RETURN


ENVIAR.MENSAJE:

    CALL F.READ(FN.TABLE.PA, PARAM.LOG2, R.TABLE.PA.2, F.TABLE.PA, F.ERR.PA.2)
    TEXTO.MSJ=R.TABLE.PA.2<EB.SLV39.VALOR.PARAM>
    TEXTO.MSJ1 = FIELD(TEXTO.MSJ,VM,1)
    TEXTO.MSJ2 = FIELD(TEXTO.MSJ,VM,2)
    TEXTO.MSJ3 = FIELD(TEXTO.MSJ,VM,3)

    IF TEXTO.MSJ3 THEN
        TEXTO.MSJ4=TEXTO.MSJ1:' ': TEXTO.MSJ2:' ':TEXTO.MSJ3
    END

    ELSE
    TEXTO.MSJ4=TEXTO.MSJ1:' ': TEXTO.MSJ2
    END


    CALL F.READ(FN.CUS, CUSTOMER, R.CUS, F.CUS, F.ERR.CUS)
    TELEFONO=R.CUS<EB.CUS.SMS.1>

    TEXTO.ARCHIVO='TEL: ':TELEFONO: 'MSJ: ':TEXTO.MSJ4
    GOSUB ESCRIBIR.ARCHIVO
    CALL SLV.ENVIO.MENSAJES.HDM(TELEFONO,TEXTO.MSJ4)

    RETURN
ENVIAR:
    IF LIST.CUST.ENV THEN
        FOR I = 1 TO NO.OF.CUST.ENV
            CUSTOMER=LIST.CUST.ENV<I>
            
            IF CUSTOMER THEN
                OFS.CUST='CUSTOMER,SLV.AML.PN/I/PROCESS//0,/,':CUSTOMER:',CUSTOMER.CURRENCY:1:1=USD,LANGUAGE:1:1=2,LF.EXE.TAX:1:1=NO'
                Y.OPTIONS='SLVOFSPS'
                CUSTOMER2=CUSTOMER
                CALL F.READ(FN.AML, CUSTOMER2, R.TABLE.AML, F.AML, F.ERR.AML)
                R.TABLE.AML<EB.EB.84.PROCESSING>='SI'
                GOSUB CONSULTAR.CUENTA
                IF CUENTA.N THEN
                    R.TABLE.AML<EB.EB.84.ACCOUNT>=CUENTA.N
                END
                CALL F.WRITE (FN.AML,CUSTOMER, R.TABLE.AML)
*                CALL JOURNAL.UPDATE(FN.AML)
                CALL OFS.POST.MESSAGE(OFS.CUST,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)

            END

        NEXT
    END

    RETURN

ACTUALIZAR:
    DISPO.STATUS=''
    HIT=''
    ESCREENEADO=''
    CUENTA.N=''
    FASE=''
	IF LIST.CUST.UPDATE THEN
	 FOR I = 1 TO NO.OF.CUST.UPDATE
	 
	 CUSTOMER=LIST.CUST.UPDATE<I>
	 IF CUSTOMER THEN

         CALL F.READ(FN.AML, CUSTOMER, R.TABLE.AML, F.AML, F.ERR.AML)
         CUSTOMER.N=CUSTOMER:'*BNK'
         FASE =R.TABLE.AML<EB.EB.84.RESERVADO.10>
         HIT = R.TABLE.AML<EB.EB.84.HIT.TYPE>
         
         IF FASE NE 'REVISION' THEN
         GOSUB CONSULTAR.CUENTA 
		 IF CUENTA.N THEN
            R.TABLE.AML<EB.EB.84.ACCOUNT>=CUENTA.N
         END

	     CALL F.READ(FN.DISPO.ITEMS, CUSTOMER.N, R.DISPO, F.DISPO.ITEMS, ERR.D)
         DISPO.STATUS=R.DISPO<DISP.ITM.DISPO.STATUS>
	     TEXTO.ARCHIVO='CUSTOMER : ':CUSTOMER:' DISPOLIVE: ':DISPO.STATUS
    	 GOSUB ESCRIBIR.ARCHIVO	 
		 IF DISPO.STATUS THEN 
			IF DISPO.STATUS EQ 'POSSIBLE' THEN
			R.TABLE.AML<EB.EB.84.HIT.TYPE>='POSSIBLE'
			CALL F.WRITE (FN.AML,CUSTOMER, R.TABLE.AML)
*			CALL JOURNAL.UPDATE (FN.AML)
			END		 
		 END
		 ELSE ;* IF DISPO.STATUS THEN 

         CALL F.READ.HISTORY(FN.DISPO.ITEMS.H, CUSTOMER.N, R.DISPO, F.DISPO.ITEMS.H, ERR.D)
         DISPO.STATUS=R.DISPO<DISP.ITM.DISPO.STATUS>
         TEXTO.ARCHIVO='CUSTOMER : ':CUSTOMER:' DISPOHIS: ':DISPO.STATUS
    	 GOSUB ESCRIBIR.ARCHIVO	 
         
		 IF DISPO.STATUS EQ 'APPROVED' THEN
				R.TABLE.AML<EB.EB.84.HIT>='NO'
                GOSUB LEVANTAR.RESTRICCION
                R.TABLE.AML<EB.EB.84.ACCOUNT>=CUENTA.N
                R.TABLE.AML<EB.EB.84.RESERVADO.1>='CUENTA APROBADA'
                R.TABLE.AML<EB.EB.84.RESERVADO.10>='REVISION'
                R.TABLE.AML<EB.EB.84.HIT.TYPE>='APPROVED'
                CALL F.WRITE (FN.AML,CUSTOMER, R.TABLE.AML)
*                CALL JOURNAL.UPDATE (FN.AML)
                PARAM.LOG2='SLV.MSJ.CUENTA.SIMP.ACTIVA'
				GOSUB ENVIAR.MENSAJE		 
		 END ;*IF DISPO.STATUS EQ 'APPROVED'		 
		 
		 ELSE ;*IF DISPO.STATUS EQ 'APPROVED'
		 IF DISPO.STATUS EQ 'REJECTED' THEN
			 R.TABLE.AML<EB.EB.84.HIT>='SI'
			 R.TABLE.AML<EB.EB.84.ACCOUNT>=CUENTA.N
			 R.TABLE.AML<EB.EB.84.RESERVADO.10>='REVISION'
			 R.TABLE.AML<EB.EB.84.HIT.TYPE>='REJECTED'
			 CALL F.WRITE (FN.AML,CUSTOMER, R.TABLE.AML)
*			 CALL JOURNAL.UPDATE (FN.AML)
			 PARAM.LOG2='SLV.MSJ.CUENTA.SIMP.DENEGADA'
			 GOSUB ENVIAR.MENSAJE		 
		 END		 
		 END ;* ELSE DISPO.STATUS EQ 'APPROVED
		 END ;*ELSE IF DISPO.STATUS
		 GOSUB ESCREENEO.2
		END
		 
	 END ;*IF CUSTOMER THEN 
	 NEXT 
	END ;*LIST.CUST.UPDATE THEN	
    RETURN




CONSULTAR.CUENTA:
    NUMERO=''
    CATEGORY=''

    CALL F.READ(FN.CUS.AC,CUSTOMER, R.CUS.AC, F.CUS.AC, F.ERR.CUS.AC)
    NUMERO=COUNT(R.CUS.AC,VM)
    IF R.CUS.AC THEN
        NUMERO=NUMERO+1
        FOR j=1 TO NUMERO
            CUENTA.N= FIELD(R.CUS.AC,FM,j)

            CALL F.READ(FN.ACC, CUENTA.N, R.ACCOUNT, F.ACC, Y.ERR.AC)
            CATEGORY=R.ACCOUNT<AC.CATEGORY>

            IF CATEGORY EQ CATEGORY.SIMP THEN
                ARRANGEMENT=R.ACCOUNT<AC.ARRANGEMENT.ID>

            END
        NEXT

    END

    RETURN

ESCREENEO.2:
    DISPO=R.TABLE.AML<EB.EB.84.HIT.TYPE>
    FASE=R.TABLE.AML<EB.EB.84.RESERVADO.10>
    IF DISPO EQ 'APPROVED' AND FASE NE 'REVISION'   THEN
        R.TABLE.ML<EB.EB.84.HIT>='NO'
        GOSUB LEVANTAR.RESTRICCION
        R.TABLE.AML<EB.EB.84.ACCOUNT>=CUENTA.N
        R.TABLE.AML<EB.EB.84.RESERVADO.1>='CUENTA APROBADA'
        R.TABLE.AML<EB.EB.84.RESERVADO.10>='REVISION'
        CALL F.WRITE (FN.AML,CUSTOMER, R.TABLE.AML)
*        CALL JOURNAL.UPDATE (FN.AML)
        PARAM.LOG2='SLV.MSJ.CUENTA.SIMP.ACTIVA'
        GOSUB ENVIAR.MENSAJE
    END


    RETURN




LEVANTAR.RESTRICCION:
    Y.OPTIONS='SLVOFSPS'
    OFS='AA.ARRANGEMENT.ACTIVITY,/I///0,/,,ARRANGEMENT:1:1=':ARRANGEMENT:',ACTIVITY:1:1=SLV.COND.NINGUNA'

    TEXTO.ARCHIVO='OFS: ':OFS
    GOSUB ESCRIBIR.ARCHIVO

    CALL OFS.POST.MESSAGE(OFS,Y.RESPONSE,Y.OPTIONS,TXN.RESULT)

    RETURN

CAT.SIMP:

    FN.AA.PRODUCT.DESIGNER = 'F.AA.PRD.DES.ACCOUNT'
    F.AA.PRODUCT.DESIGNER = ''
    CALL OPF(FN.AA.PRODUCT.DESIGNER,F.AA.PRODUCT.DESIGNER)
    Y.PRODUCTO='CUENTA.AHORRO.SIM'
    SELECT.PROD.DES = "SELECT " : FN.AA.PRODUCT.DESIGNER : " WITH @ID LIKE '" : Y.PRODUCTO : "...'"
    CALL EB.READLIST(SELECT.PROD.DES, PROD.DES,'',NO.REC.PROD.DES, ERR.REC.PROD.DES)
    IF NO.REC.PROD.DES NE 0 THEN
        CALL F.READ(FN.AA.PRODUCT.DESIGNER,PROD.DES, PROD.DES.REC, F.AA.PRODUCT.DESIGNER, ERR.REC1)
        IF PROD.DES.REC THEN


            CALL F.READ(FN.AA.PRODUCT.DESIGNER,PROD.DES, PROD.DES.REC, F.AA.PRODUCT.DESIGNER, ERR.REC1)
            IF PROD.DES.REC THEN
                CATEGORY.SIMP = PROD.DES.REC<AA.AC.CATEGORY> ;*Obtener el parent para luego ir a traer sus propiedades
            END

        END

    END
    RETURN








ESCRIBIR.ARCHIVO:
    IF LOGS EQ 'SI' THEN
        DIR.NAME= 'SIMPLIFICADA'
        R.ID   = 'Aml.txt'
        OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
        WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    END
    RETURN

    END
