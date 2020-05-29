*-----------------------------------------------------------------------------
* <Rating>965</Rating>
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
SUBROUTINE SLV.E.NOF.REPORTE.IND.CAJ.COL(A.INFO)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.EB.SLV.COLECTOR
;*$INSERT I_F.EB.SLV.COLECTOR.TRX.PAGO.COL
$INSERT I_F.EB.SLV.COLECTOR.TRX.PAGO
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.COMPANY
$INSERT I_F.TFS.TRANSACTION
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.FT.TXN.TYPE.CONDITION
*-----------------------------------------------------------------------------

GOSUB INIT:
GOSUB PROCCESS:
RETURN

INIT:

FN.TABLA.PAGO.COL = 'F.EB.SLV.COLECTOR.TRX.PAGO'
F.TABLA.PAGO.COL  = ''

FN.TABLA.DET.TFS  = 'F.TELLER.FINANCIAL.SERVICES'
F.TABLA.DET.TFS   = ''

FN.TABLA.DET.FT   = 'F.FUNDS.TRANSFER'
F.TABLA.DET.FT    = ''

FN.TABLA.COLECTOR = 'F.EB.SLV.COLECTOR'
F.TABLA.COLECTOR  = ''

FN.AGENCIA = 'F.COMPANY'
F.AGENCIA  = ''

FN.TFS.TRX = 'F.TFS.TRANSACTION'
F.TFS.TRX  = ''

FN.TF.TYPE.CON = 'F.FT.TXN.TYPE.CONDITION'
F.TF.TYPE.CON  = ''

FN.FUNDS.TRANSFER$HIS = 'F.FUNDS.TRANSFER$HIS'
F.FUNDS.TRANSFER$HIS  = ''

FN.USER.APLICATION='F.USER'
F.USER.APLICATION=''


CALL OPF(FN.TABLA.PAGO.COL,F.TABLA.PAGO.COL)
CALL OPF(FN.TABLA.DET.TFS,F.TABLA.DET.TFS)
CALL OPF(FN.TABLA.COLECTOR,F.TABLA.COLECTOR)
CALL OPF(FN.AGENCIA,F.AGENCIA)
CALL OPF(FN.TFS.TRX,F.TFS.TRX)
CALL OPF(FN.TABLA.DET.FT,F.TABLA.DET.FT)
CALL OPF(FN.TF.TYPE.CON,F.TF.TYPE.CON)
CALL OPF(FN.FUNDS.TRANSFER$HIS,F.FUNDS.TRANSFER$HIS)
ID.TRX.COL = ''

;*PROCEDEMOS A OBTENER EL ID DEL COLECTOR
STMT.ARR.ID.PEX = "SELECT ":FN.TABLA.COLECTOR:" WITH NOMBRE.COLECTOR LIKE %Punto%"
CALL EB.READLIST(STMT.ARR.ID.PEX,ARR.LST.ID.PEX,'',COUNT.ID.PEX,Y.ARR.ERR.PEX)

FOR U=1 TO COUNT.ID.PEX
  ID.PEX = ARR.LST.ID.PEX<U>
NEXT U 

TEXTO.ARCHIVO = 'ID.PEX >':ID.PEX

 
RETURN



PROCCESS:


LOCATE "ID.COLECTOR" IN D.FIELDS<1> SETTING POS THEN
S.COLECTOR.ID = D.RANGE.AND.VALUE<POS>
END


LOCATE "FIELD.1" IN D.FIELDS<1> SETTING POSDATE THEN
S.DATE.FIND = D.RANGE.AND.VALUE<POSDATE>
END


S.DATE.FIND='20160716'



IF S.COLECTOR.ID EQ ID.PEX THEN
	CALL SLV.V.GET.COLECTOR.PEX(S.DATE.FIND,A.INFO.PEX)
	A.INFO = A.INFO.PEX
END ;*FIN DE IF DONDE SE VALIDA SI S.COLECTOR.ID ES IGUAL A ID.PEX
ELSE

	ID.BUSQUEDA = S.DATE.FIND[1,4]:S.DATE.FIND[5,2] 
	
	STMT.ARRANGEMENT = "SELECT ":FN.TABLA.PAGO.COL:" WITH @ID LIKE %":ID.BUSQUEDA:"%"
	
	
	;*EJECUTAMOS EL QUERY
	CALL EB.READLIST(STMT.ARRANGEMENT, ARRANGEMENT.LIST,'',NO.OF.RECS,Y.ARRANGEMENT.ERR1)
	
	FOR I=1 TO NO.OF.RECS
		ID.TRX.COL = ARRANGEMENT.LIST<I>
		;*OBTENEMOS EL DETALLE DEL PAGO DEL COLECTOR
		CALL F.READ(FN.TABLA.PAGO.COL,ID.TRX.COL,RS.PAGO.COL,F.TABLA.PAGO.COL,PAGO.ERR)
		
		ID.T24 = RS.PAGO.COL<EB.SLV54.ID.T24>
		
		ID.AGENCIA  = RS.PAGO.COL<EB.SLV54.AGENCIA>
		ID.COLECTOR = RS.PAGO.COL<EB.SLV54.ID.COLECTOR> 
		CANAL.PAGO  = RS.PAGO.COL<EB.SLV54.CANAL.PAGO>
		USUARIO.TRX = RS.PAGO.COL<EB.SLV54.USUARIO>
		NO.CONF.PEX = RS.PAGO.COL<EB.SLV54.ID.PUNTO.EXPRESS>
		NPE.TRX     = RS.PAGO.COL<EB.SLV54.CMP.RESERVA.9>
		
		;*CONSULTAMOS LA AGENCIA.
		CALL F.READ(FN.AGENCIA,ID.AGENCIA,RS.AGENCIA.APP,F.AGENCIA,AGENCIA.ERR)
		NOMBRE.AGENCIA  = RS.AGENCIA.APP<EB.COM.COMPANY.NAME>
		
		;*CONSULTAMOS EL COLECTOR
		CALL F.READ(FN.TABLA.COLECTOR,ID.COLECTOR,RS.COLECTOR,F.TABLA.COLECTOR,COLECTOR.ERR)
		NOMBRE.COLECTOR = RS.COLECTOR<EB.CL.NOMBRE.COLECTOR>
	
		;*VALIDAMOS SI EL CANAL DE PAGO ES POR CAJA
		IF CANAL.PAGO EQ 'CAJA' THEN
			;*OBTENEMOS EL DETALLE DE LA TRANSACCION EN TFS
			CALL F.READ(FN.TABLA.DET.TFS,ID.T24,RS.DET.TFS,F.TABLA.DET.TFS,DET.TFS.ERR)
			;*LISTA DE LAS FORMAS DE PAGO
			LST.FORMAS.PAGO   = RS.DET.TFS<TFS.TRANSACTION>
			CONTADOR.FRM.PAGO = 1
			
			LOOP
			REMOVE Y.SLV.TRX.TFS.COL FROM LST.FORMAS.PAGO SETTING POS
			WHILE Y.SLV.TRX.TFS.COL:POS
				
				CANTIDAD = 1
				COLECTOR = NOMBRE.COLECTOR
				FECHA.HR = RS.DET.TFS<TFS.DATE.TIME>
				MONTO.FR = RS.DET.TFS<TFS.AMOUNT.DR,CONTADOR.FRM.PAGO>
				AGEN.FRM = NOMBRE.AGENCIA
				USR      = USUARIO.TRX
				REF.FRM  = ID.TRX.COL
				NO.CONF  = NO.CONF.PEX
				NPE.FRM  = NPE.TRX
				FRM.PG   = RS.DET.TFS<TFS.TRANSACTION,CONTADOR.FRM.PAGO>
			
				;*OBTENEMOS LA CUENTA A LA QUE FUE CARGADO EL SERVICIO
				CUENTA.DEBITO.TFS = RS.DET.TFS<TFS.PRIMARY.ACCOUNT>
	
				GOSUB GET.NAME.ACCOUNT
			
				;*OBTENEMOS EL TIPO DE CUENTA A)AHORO B)CORRIENTE
				CALL SLV.UTIL.GET.TITLE.PROD.ACCOUNT(NOMBRE.CUENTA.TFS)
				
				;*OBTENEMOS LA DESCRIPCION DE LA FORMA DE PAGO.
				CALL F.READ(FN.TFS.TRX,FRM.PG,RS.FRM.PG,F.TFS.TRX,TFS.TRX.ERR)
				FRM.DESC  = RS.FRM.PG<TFS.TXN.DESCRIPTION>
				ESTADO.PG = "PAGADO"
				STD.OUT = ID.TRX.COL:"*":CANTIDAD:"*":COLECTOR:"*":FECHA.HR:"*":MONTO.FR:"*":AGEN.FRM:"*":USR:"*":REF.FRM:"*":NO.CONF:"*":NPE.FRM:"*":FRM.DESC:"*":CUENTA.DEBITO.TFS:"*":NOMBRE.CUENTA:"*":ESTADO.PG
				A.INFO<-1> = STD.OUT
				
				CONTADOR.FRM.PAGO++
			REPEAT
		END ;*FIN DE IF DONDE SE VALIDA EL CANAL DE PAGO
	ELSE IF CANAL.PAGO NE '' THEN
		;*OBTENEMOS EL DETALLE DE LA TRANSACCION EN FT
		CALL F.READ(FN.TABLA.DET.FT,ID.T24,RS.DET.FT,F.TABLA.DET.FT,DET.FT.ERR)
			IF DET.FT.ERR EQ '' THEN
				CANTIDAD.FT = 1
				COLECTOR    = NOMBRE.COLECTOR
				FECHA.HR    = RS.DET.FT<FT.DATE.TIME>
				MONTO.FR    = RS.PAGO.COL<EB.TR.CO.MONTO>
				AGEN.FRM    = NOMBRE.AGENCIA
				USR         = USUARIO.TRX
				REF.FRM     = ID.TRX.COL
				NO.CONF     = NO.CONF.PEX
				NPE.FRM     = NPE.TRX
				FRM.PG      = RS.DET.FT<FT.TRANSACTION.TYPE>
				CUENTA.DEBITO.FT = RS.DET.FT<FT.DEBIT.ACCT.NO>
				
	
				;*OBTENEMOS LA CUENTA A LA QUE FUE CARGADO EL SERVICIO.
				GOSUB GET.NAME.ACCOUNT
				
			END
			ELSE
				;*PROCEDEMOS A CONSULTAR EL HISTORICO DE FT
				
				CALL EB.READ.HISTORY.REC(F.FUNDS.TRANSFER$HIS,ID.T24,R.FUNDS.TRANSFER,Y.ERR)
				CANTIDAD.FT = 1
				COLECTOR    = NOMBRE.COLECTOR
				FECHA.HR    = R.FUNDS.TRANSFER<FT.DATE.TIME>
				MONTO.FR    = RS.PAGO.COL<EB.TR.CO.MONTO>
				AGEN.FRM    = NOMBRE.AGENCIA
				USR         = USUARIO.TRX
				REF.FRM     = ID.TRX.COL
				NO.CONF     = NO.CONF.PEX
				NPE.FRM     = NPE.TRX
				FRM.PG      = R.FUNDS.TRANSFER<FT.TRANSACTION.TYPE>
				CUENTA.DEBITO.FT = R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO>
				
				
				
				;*OBTENEMOS LA CUENTA A LA QUE FUE CARGADO EL SERVICIO.
				GOSUB GET.NAME.ACCOUNT
	                   
			END
	
		;*FRM.PG = 'AC64'
		CALL F.READ(FN.TF.TYPE.CON,FRM.PG,RS.FT.TYP.CON,F.TF.TYPE.CON,ERR.FT.TYPE.CON)
		FORMA.PAGO.FT = RS.FT.TYP.CON<FT6.DESCRIPTION,2>
		
		STD.OUT = ID.TRX.COL:"*":CANTIDAD.FT:"*":COLECTOR:"*":FECHA.HR:"*":MONTO.FR:"*":AGEN.FRM:"*":USR:"*":REF.FRM:"*":NO.CONF:"*":NPE.FRM:"*":FORMA.PAGO.FT:"*":CUENTA.DEBITO.FT :"*":NOMBRE.CUENTA:"*":ESTADO.PG
		A.INFO<-1> = STD.OUT
		END ;* FIN DE ELSE DONDE SE VALIDA EL CANAL DE PAGO
	NEXT I
END ;* FIN DE ELSE DONDE SE VALIDA SI EL COLECTOR ES IGUAL AL DE PUNTO EXPRESS
RETURN






GET.NAME.ACCOUNT:

IF CANAL.PAGO EQ 'CAJA' THEN
CUENTA = CUENTA.DEBITO.TFS
END
ELSE
CUENTA = CUENTA.DEBITO.FT
END

CALL SLV.UTIL.GET.TIPO.CTA(CUENTA)

NOMBRE.CUENTA = ''

IF CUENTA EQ '0' THEN
NOMBRE.CUENTA = 'CUENTA DE AHORRO'
END
ELSE
NOMBRE.CUENTA = 'CUENTA CORRIENTE'
END

RETURN





VALIDACION.COLECTOR:
RETURN

END
