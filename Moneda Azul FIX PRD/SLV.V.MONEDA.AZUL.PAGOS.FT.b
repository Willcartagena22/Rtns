*-----------------------------------------------------------------------------
* <Rating>-43</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.MONEDA.AZUL.PAGOS.FT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* Modification Id          Date        Modified By              Description
* 20190627            27 Jun 2019     	vburgos       Creacion - rtn para sumar a contadores diarios y mensuales MonedaAzul desde canales ATM y PX
* 20190829			  29 Ago 2019       vburgos		  Adicionando MonedAzul Masiva	
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.PAGOS.MONEDA.AZUL
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.AC.LOCKED.EVENTS
$INSERT I_F.EB.SLV.MASTER.MONEDA.AZUL
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

INIT:
	 FN.MONEDA.AZUL	='F.EB.SLV.PAGOS.MONEDA.AZUL'
	 F.MONEDA.AZUL	=''
	 FN_FT 			= 'F.FUNDS.TRANSFER'
	 F_FT 			= ''
	 FN.AC.LCK.HIS  = 'F.AC.LOCKED.EVENTS$HIS'
	 F.AC.LCK.HIS	=''
	 FN.MASTER 		= 'F.EB.SLV.MASTER.MONEDA.AZUL'
	 F.MASTER 		= ''
RETURN

OPENFILE:
	CALL OPF(FN.MONEDA.AZUL,F.MONEDA.AZUL)
	CALL OPF(FN_FT, F_FT)
	CALL OPF(FN.AC.LCK.HIS, F.AC.LCK.HIS)
	CALL OPF(FN.MASTER, F.MASTER)
RETURN

PROCESS:
	ID_TNX 			= ID.NEW
	
	CUSTOMER= R.NEW(FT.DEBIT.CUSTOMER)
	MONTO= R.NEW(FT.DEBIT.AMOUNT)
	RESERVA = R.NEW(FT.ORDERING.CUST)<1,1>
	CALL GET.LOC.REF('FUNDS.TRANSFER','LF.DOC.CLIEN.EX', DOC.CLIEN.EX)
	DOCUMENTO = R.NEW(FT.LOCAL.REF)<1,DOC.CLIEN.EX>
	
	;*RESERVA = 'ACLK1832702438'
	Y.MAS = SUBSTRINGS(RESERVA,0,6)

	IF Y.MAS = 'MNDAZL' THEN 						
		
		Y.MASTER.ID = RESERVA	 	
		CALL F.READ(FN.MASTER,Y.MASTER.ID,R.MASTER,F.MASTER,MASTER.ERR)
		
		FECHA = R.MASTER<EB.SLV44.FECHA.ULT.AUT>
		

		RESERVA = R.NEW(FT.ORDERING.CUST)<1,1> : '.' : R.NEW(FT.ORDERING.CUST)<1,2>

	END
	ELSE
		CALL F.READ.HISTORY(FN.AC.LCK.HIS,RESERVA,R.AC.LCK.HIS,F.AC.LCK.HIS,ERR)
		FECHA = R.AC.LCK.HIS<AC.LCK.FROM.DATE>
	
	END
	
	
	;*FECHA=TODAY
	
	R.MON<EB.PMA.CUSTOMER>=CUSTOMER
	R.MON<EB.PMA.DATE.TIME>=TODAY
	R.MON<EB.PMA.FECHA.PAGO>=FECHA
	R.MON<EB.PMA.MONTO>=MONTO
	R.MON<EB.PMA.INPUTTER> = OPERATOR
	R.MON<EB.PMA.AUTHORISER>= OPERATOR
	R.MON<EB.PMA.CURR.NO> += 1
	R.MON<EB.PMA.RESERVA>=RESERVA   
	
	;*Actualizar contador de moneda azul beneficiario
	;*CALL SLV.I.UPD.CONT.TMMA(DOCUMENTO, MONTO) 

	
	CALL F.WRITE (FN.MONEDA.AZUL,ID_TNX,R.MON)
RETURN

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    ;*DIR.NAME='C:\Users\rramos\Documents\Temp'
    R.ID   = 'CONSULTA.MONEDA.PE.':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
 END
    CLOSESEQ SEQ.PTR
    
RETURN

END
