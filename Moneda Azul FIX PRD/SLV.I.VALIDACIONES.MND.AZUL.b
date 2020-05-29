*-----------------------------------------------------------------------------
* <Rating>-57</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.VALIDACIONES.MND.AZUL 
*-----------------------------------------------------------------------------
* Nombre: SLV.I.VALIDACIONES.MND.AZUL
* Descripcion: Rutina encargada de validar Moneda Azul Masiva - Banca Empresas
*----------------------------------------------------------------------------------------------------
* Version	Autor		Fecha		Comentario 
*----------------------------------------------------------------------------------------------------
* 1.0		iTurcios	29.03.19	Version inicial
* Jonas		15.08.2019	Modificar días de vigencia de token de acuerdo a parametro
*						de días hábiles o calendario.
*----------------------------------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE  
$INSERT I_System
$INSERT I_GTS.COMMON
$INSERT I_F.EB.SLV.MASTER.MONEDA.AZUL 
$INSERT I_F.EB.SLV.KEYS.PARAMS
*-----------------------------------------------------------------------------
GOSUB INIT   
GOSUB PROCESS
RETURN

INIT:
	FN.KEYS = 'F.EB.SLV.KEYS.PARAMS'
	F.KEYS	= ''
	CALL OPF(FN.KEYS, F.KEYS)

	FN.MASTER = 'F.EB.SLV.MASTER.MONEDA.AZUL$NAU'
	F.MASTER  = ''
	CALL OPF(FN.MASTER, F.MASTER)
	
	;*Constantes.
	EQU EXT.CUSTOMER TO 'EXT.CUSTOMER'
	EQU ID.TOKEN	TO 'TMMA'
	
	;*Variables de entrada.
	Y.ID.PJ = R.NEW(EB.SLV44.ID.CUSTOMER)
	Y.ID.PN = System.getVariable(EXT.CUSTOMER)	 
	Y.MONTO = R.NEW(EB.SLV44.MONTO)
	Y.IVA   = R.NEW(EB.SLV44.IVA)
	Y.COMISION = R.NEW(EB.SLV44.COMISION)
	Y.TOTAL = Y.MONTO + Y.IVA + Y.COMISION
	Y.CUENTA = R.NEW(EB.SLV44.CUENTA)
	Y.GROUPS = ''
	
	;*Debug
*	Y.ID.PJ = '101296' 
RETURN
 
PROCESS:
	;*Leyendo signatories escritos en master, antes de la firma.
	CALL F.READ(FN.MASTER, ID.NEW, REC.MASTER, F.MASTER, ERR.MASTER)
	Y.GROUPS = REC.MASTER<EB.SLV44.GRUPO.MNDT>
	
	;*Validando Mandates
	CALL SLV.UTIL.CHECK.MANDATES(Y.ID.PJ, Y.ID.PN, Y.MONTO, Y.GROUPS, OUT.VER.REG, OUT.NO.OF.AUT, OUT.GRUPO)
		
	R.NEW(EB.SLV44.RES.VALIDACIONES) = OUT.VER.REG : "|" : OUT.NO.OF.AUT : "|" : OUT.GRUPO
	
	;*Sumando dias a ultima fecha autorizacion segun parametria.
	GOSUB ADD.N.DAYS ;*Salida en Y.FECHA.FIN
	R.NEW(EB.SLV44.FECHA.FIN) = Y.FECHA.FIN
	
	IF OUT.NO.OF.AUT EQ 0 AND OFS$OPERATION EQ 'PROCESS' THEN ;*Correr siguientes validaciones solo en commit y cuando sea la ultima autorizacion	
		;*Validacion estado y restricciones para cuenta debito.
		CALL SLV.UTIL.VAL.CUENTA(Y.CUENTA, OUT.FLAG.CTA)

		IF OUT.FLAG.CTA EQ 0 THEN
			E = 'AI-MND.AZL.CTA.INVALIDA' 
			CALL ERR 
		END
		
		;*Validando Monto Limite.
		CALL SLV.V.MNT.LIMITE.MND.AZL(Y.ID.PJ, Y.MONTO, OUT.FLAG)

		IF OUT.FLAG EQ 0 THEN
			E = 'AI-PROTECTION.LIMIT' 
			CALL ERR
		END
		
		;*Validando fondos insuficientes.
		CALL SLV.UTIL.GET.AVAILABLE(Y.CUENTA, OUT.AVAILABLE)
		
		IF OUT.AVAILABLE LT Y.TOTAL THEN
			E = 'EB-SLV.AMT.ER': FM : Y.CUENTA
			CALL ERR		
		END				 
	END	
			
	;*Acciones en todos los commit de la version, no importando autorizaciones.
	IF OFS$OPERATION EQ 'PROCESS' THEN			
		;*Limpiando variable de resultado de validaciones
		R.NEW(EB.SLV44.RES.VALIDACIONES) = ""		
	END
RETURN

ADD.N.DAYS: 
	Y.FECHA.FIN = ""
	;*Obtener dias a sumar
	CALL F.READ(FN.KEYS, 'SLV.TOKEN', R.KEYS, F.KEYS, E.KEYS)
	FIND ID.TOKEN IN R.KEYS<EB.SLV18.PARAM.ID> SETTING Ap, Vp THEN
		Y.TIME.TOKEN   = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 3) ;*Duracion en Dias
		;*Según posición de parametros de EB.SLV.KEYS.PARAMS>SLV.TOKEN para MAzul Masivo o Individual
		Y.DIAS.HAB.CAL = FIELD(R.KEYS<EB.SLV18.VALOR><Ap, Vp>, SM, 6) ;*Días habiles o calendario (W o C)
	END
	Y.TIME.TOKEN := Y.DIAS.HAB.CAL
	;*Anadiedo N dias a TODAY para calculo de fecha fin del bloqueo
	Y.DATE = TODAY
   	CALL CDT("SV05": Y.DATE[1,4],Y.DATE,"+":Y.TIME.TOKEN)
    ;*Se resta un día a PROCESS.DATE ya que la función CDT suma los días del parámetro definido a la fecha actual
    Y.UN.DIA = '-1': Y.DIAS.HAB.CAL
    CALL CDT("SV05": Y.DATE[1,4],Y.DATE,Y.UN.DIA)

   	Y.FECHA.FIN = Y.DATE
   	
RETURN

;*Archivo para Revisión de Errores
WRITE_LOG_FILE:
	DIR.NAME = 'SIF.OUT'
	R.ID = 'LOG.MONEDA.AZUL' : '.txt'
 
	OPENSEQ DIR.NAME, R.ID TO SEQ.PTR
		WRITESEQ Y.TXT APPEND TO SEQ.PTR THEN
		END
	CLOSESEQ SEQ.PTR 
RETURN

END
