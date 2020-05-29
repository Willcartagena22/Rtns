*-----------------------------------------------------------------------------
* <Rating>626</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.GEN.CSV.SSF
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_AA.LOCAL.COMMON  
$INSERT I_F.EB.SLV.LEND.REC.SSF
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB PROCESS
RETURN

INIT:
	FN.LEND.SSF = 'FBNK.EB.SLV.LEND.REC.SSF'
	F.LEND.SSF  = ''
	CALL OPF(FN.LEND.SSF, F.LEND.SSF)
	
	DIR.NAME    = 'SIF.OUT'
	EndLine     = '|'
	MNT.REC.INT = 0.0
	MNT.OTROS   = 0.0
	J           = 0
	PAGO.DACION = ''
	FE.INI      = '' 
	FE.FINAL    = ''
RETURN

PROCESS:
	STMT.LEND = "SELECT ":FN.LEND.SSF	
	CALL EB.READLIST(STMT.LEND, LIST.IDS, '', NO.RECS, Y.LEND.ERR)
	CRT 'NO.RECS>>':NO.RECS
	FOR MR = 1 TO NO.RECS
		CALL F.READ(FN.LEND.SSF, LIST.IDS<MR>, R.LEND, F.LEND.SSF, Y.ERR2)
		CRT 'MNT.RMES.CPROC':R.LEND<EB.SLV76.MNT.RMES.CPROC>
		CRT 'CTA.ESTADO':R.LEND<EB.SLV76.CTA.ESTADO>
		IF  ((R.LEND<EB.SLV76.MNT.RMES.CPROC> GE 0) OR (R.LEND<EB.SLV76.CTA.ESTADO> EQ 1)) THEN
			J = J + 1
			STR.ARR  = R.LEND<EB.SLV76.NO.NIT>                         : ";"  ;*numero_identificador_persona
			STR.ARR := R.LEND<EB.SLV76.NO.REFERENCIA>                  : ";"  ;*num_referencia
			STR.ARR := R.LEND<EB.SLV76.COD.CARTERA>                    : ";"  ;*cod_cartera
			STR.ARR := R.LEND<EB.SLV76.COD.ACTIVO>                     : ";"  ;*cod_activo
			
			IF (R.LEND<EB.SLV76.CTA.ESTADO> EQ '5') AND (R.LEND<EB.SLV76.MNT.RMES.CPROC> GT 0) THEN
				STR.ARR := R.LEND<EB.SLV76.MNT.RMES.CPROC>             : ";"  ;*monto_recuperado_mes
			END ELSE
				STR.ARR := R.LEND<EB.SLV76.MNT.REC.MES>                : ";"
			END
			
			STR.ARR := R.LEND<EB.SLV76.MNT.REC.MES.CAP>                : ";"  ;*monto_recuperado_mes_capital
			
			MNT.REC.INT = (R.LEND<EB.SLV76.MNT.RMES.INTV> + R.LEND<EB.SLV76.MNT.RMES.INTC>)
			STR.ARR := MNT.REC.INT                                     : ";"  ;*monto_recuperado_mes_interes
			
			MNT.OTROS = (R.LEND<EB.SLV76.MNT.RMES.SEGD> + R.LEND<EB.SLV76.MNT.RMES.SEGDI> + R.LEND<EB.SLV76.MNT.RMES.SEGV> + R.LEND<EB.SLV76.MNT.RMES.CPROC> + R.LEND<EB.SLV76.MNT.RMES.INTM>)
			STR.ARR := MNT.OTROS                                       : ";"  ;*monto_recuperado_mes_otros
			
			IF (R.LEND<EB.SLV76.CTA.ESTADO> EQ '5') AND (R.LEND<EB.SLV76.MNT.RMES.CPROC> GT 0) THEN
				PAGO.DACION = '3'
			END ELSE
				PAGO.DACION = R.LEND<EB.SLV76.FRM.REC.MES>
				IF R.LEND<EB.SLV76.REC.PAG.DACION> EQ 'SI' THEN
					PAGO.DACION = '2'
				END
				
				IF R.LEND<EB.SLV76.REC.PAG.DACION> EQ 'NO' THEN
					PAGO.DACION = '1'
				END
			END
			
			STR.ARR := PAGO.DACION                                     : ";"  ;*forma_recuperacion_mes
			
			FE.INI   = R.LEND<EB.SLV76.FCH.REC.MES>
			FE.FINAL = FE.INI[1,4]:'-':FE.INI[5,2]:'-':FE.INI[7,2]
			STR.ARR := FE.FINAL                                        : ";"  ;*fecha_recuperacion_mes
CRT 'VENT.CART>>':R.LEND<EB.SLV76.NOM.VENT.CART>
																			  ;*origen_recuperacion	
			IF (R.LEND<EB.SLV76.CTA.ESTADO> EQ '5') AND (R.LEND<EB.SLV76.MNT.RMES.CPROC> GT 0) THEN
				STR.ARR := '1'                                             : ";"
			END ELSE
				IF R.LEND<EB.SLV76.REC.PAG.DACION> EQ 'SI' THEN
					STR.ARR := '5'                                         : ";"  
				END ELSE

					IF R.LEND<EB.SLV76.NOM.VENT.CART> EQ '0' THEN          
						STR.ARR := R.LEND<EB.SLV76.ORI.REC.MES>            : ";"
					END ELSE
						STR.ARR := '4'                                      :";"
					END
			
				END
			END
						
			IF R.LEND<EB.SLV76.NOM.VENT.CART> EQ '0' THEN                     ;*nombre_venta_cartera
				STR.ARR := ''
			END ELSE
				STR.ARR := R.LEND<EB.SLV76.NOM.VENT.CART>
			END

			STR.ARR := EndLine
			ARRAY.DATA<-1> = STR.ARR
		END
		CRT 'STR.ARR>>':STR.ARR
		STR.ARR = ''
	NEXT MR
	
	;*--Generando Archivos		
	NAME.FILE = '_recuperacion.csv'

	;*Eliminando archivo existente
	DELETESEQ DIR.NAME, NAME.FILE THEN
	END  
	
	;* Abriendo archivo para escritura
	OPENSEQ DIR.NAME, NAME.FILE TO SEQ.PTR THEN
	WEOFSEQ NAME.FILE
	END
	
	FOR LM = 1 TO J
	    ;*Escribiendo
	    IF LM<J THEN
	  		WRITESEQ ARRAY.DATA<LM> ON SEQ.PTR THEN  ;*con retorno de carro
	  		END 
	  	END

    	IF LM=J THEN
        	WRITEBLK ARRAY.DATA<LM> ON SEQ.PTR THEN ;*sin retorno de carro
    		END
    	END
	NEXT LM	
	CLOSESEQ SEQ.PTR

RETURN
END
