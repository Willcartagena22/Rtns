*-----------------------------------------------------------------------------
* <Rating>736</Rating>
*-----------------------------------------------------------------------------
* Nombre:      SLV.UTIL.VALID.FECHA.FORMAT.b
* Descripcion: Rutina de validacion de fecha. Verifica que un valor fecha ha
*              sido correctamente ingresada.
*              Retorno:  1 - Fecha Correcta
*                        0 - Fecha Incorrecta
* Parametros:  P.FECHA  - Fecha a validar
*              P.FORMAT - Formato en el cualse valida la fecha 
*                         'yyyymmdd' 
*                         'ddmmyyyy'
*                         'mmddyyyy'  
*-----------------------------------------------------------------------------
* Version		Autor			Fecha			Comentario
*-----------------------------------------------------------------------------
* 1.0			CMonterrosa		20082017		Version inicial
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.UTIL.VALID.FECHA.FORMAT(P.FECHA, P.FORMAT, P.RESULT)
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS
    RETURN


INIT:
*-----------------------------------------------------------------------------	
	;*Declaracion de variables de trabajo a utilizar
    ;*-------------------------------------------------
    Y.MONTH30 = "04" : FM : "06" : FM : "09" : FM : "11"
    Y.MONTH31 = "01" : FM : "03" : FM : "05" : FM : "07" : FM : "08"  : FM : "10" : FM : "12"
    Y.DATE.OK = 1
    Y.FECHA   = TRIM(P.FECHA)
		
RETURN
*-----------------------------------------------------------------------------	
*FIN - INIT
*-----------------------------------------------------------------------------

PROCESS:
*-----------------------------------------------------------------------------
    BEGIN CASE
       CASE P.FORMAT = 'yyyymmdd'
          IF LEN(Y.FECHA) NE 8 THEN
              Y.DATE.OK = 0
          END
          ELSE
             Y.YEAR  = SUBSTRINGS(Y.FECHA,1,4)
             Y.MONTH = RIGHT('00':SUBSTRINGS(Y.FECHA,5,2),2)
             Y.DAY   = RIGHT('00':SUBSTRINGS(Y.FECHA,7,2),2)
          END          
     
       CASE P.FORMAT = 'ddmmyyyy'
          IF LEN(Y.FECHA) NE 8 THEN
              Y.DATE.OK = 0
          END
          ELSE
             Y.DAY   = RIGHT('00':SUBSTRINGS(Y.FECHA,1,2),2)          
             Y.MONTH = RIGHT('00':SUBSTRINGS(Y.FECHA,3,2),2)
             Y.YEAR  = SUBSTRINGS(Y.FECHA,5,4)
          END
             
          
       CASE P.FORMAT = 'mmddyyyy'
          IF LEN(Y.FECHA) NE 8 THEN
             Y.DATE.OK = 0
          END
          ELSE
             Y.MONTH = RIGHT('00':SUBSTRINGS(Y.FECHA,1,2),2)
             Y.DAY   = RIGHT('00':SUBSTRINGS(Y.FECHA,3,2),2)                    
             Y.YEAR  = SUBSTRINGS(Y.FECHA,5,4)
          END
              
       
       CASE P.FORMAT = 'dd/mm/yyyy'
          IF LEN(Y.FECHA) NE 10 THEN
              Y.DATE.OK = 0
          END
          ELSE
             Y.S1 = SUBSTRINGS(Y.FECHA,3,1)
             Y.S2 = SUBSTRINGS(Y.FECHA,6,1)
             IF Y.S1 NE '/' AND Y.S2 NE '/' THEN
                Y.DATE.OK = 0
             END
             ELSE
                Y.DAY   = RIGHT('00':SUBSTRINGS(Y.FECHA,1,2),2)          
                Y.MONTH = RIGHT('00':SUBSTRINGS(Y.FECHA,4,2),2)
                Y.YEAR  = SUBSTRINGS(Y.FECHA,7,4)
             END
          END
                              
       CASE P.FORMAT = 'yyyy/mm/dd'
          IF LEN(Y.FECHA) NE 10 THEN
              Y.DATE.OK = 0
          END
          ELSE
             Y.S1 = SUBSTRINGS(Y.FECHA,5,1)
             Y.S2 = SUBSTRINGS(Y.FECHA,8,1)
             IF Y.S1 NE '/' AND Y.S2 NE '/' THEN
                Y.DATE.OK = 0
             END
             ELSE
                Y.YEAR  = SUBSTRINGS(Y.FECHA,1,4)
                Y.MONTH = RIGHT('00':SUBSTRINGS(Y.FECHA,6,2),2)
                Y.DAY   = RIGHT('00':SUBSTRINGS(Y.FECHA,9,2),2)                                    
             END 
          END                       
        
       CASE P.FORMAT = 'mm/dd/yyyy'
          IF LEN(Y.FECHA) NE 10 THEN
              Y.DATE.OK = 0
          END
          ELSE
             Y.S1 = SUBSTRINGS(Y.FECHA,3,1)
             Y.S2 = SUBSTRINGS(Y.FECHA,6,1)
             IF Y.S1 NE '/' AND Y.S2 NE '/' THEN
                Y.DATE.OK = 0
             END
             ELSE
                Y.MONTH = RIGHT('00':SUBSTRINGS(Y.FECHA,1,2),2)
                Y.DAY   = RIGHT('00':SUBSTRINGS(Y.FECHA,4,2),2)
                Y.YEAR  = SUBSTRINGS(Y.FECHA,7,4)                                                              
             END
          END                          
    END CASE
    
    ;*----------------------------------------------------------
    ;* VALIDACION DE CAMPOS DE LA FECHA
    ;*----------------------------------------------------------
    
    IF Y.DATE.OK EQ 1 THEN
       ;*VERIFICACION DE AÑO
       IF Y.YEAR LT 1 OR Y.YEAR GT 9999 THEN
          Y.DATE.OK = 0
       END
       ELSE
          ;*VERIFICACION DE MES
          IF Y.MONTH LT 1 OR Y.MONTH GT 12 THEN
             Y.DATE.OK = 0       
          END
          ELSE
             ;*MAX DIA EN EL MES
             FINDSTR Y.MONTH IN Y.MONTH30 SETTING Ap, Vp THEN
                Y.DAY.MAX = 30    
             END
             ELSE
                FINDSTR Y.MONTH IN Y.MONTH31 SETTING Ap, Vp THEN
                   Y.DAY.MAX = 31
                END             
             END    
             IF Y.MONTH EQ '02' THEN
                ;*VALIDAR SI ES AÑO BICIESTO          
                IF ((MOD(Y.YEAR, 4) EQ 0) AND (MOD(Y.YEAR, 100) NE 0) OR (MOD(Y.YEAR, 400) EQ 0)) THEN
                   Y.DAY.MAX = 29
                END
                ELSE
                   Y.DAY.MAX = 28
                END          
             END          
             ;*VERIFICACION DE DIA
             IF Y.DAY LT 1 OR Y.DAY GT Y.DAY.MAX THEN
                Y.DATE.OK = 0
             END
          END   
       END
    END
    
    P.RESULT = Y.DATE.OK
    P.FECHA  = Y.FECHA
 	
RETURN
*-----------------------------------------------------------------------------
* FIN - PROCESS
*-----------------------------------------------------------------------------
