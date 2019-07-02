       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARSERJS2.
       AUTHOR. LOCOMOTORA.
       INSTALLATION. DICIEMBRE-DE-2018.
       DATE-WRITTEN. DICIEMBRE-DE-2018.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
            COPY "sysinfo.wrk".
            COPY "wsubx01.wrk". 
       77  ws-borrame pic x.
       77 WS-FIN PIC X. 
          88 WS-FIN-SI VALUE "S".
          88 WS-FIN-NO VALUE "N".
       77 WS-EN PIC X. 
          88 WS-EN-HDR  VALUE "H".
          88 WS-EN-BODY VALUE "B".
       77 WS-LINEA-RTA PIC X(4096).
       77 STRING-PTR PIC 9(4).
       77 STRING-PTR1 PIC 9(4).
       77 WS-RECURSO  PIC X(1024) VALUE SPACES.
       77 WS-RECURSO2 PIC X(1024)  VALUE SPACES.
       77 WS-RECURSO3 PIC X(1024)  VALUE SPACES.
       77  TCP-PORT             PIC 9999 VALUE 5012.
       77  WSOPENX25            PIC X VALUE "N".
       77  WSANULAVTA           PIC X VALUE "N".
       77  AUX-C-V-X25          PIC X(20).
       01  NMAX-BUF-READS.
               03  MAX-BUF-READS  PIC XXXX.
               03 FILLER PIC X VALUE LOW-VALUE.
       01  NMAX-BUF-WRITE.
               03 MAX-BUF-WRITE  PIC XXXX.
               03 FILLER PIC X VALUE LOW-VALUE.
       01  NCON-LOG.
               03 CON-LOG        PIC X.
               03 FILLER PIC X VALUE LOW-VALUE.
       01  NFILE-CNF.
               03 FILE-CNF       PIC X(50).
               03 FILLER PIC X VALUE LOW-VALUE.
       01  NHTONS.
               03  HTONS          PIC XXXX.
               03 FILLER PIC X VALUE LOW-VALUE.
       01  WS-STOCKS.
           03 WS-SUCURS         PIC X(3).
           03 WS-CODART         PIC X(6).
           03 WS-STNORM         PIC S9(7).
           03 WS-FRAVEG         PIC S9(7).
           03 WS-TRANSF         PIC S9(7).
           03 WS-STANAL         PIC S9(7).
           03 WS-STOCK92        PIC S9(7).
           03 WS-STEXHI         PIC S9(7).
           03 WS-STOSEN         PIC S9(7).
           03 WS-STEXPE         PIC S9(7).
           03 WS-STKBUS         PIC S9(7).
       77  AUX-TIPO-DE-OPERACION-X25 PIC X.
       77  AUX-R-X25            PIC X(4096).
       77 HTTP-HEADER PIC X(4096) VALUE SPACES.
       77 HTTP-DATA   PIC X(4096) VALUE SPACES.
       
       
       01  R4096-X25.
           03 REGISTRO4096-X25     PIC X(4096).
           03 FILLER               PIC X VALUE LOW-VALUE.

           
       77 PTR-TAM-DATA PIC 9(4) VALUE ZEROS.
       77 PTR-TAM-HDR  PIC 9(4) VALUE ZEROS.
       77 PTR-TAM-4096 PIC 9(4) VALUE ZEROS.
       77 PTR-TAM-URL  PIC 9(4) VALUE ZEROS.
       77 WS-PARAM-URL PIC X(40) VALUE SPACES.
       77 WS-CONTA     PIC 9(4) VALUE ZEROS.
       77 WS-CONTA2    PIC 9(4) VALUE ZEROS.
       01 WS-URL-TBL.
          03 WS-URL  PIC X(512) OCCURS 2 TIMES.
       01 WS-URL2-TBL.
          03 WS-URL2 PIC X(512) OCCURS 2 TIMES.
      *LK-DATOS se usa para el envio de datos al gateway 
       01 WS-DATOS.
          03 WS-TABLA OCCURS 300 TIMES.
             05 WS-VARIABLE PIC X(40).
             05 WS-VALOR    PIC X(256).
             05 WS-TIPO     PIC X.
       01 WS-DES-RTA        PIC X(30). |*> Descr. de la respuesta 
       01 WS-COD-RTA        PIC 9(3).     |*> 200 409 ect"
       01 WS-IDTRANSACTION PIC X(36).
      *LK-DATOS se usa para recibir datos del gateway 
       01 WS-DATOS-RESP.
          03 WS-TABLA-RESP OCCURS 300 TIMES.
             05 WS-VAR-RESP    PIC X(40).
             05 WS-VAL-RESP    PIC X(256).
             05 WS-TIP-RESP    PIC X.
       77 WS-VALOR-AUX  PIC X(256).
       77 WS-VALOR-AUX2  PIC X(256).
       77 WS-TXT-ANT PIC X(256).
       77 WS-REEMPLAZO pic x(256).
       77 WS-INDICE PIC 999.
       77 WS-INDICE-TOT PIC 999V99.
       01 WS-EOF PIC X.
          88 WS-EOF-NO VALUE "N".
          88 WS-EOF-SI VALUE "S".
       77 WS-COMA PIC X. 
       77 WS-IND  PIC 9(4).
       77 WS-IND2 PIC 9(4).
       77 WS-IND3 PIC 9(4).
       77 WS-IND4 PIC 9(4).
       77 WS-IND5 PIC 9(4).
       77 WS-ANT PIC 9(4).
       77 COUNTER PIC 9(4).

       01 ws-tbl-conver.
          03 ws-ansi-cp1252 occurs 128 times.
             05 ws-ansi     pic x.
             05 ws-cp1252   pic x(3).
             05 ws-tam-1252 pic 9.

       77 FI-N-COD PIC 9(4) BINARY.
       77 FI-COD REDEFINES FI-N-COD PIC X(2).
       77 HEX-VALUE PIC X(4).
       77 W-HEX-VALUE PIC X(4).

       01 WS-AUX-Z8V99            PIC -Z(8).99.
       01 WS-AUX-98V99            PIC +++++++9.99.
       77 WS-DATA-INI PIC X(15).

       LINKAGE SECTION.
      *LK-DATOS se usa para el envio de datos al gateway y para recibir 
       01 LK-DATOS.
          03 LK-TABLA OCCURS 300 TIMES.
             05 LK-VARIABLE PIC X(40).
             05 LK-VALOR    PIC X(256).
             05 LK-TIPO     PIC X.
       01 LK-DES-RTA        PIC X(30). |*> Descr. de la respuesta 
       01 LK-COD-RTA        PIC 9(3).     |*> "E"=ERROR, " "=OK
       01 LK-IDTRANSACTION PIC X(36).
       01 LK-DATOS-RESP.
          03 LK-TABLA-RESP OCCURS 300 TIMES.
             05 LK-VAR-RESP    PIC X(40).
             05 LK-VAL-RESP    PIC X(256).
             05 LK-TIP-RESP    PIC X.
      *01 LK-PARAMS.
      *  03 LK-OPERACION      PIC X.|*> "V"=VEnta o "D"=Devolucion
      *  03 LK-SUC            PIC 9(3).
      *   03 LK-NRO-PRESU      PIC Z(9) BLANK WHEN ZERO.
      *   03 LK-NRO-FACTURA    PIC X(9).
      *   03 WS-COD-RTA        PIC X.|*> "V"=verdadero "F"=Falso
      *   03 WS-DES-RTA        PIC X(30). |*> Codigo de Rta Http 
      *
       PROCEDURE DIVISION USING LK-DATOS, LK-DES-RTA, LK-COD-RTA ,
                                LK-IDTRANSACTION, LK-DATOS-RESP.
      * PROCEDURE DIVISION.              
       DECLARATIVES.                                   
       INPUT-ERROR SECTION.                             
           USE AFTER STANDARD ERROR PROCEDURE ON INPUT. 
       0010-DECL.                                       
           EXIT.                                        
       I-O-ERROR SECTION.                               
           USE AFTER STANDARD ERROR PROCEDURE ON I-O.   
       0020-DECL.                                       
           EXIT.                                        
       OUTPUT-ERROR SECTION.                            
           USE AFTER STANDARD ERROR PROCEDURE ON OUTPUT.
       0030-DECL.                                       
           EXIT.                                        
       EXTEND-ERROR SECTION.                            
           USE AFTER STANDARD ERROR PROCEDURE ON EXTEND.
       0040-DECL.                                       
           EXIT.                                        
       END DECLARATIVES.                                
       COMIENZO SECTION.                                
       PGMA.
           MOVE 0 TO WS-AUX-98V99.
           INITIALIZE WS-COD-RTA.
           INITIALIZE WS-DES-RTA.
           MOVE SPACES TO WS-RECURSO.
      *     MOVE "10.6.11.66" TO NUMERO-X25. |*> IP jboss docker(58)
           ACCEPT NUMERO-X25 FROM ENVIRONMENT "IP_NODUM". |*> IP jboss
      *    ACCEPT TCP-PORT  FROM ENVIRONMENT "PORT_GO"
           MOVE 8082 TO TCP-PORT.
           MOVE TCP-PORT TO HTONS.
           CALL "C$NARG" USING NUM-ARGS.
           IF NUM-ARGS > 0
              MOVE LK-DATOS TO WS-DATOS
              IF NUM-ARGS > 3
                MOVE LK-IDTRANSACTION TO WS-IDTRANSACTION
              END-IF  
           ELSE
              PERFORM SIMULA-DATOS THRU F-SIMULA-DATOS
           END-IF.
           PERFORM PREPARO-DATOS THRU F-PREPARO-DATOS.
           PERFORM INICIALIZO-TCP THRU F-INICIALIZO-TCP.
           PERFORM CONSULTO-WS THRU F-CONSULTO-WS.
      *     PERFORM EVALUO-RESPUESTA THRU F-EVALUO-RESPUESTA.
       FIN-PGMA.

           IF NUM-ARGS > 0
              MOVE LK-DATOS TO WS-DATOS
              MOVE WS-COD-RTA TO LK-COD-RTA
              MOVE WS-DES-RTA TO LK-DES-RTA
              IF NUM-ARGS > 3
                MOVE WS-IDTRANSACTION TO LK-IDTRANSACTION
                MOVE WS-DATOS-RESP TO LK-DATOS-RESP
              END-IF  
      D    ELSE
      D       DISPLAY "NUM-ARGS: " NUM-ARGS
      D       DISPLAY "WS-COD-RTA: " WS-COD-RTA
      D       DISPLAY "WS-DES-RTA: " WS-DES-RTA
           END-IF.
           GOBACK.

       SIMULA-DATOS.
           DISPLAY "EJECUTA SIMULACION DE CIERRE Z?"
           ACCEPT WS-VALOR(1).
           IF WS-VALOR(1) NOT = "S" AND WS-VALOR(1) NOT = "s" 
             GO TO FIN-PGMA
           END-IF.
           INITIALIZE WS-DATOS.
           MOVE 1 TO WS-INDICE.
      *SERVICIO A USAR #https://documentacion-adn.development.fravega.com/v{version}/documentacion/{ipImpresora}/{codigoDocumento}
           MOVE "SERVICIO" TO  WS-VARIABLE(WS-INDICE).
           MOVE "RECORD"  TO  WS-VALOR(WS-INDICE).
           MOVE "S"        TO  WS-TIPO(WS-INDICE).
           IF WS-VARIABLE(WS-INDICE) = "SERVICIO" AND
                WS-VALOR(WS-INDICE) <> SPACES
                EVALUATE WS-VALOR(1)
                  WHEN "RECORD"
                    PERFORM IMPRPDF
                  WHEN "VTAEMPFAC"
                    PERFORM VTAEMPFAC
                  WHEN "VTAEMPDEV"
                    PERFORM VTAEMPDEV
                  WHEN OTHER
                    MOVE "ERROR: SERVICIO NO HABILITADO" TO WS-DES-RTA
                    MOVE 409 TO WS-COD-RTA
                    PERFORM ERRORES
                END-EVALUATE
           ELSE
             MOVE "ERROR: SERVICIO NO DECLARADO" TO WS-DES-RTA
             MOVE 409 TO WS-COD-RTA
             PERFORM ERRORES
           END-IF.
       F-SIMULA-DATOS.
           EXIT.
       IMPRPDF.
           COPY "imprpdf.cpy".
           .
       VTAEMPFAC.
           COPY  "vtaempfac.cpy".
           .
       VTAEMPDEV.
           COPY  "vtaempdev.cpy".
           .
      *
       CARGA-TBL-CP1252.       
           COPY "cp1252.cpy".
           .

       PREPARO-DATOS .
      *todos los datos de cada variable se terminan con low-values     
      * y para quitar el signo "+" de cada valor numerico
           perform CARGA-TBL-CP1252.
           MOVE 1 TO WS-INDICE.
           SET WS-EOF-NO TO TRUE.
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-EOF-SI
             IF WS-TIPO(WS-IND) = "9"
               MOVE 0 TO counter
               MOVE WS-VALOR(WS-IND) TO WS-VALOR-AUX
               INSPECT WS-VALOR-AUX TALLYING counter FOR LEADING SPACE
               MOVE WS-VALOR-AUX(counter + 1 :) TO WS-VALOR(WS-IND)

               MOVE 0 TO counter
               MOVE WS-VALOR(WS-IND) TO WS-VALOR-AUX
               INSPECT WS-VALOR-AUX TALLYING counter FOR LEADING "+"
               MOVE WS-VALOR-AUX(counter + 1 :) TO WS-VALOR(WS-IND)

      *AR:I - Se eliminan los ceros a la izquierda de los tipo "9"
               MOVE 0 TO counter
               MOVE WS-VALOR(WS-IND) TO WS-VALOR-AUX
               INSPECT WS-VALOR-AUX TALLYING counter FOR LEADING "0"
               MOVE WS-VALOR-AUX(counter + 1 :) TO WS-VALOR(WS-IND)
      *AR: si queda ".01" o vacio  rellena con "0"
               evaluate WS-VALOR(WS-IND)(1:1) 
                 when "." 
                   MOVE WS-VALOR(WS-IND) TO WS-VALOR-AUX
                   string "0" delimited by size, 
                          WS-VALOR-AUX delimited by size
                        into WS-VALOR(WS-IND) 
                   end-string     
                 when space 
                   MOVE "0" TO WS-VALOR(WS-IND)
               end-evaluate
      *AR:F 

             END-IF
      *se escapa las comillas dobles " con \"
             IF WS-TIPO(WS-IND) = "X"
               SET WS-FIN-NO TO TRUE
               move 1 to ws-ant STRING-PTR
               MOVE 0 TO ws-ind4
               MOVE SPACES TO WS-REEMPLAZO
               perform varying ws-ind2 from 1 by 1 until ws-ind2 > 256
                     or ws-fin-si
                 IF WS-VALOR(ws-ind)(ws-ind2:) = SPACES
                   SET WS-FIN-SI TO TRUE
                 END-IF
                 perform varying ws-ind5 from 1 by 1 until ws-ind5 > 128
                         OR WS-REEMPLAZO NOT = SPACES
                   if WS-VALOR(WS-IND)(ws-ind2:1) = ws-ansi(ws-ind5)
                     move ws-cp1252(ws-ind5) TO WS-REEMPLAZO
                     move ws-tam-1252(ws-ind5) to ws-ind3
                   else 
                     if WS-VALOR(WS-IND)(ws-ind2:1) = '"'
                       move '\"' to ws-reemplazo
                       move 2 to ws-ind3
                     end-if
                   end-if
                 end-perform
                 IF WS-REEMPLAZO NOT = SPACES
                   IF WS-IND2 > 1
                     |*> resto 2 porque 1 se sumo al salir del perform 
                     subtract 2 from ws-ind2 giving ws-ind4
                     STRING WS-VALOR(WS-IND)(ws-ant:ws-ind4)
                             delimited by size, 
                         ws-reemplazo delimited by spaces
                        into ws-valor-aux with pointer STRING-PTR
                     END-STRING
                   ELSE 
                     move 1 to ws-ind4
                     STRING ws-reemplazo delimited by spaces
                        into ws-valor-aux with pointer STRING-PTR
                      END-STRING
                   end-if
                   add 1 to  ws-ind2 giving ws-ant
      *AB"CD  
                 END-IF
                 MOVE spaces TO WS-REEMPLAZO
               end-perform
               if ws-ind4 > 0   |*> hubo reemplazo
                 STRING WS-VALOR(WS-IND)(ws-ant:)
                         delimited by size, 
                    into ws-valor-aux with pointer STRING-PTR
                 END-STRING
                 move spaces to WS-VALOR(WS-IND)
                 move ws-valor-aux to WS-VALOR(WS-IND)
               END-IF  
      *AR:F - GOLANG
             END-IF
             STRING WS-TABLA(WS-IND) DELIMITED BY SIZE
               INTO W-MENSAJE-LOG 
             END-STRING       
             CALL "logger" USING W-MENSAJE-LOG
             INSPECT WS-VARIABLE(WS-IND) 
                REPLACING TRAILING SPACES BY LOW-VALUES
             INSPECT WS-VALOR(WS-IND) 
                REPLACING TRAILING SPACES BY LOW-VALUES
             INSPECT WS-TIPO(WS-IND)
                REPLACING TRAILING SPACES BY LOW-VALUES
             IF WS-TABLA(WS-IND) = LOW-VALUES
               SET WS-EOF-SI TO TRUE
             END-IF
             MOVE SPACES TO W-MENSAJE-LOG
             ADD 1 TO WS-INDICE
           END-PERFORM.
        F-PREPARO-DATOS.
            EXIT.

       LEO-CHUNK.
           INITIALIZE W-MENSAJE-LOG.
           MOVE "-----LEO CHUNK RESPUESTA-----" TO W-MENSAJE-LOG.
           CALL "logger" USING W-MENSAJE-LOG.
           MOVE "R" TO TIPO-DE-OPERACION-X25.
           MOVE "4096" TO PARAM1-X25.
           PERFORM LOPERA-X25 THRU FOPERA-X25
           IF RETCODE-X25 < 0
              EVALUATE RETCODE-X25 
                WHEN -1 
                WHEN -9
                  STRING "ERROR TIMEOUT. COD.: ",
                         X25-RETCODE delimited by size
                    INTO WS-DES-RTA
                  END-STRING
                WHEN -3
                  STRING "ERROR DE CONEXION. COD.: ",
                         X25-RETCODE delimited by size
                    INTO WS-DES-RTA
                  END-STRING
                WHEN OTHER
                  STRING "ERROR COD.: ",
                         X25-RETCODE delimited by size
                    INTO WS-DES-RTA
                  END-STRING
               END-EVALUATE
               MOVE 409 TO WS-COD-RTA
               PERFORM ERRORES
           END-IF.
       
            
       CONSULTO-WS.
           INITIALIZE WS-DES-RTA.

           PERFORM TIPO-VTAEMP
      *Lee la respuesta
           PERFORM LEO-CHUNK.
           IF RETCODE-X25 < 0
             GO TO F-CONSULTO-WS
           END-IF.  
           PERFORM LOGUEO-CHUNK-DATA.
       
           INITIALIZE WS-LINEA-RTA.
           SET WS-FIN-NO TO TRUE.
           set WS-EN-HDR TO TRUE.
           MOVE 1 TO STRING-PTR.
           MOVE 0 TO WS-INDICE WS-IND4.
           MOVE SPACES TO REGISTRO4096-X25(RETCODE-X25 + 1:).
           PERFORM UNTIL STRING-PTR > RETCODE-X25 OR WS-FIN-SI
            INITIALIZE WS-LINEA-RTA
            MOVE 0 TO WS-IND
            UNSTRING REGISTRO4096-X25
             DELIMITED BY X"0D0A"
             INTO WS-LINEA-RTA WITH POINTER STRING-PTR
             ON OVERFLOW
                IF STRING-PTR > RETCODE-X25 THEN
                   SET WS-FIN-SI TO TRUE
                END-IF
                PERFORM EVALUO-RESPUESTA THRU F-EVALUO-RESPUESTA
             NOT ON OVERFLOW
                PERFORM EVALUO-RESPUESTA THRU F-EVALUO-RESPUESTA
            END-UNSTRING
            IF WS-COD-RTA = 200 
              IF STRING-PTR > RETCODE-X25 AND WS-FIN-NO
                PERFORM LEO-CHUNK
                IF RETCODE-X25 < 0
                  GO TO F-CONSULTO-WS
                END-IF
              END-IF
            ELSE
              IF WS-EN-BODY
                MOVE REGISTRO4096-X25(STRING-PTR:) TO WS-DATOS-RESP
                SET WS-FIN-SI TO TRUE
              END-IF
            END-IF
           END-PERFORM.
           PERFORM CERRAR-X25 THRU F-CERRAR-X25.
       F-CONSULTO-WS.
           EXIT.
           
       LOGUEO-CHUNK-DATA.
           INITIALIZE W-MENSAJE-LOG.
           if RETCODE-X25 < 257
             MOVE REGISTRO4096-X25(1:RETCODE-X25) TO W-MENSAJE-LOG
           else
             divide RETCODE-X25 into 256 
                    giving ws-ind remainder ws-ind2
             move 1 to ws-ind3       
             perform varying WS-INDICE from 1 by 1 
                     until WS-INDICE  > ws-ind
               INITIALIZE W-MENSAJE-LOG
               MOVE REGISTRO4096-X25(ws-ind3:256) TO W-MENSAJE-LOG  
               add 256 to ws-ind3
               CALL "logger" USING W-MENSAJE-LOG
             end-perform
             INITIALIZE W-MENSAJE-LOG
             MOVE REGISTRO4096-X25(ws-ind3:ws-ind2) TO W-MENSAJE-LOG
             CALL "logger" USING W-MENSAJE-LOG
           end-if.
           CALL "logger" USING W-MENSAJE-LOG
           .
       
       
       ARMA-CABECERA.
      *Arma la cabecera http
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND >= 
                   WS-INDICE - 1 
      *            OR ( WS-TIPO(WS-IND)(1:1) NOT = "S" 
      *            AND WS-TIPO(WS-IND)(1:1) NOT = "U" )
                   
             EVALUATE WS-TIPO(WS-IND)(1:1)
               WHEN "S"
                 PERFORM BUSCA-SERVICIO
               WHEN "U"
                 PERFORM ARMA-URL
               WHEN OTHER
                 CONTINUE
             END-EVALUATE
           END-PERFORM.
      
           MOVE 1 TO PTR-TAM-HDR  PTR-TAM-4096.
           MOVE SPACES TO REGISTRO4096-X25 HTTP-HEADER.
           IF WS-IDTRANSACTION NOT = SPACES AND 
              WS-IDTRANSACTION NOT = LOW-VALUES 
             STRING 'POST ' DELIMITED BY SIZE, 
               WS-RECURSO DELIMITED BY SPACE, 
               ' HTTP/1.1' DELIMITED BY SIZE, X"0D0A",
               'Content-Type: application/json; charset=windows-1252', 
               X"0D0A",
               'Transfer-Encoding: chunked' DELIMITED BY SIZE, X"0D0A",
               'Host: '  DELIMITED BY SIZE, 
               NUMERO-X25 DELIMITED BY SPACE,
               ':', 
               TCP-PORT  DELIMITED BY SIZE, X"0D0A",
               'X-Request-ID: ' , WS-IDTRANSACTION, X"0D0A",
               'User-Agent: Legados-HttpClient/1.1',X"0D0A"
                ,X"0D0A",'1',X"0D0A",'{'
                DELIMITED BY SIZE
                  INTO HTTP-HEADER  WITH POINTER PTR-TAM-HDR
             END-STRING
           ELSE
             STRING 'POST ' DELIMITED BY SIZE, 
               WS-RECURSO DELIMITED BY SPACE, 
               ' HTTP/1.1' DELIMITED BY SIZE, X"0D0A",
               'Content-Type: application/json; charset=windows-1252',
               X"0D0A",
               'Transfer-Encoding: chunked' DELIMITED BY SIZE, X"0D0A",
               'Host: '  DELIMITED BY SIZE, 
               NUMERO-X25 DELIMITED BY SPACE,
               ':', 
               TCP-PORT  DELIMITED BY SIZE, X"0D0A",
               'User-Agent: Legados-HttpClient/1.1',X"0D0A"
                ,X"0D0A",'1',X"0D0A",'{'
                DELIMITED BY SIZE
                  INTO HTTP-HEADER  WITH POINTER PTR-TAM-HDR
             END-STRING
           END-IF.
      *      DISPLAY "WS-RECURSO= " WS-RECURSO.
           SUBTRACT 1 FROM PTR-TAM-HDR.
           INSPECT HTTP-HEADER
                REPLACING TRAILING SPACES BY LOW-VALUES.
           STRING HTTP-HEADER DELIMITED BY LOW-VALUES
                 ,X"0D0A" DELIMITED BY SIZE
              INTO REGISTRO4096-X25 WITH POINTER PTR-TAM-4096
           END-STRING.
           MOVE "o" TO TIPO-DE-OPERACION-X25.
           MOVE "0000" TO PARAM1-X25.
           PERFORM LOPERA-X25 THRU FOPERA-X25

      *     MOVE 1 TO PTR-TAM-DATA PTR-TAM-4096.
      *    MOVE LOW-VALUES TO REGISTRO4096-X25 HTTP-DATA.
      *    MOVE '{' TO HTTP-DATA.
      *    INSPECT HTTP-DATA
      *         REPLACING TRAILING SPACES BY LOW-VALUES.
      *    STRING HTTP-DATA DELIMITED BY LOW-VALUES
      *          INTO HTTP-DATA WITH POINTER PTR-TAM-DATA
      *    END-STRING.
      *    SUBTRACT 1 FROM PTR-TAM-DATA.
      *    PERFORM ENVIO-CHUNK.
           .
       
           
       TIPO-VTAEMP.
           PERFORM ARMA-CABECERA.
      *Arma el body   
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND >= 
                   WS-INDICE - 1
      *I=inicio de { o [, 
      *F=fin de } o ], 
      *S=SERVICIO debe existir en variable de entorno, 
      *U=Parametro en URL secuencial, 
      *X=texto
      *9=numerico
      
             IF WS-TIPO(WS-IND + 1)  = "F" OR
                WS-TIPO(WS-IND + 1)  = "S" OR
                WS-TIPO(WS-IND + 1)  = "U" OR
                WS-TIPO(WS-IND + 1)  = LOW-VALUE
               MOVE " " TO WS-COMA
             ELSE
               MOVE "," TO WS-COMA
             END-IF
             EVALUATE WS-TIPO(WS-IND)(1:1)
               WHEN "S"
               WHEN "U"
                 CONTINUE
               WHEN "X"
                 PERFORM ARMA-BODY-X
                 PERFORM ENVIO-CHUNK
               WHEN "9"
                 PERFORM ARMA-BODY-9
                 PERFORM ENVIO-CHUNK
               WHEN "I"
                 PERFORM ARMA-BODY-I
                 PERFORM ENVIO-CHUNK
               WHEN "F"
                 PERFORM ARMA-BODY-F
                 PERFORM ENVIO-CHUNK
               WHEN OTHER
                 STRING "ERROR: TIPO DE DATO INCORRECTO"
                        delimited by size,
                        WS-TABLA(ws-ind) delimited by spaces
                        INTO  WS-DES-RTA
                 end-string       
                 MOVE 409 TO WS-COD-RTA
                 PERFORM ERRORES
             END-EVALUATE
           END-PERFORM.

           MOVE 1 TO PTR-TAM-DATA PTR-TAM-4096.
           MOVE LOW-VALUES TO REGISTRO4096-X25 HTTP-DATA.
           MOVE '}' TO HTTP-DATA.
           INSPECT HTTP-DATA
                  REPLACING TRAILING SPACES BY LOW-VALUES.
           PERFORM ENVIO-CHUNK.

           MOVE 1 TO PTR-TAM-DATA PTR-TAM-4096.
           MOVE LOW-VALUES TO REGISTRO4096-X25 HTTP-DATA.
           STRING "0", X"0D0A", X"0D0A" DELIMITED BY SIZE
                INTO REGISTRO4096-X25 WITH POINTER PTR-TAM-4096
           END-STRING.
           SUBTRACT 1 FROM PTR-TAM-4096.
           MOVE "W" TO TIPO-DE-OPERACION-X25.
           MOVE "0000" TO PARAM1-X25.
           PERFORM LOPERA-X25 THRU FOPERA-X25
           .

       ENVIO-CHUNK.
           MOVE 1 TO PTR-TAM-4096.
           MOVE LOW-VALUES TO REGISTRO4096-X25.
           MOVE PTR-TAM-DATA TO FI-N-COD.
           PERFORM PASO-A-HEX.  |*> CONVIERTE FI-N-COD A W-HEX-VALUE.
           STRING W-HEX-VALUE DELIMITED BY LOW-VALUES,
                  X"0D0A",
                  HTTP-DATA DELIMITED BY LOW-VALUES,
                  X"0D0A" DELIMITED BY SIZE
                INTO REGISTRO4096-X25 WITH POINTER PTR-TAM-4096
           END-STRING.
           SUBTRACT 1 FROM PTR-TAM-4096.
           MOVE "W" TO TIPO-DE-OPERACION-X25.
           MOVE "0000" TO PARAM1-X25.
           PERFORM LOPERA-X25 THRU FOPERA-X25.
           .

       ERRORES.
             MOVE SPACES TO W-MENSAJE-LOG.
             STRING  WS-DES-RTA , " Codigo: ",
                     WS-COD-RTA DELIMITED BY SIZE 
                     INTO W-MENSAJE-LOG 
             END-STRING.
             CALL "logger" USING W-MENSAJE-LOG.
             GO TO FIN-PGMA.
            .
      *Por ahora esta definido en una variable de entorno  
       BUSCA-SERVICIO.  
           ACCEPT WS-RECURSO FROM ENVIRONMENT WS-VALOR(WS-IND).

      *Re-escribe la variable WS-RECURSO 
       ARMA-URL.
           MOVE SPACES TO WS-URL-TBL WS-URL2-TBL.
      * SEPARA LA URL POR "{ o }"
           move 0 to ws-conta.
           perform varying ws-ind2 from 1 by 1 until ws-ind2 > 1024 or
                   ws-recurso(ws-ind2:) = low-values
             if ws-recurso(ws-ind2:1) = "{"
               move ws-ind2 to ws-conta
               move ws-recurso(1:ws-conta - 1) to ws-recurso2
               add 1024 to ws-ind2
             end-if
           end-perform.
           move 0 to ws-conta2.
           perform varying ws-ind2 from 1 by 1 until ws-ind2 > 1024 or
                   ws-recurso(ws-ind2:) = low-values
             if ws-recurso(ws-ind2:1) = "}"
               move ws-ind2 to ws-conta2
               move ws-recurso(ws-conta2 + 1:) to ws-recurso3
               add 1024 to ws-ind2
             end-if
           end-perform.
      *     if ws-conta > 1 
      *       add 1 from ws-conta2
      *    end-if.
           if ws-conta = 0 or ws-conta2 = 0
                 MOVE "ERROR: URL MAL FORMADA" TO WS-DES-RTA
                 MOVE 409 TO WS-COD-RTA
                 PERFORM ERRORES
           end-if.             
           move ws-recurso(ws-conta + 1 :ws-conta2 - (ws-conta + 1)) 
                to WS-PARAM-URL.
           IF WS-VARIABLE(WS-IND)(1:ws-conta2 - (ws-conta + 1)) = 
              WS-PARAM-URL
      * reemplaza la variable por el valor 
             move spaces to ws-recurso
             string ws-recurso2 delimited by spaces,
                    WS-VALOR(WS-IND) delimited by low-values,
                    ws-recurso3 delimited by spaces
               into ws-recurso
             end-string               
           ELSE
             MOVE "ERROR: VARIABLE EN URL INCORRECTA" TO WS-DES-RTA
             MOVE 409 TO WS-COD-RTA
             PERFORM ERRORES
           END-IF. 
           .

       ARMA-BODY-X.
               MOVE PTR-TAM-DATA TO PTR-TAM-HDR.
               MOVE 1 TO PTR-TAM-DATA.
               MOVE LOW-VALUES TO HTTP-DATA.
               IF WS-VARIABLE(WS-IND) = spaces or
                  WS-VARIABLE(WS-IND) = low-values
                 STRING '"' DELIMITED BY SIZE,
                     WS-VALOR(WS-IND) DELIMITED BY LOW-VALUES, 
                     '"' DELIMITED BY SIZE,
                     WS-COMA DELIMITED BY SPACE
                   INTO HTTP-DATA WITH POINTER PTR-TAM-DATA
                 END-STRING
               ELSE               
                 STRING '"' DELIMITED BY SIZE,
                     WS-VARIABLE(WS-IND) DELIMITED BY LOW-VALUES, 
                     '":"',
                     WS-VALOR(WS-IND) DELIMITED BY LOW-VALUES, 
                     '"' DELIMITED BY SIZE,
                     WS-COMA DELIMITED BY SPACE
                   INTO HTTP-DATA WITH POINTER PTR-TAM-DATA
                 END-STRING
               END-IF  
               SUBTRACT 1 FROM PTR-TAM-DATA.
           .
       ARMA-BODY-9.
               MOVE PTR-TAM-DATA TO PTR-TAM-HDR
               MOVE LOW-VALUES TO HTTP-DATA.
               MOVE 1 TO PTR-TAM-DATA
               STRING '"' DELIMITED BY SIZE,
                     WS-VARIABLE(WS-IND) DELIMITED BY LOW-VALUES, 
                     '":',
                     WS-VALOR(WS-IND) DELIMITED BY LOW-VALUES, 
                     WS-COMA DELIMITED BY SPACE
                 INTO HTTP-DATA WITH POINTER PTR-TAM-DATA
               END-STRING.
               SUBTRACT 1 FROM PTR-TAM-DATA.
           .
       ARMA-BODY-I.
               MOVE PTR-TAM-DATA TO PTR-TAM-HDR
               MOVE 1 TO PTR-TAM-DATA
               MOVE LOW-VALUES TO HTTP-DATA.
               IF WS-VARIABLE(WS-IND) = LOW-VALUES
                 STRING WS-VALOR(WS-IND) DELIMITED BY LOW-VALUES
                   INTO HTTP-DATA WITH POINTER PTR-TAM-DATA
                 END-STRING
               ELSE               
                 STRING '"' DELIMITED BY SIZE,
                       WS-VARIABLE(WS-IND) DELIMITED BY LOW-VALUES, 
                       '":',
                       WS-VALOR(WS-IND) DELIMITED BY LOW-VALUES
                   INTO HTTP-DATA WITH POINTER PTR-TAM-DATA
                 END-STRING
               END-IF.
               SUBTRACT 1 FROM PTR-TAM-DATA.
           .
       ARMA-BODY-F.
               MOVE PTR-TAM-DATA TO PTR-TAM-HDR
               MOVE LOW-VALUES TO HTTP-DATA.
               MOVE 1 TO PTR-TAM-DATA
               STRING WS-VALOR(WS-IND) DELIMITED BY LOW-VALUES, 
                     WS-COMA DELIMITED BY SPACE
                 INTO HTTP-DATA WITH POINTER PTR-TAM-DATA
               END-STRING.
               SUBTRACT 1 FROM PTR-TAM-DATA.
           .

       INICIALIZO-TCP.
           MOVE "0000" TO MAX-BUF-READS.
           MOVE "0000" TO MAX-BUF-WRITE.
           MOVE "1"    TO CON-LOG.|*> Esto determina si usa log el subftp.c
           MOVE "sin_conf" TO FILE-CNF.
           CALL "INIFTP" USING
                BY REFERENCE NMAX-BUF-READS,
                BY REFERENCE NMAX-BUF-WRITE,
                BY REFERENCE NCON-LOG,
                BY REFERENCE NFILE-CNF,
                BY REFERENCE NHTONS,
                VALUE 0.
           MOVE "-----INICIO SOCKET-----" TO W-MENSAJE-LOG.
           CALL "logger" USING W-MENSAJE-LOG.
           MOVE "0000" TO PARAM1-X25.
           MOVE "000" TO PARAM2-X25.|*> 20 seg. de timeout
       F-INICIALIZO-TCP.
           EXIT.

       EVALUO-RESPUESTA.
            IF WS-EN-HDR
              if WS-LINEA-RTA = spaces
                SET WS-EN-BODY TO TRUE
              ELSE
                if WS-LINEA-RTA(1:4) = 'HTTP'
                  MOVE WS-LINEA-RTA(10:3) TO WS-COD-RTA CONVERT
                  MOVE WS-LINEA-RTA(14:) TO WS-DES-RTA
                END-IF
              END-IF
            ELSE
      *       IF WS-LINEA-RTA(1:1) = "T" or WS-LINEA-RTA(1:1) = "t"
      *          MOVE "T" TO WS-COD-RTA
      *       ELSE
AR    *         IF WS-LINEA-RTA(2:1) is numeric 
AR    *            MOVE WS-LINEA-RTA(2:1) TO WS-COD-RTA
AR    *        ELSE
      *           MOVE "F" TO WS-COD-RTA
AR    *        END-IF
      *       END-IF
            if WS-LINEA-RTA NOT = spaces and WS-COD-RTA = 200
              INSPECT WS-LINEA-RTA TALLYING WS-IND FOR ALL "::="
              IF WS-IND > 0
                add 1 to ws-ind4
                UNSTRING WS-LINEA-RTA DELIMITED BY "::="
                  INTO WS-VAR-RESP(ws-ind4), 
                       WS-VAL-RESP(ws-ind4),
                       WS-TIP-RESP(ws-ind4)
                END-UNSTRING
              END-IF
            ELSE
              SET WS-FIN-SI TO TRUE           
            END-IF. 
       F-EVALUO-RESPUESTA.
           EXIT.
       
       CERRAR-X25.
           MOVE "C" TO TIPO-DE-OPERACION-X25.
           PERFORM LOPERA-X25 THRU FOPERA-X25.
      *     MOVE "N" TO WSOPENX25.
       F-CERRAR-X25.
           EXIT.

       LOPERA-X25.
      D    DISPLAY "llama a SUBFTP..."        
           INSPECT NUMERO-X25
                REPLACING TRAILING SPACES BY LOW-VALUES.
           INSPECT REGISTRO4096-X25 
                REPLACING TRAILING SPACES BY LOW-VALUES.
           INSPECT DEVICE-X25
                REPLACING TRAILING SPACES BY LOW-VALUES.
      *     DISPLAY "T-D-O-X25=" T-D-O-X25.
      D    DISPLAY REGISTRO4096-X25.
           IF TIPO-DE-OPERACION-X25 = "o"
             MOVE "020" TO PARAM2-X25
           ELSE
             MOVE "000" TO PARAM2-X25
           END-IF.
           CALL "SUBFTP" USING
                BY REFERENCE T-D-O-X25,  
                BY REFERENCE N-X25,      
                BY REFERENCE C-V-X25,    
                BY REFERENCE R4096-X25,      
                BY REFERENCE D-X25,      
                BY REFERENCE PARAM1-X25, 
                BY REFERENCE PARAM2-X25, 
                VALUE 0.
           MOVE RETURN-CODE TO RETCODE-X25.                         
           MOVE RETCODE-X25 TO X25-RETCODE.
           IF RETCODE-X25 < 0
              EVALUATE RETCODE-X25 
                WHEN -1 
                WHEN -9
                  STRING "ERROR TIMEOUT. COD.: ",
                         X25-RETCODE delimited by size
                    INTO WS-DES-RTA
                  END-STRING
                WHEN -3
                  STRING "ERROR DE CONEXION. COD.: ",
                         X25-RETCODE delimited by size
                    INTO WS-DES-RTA
                  END-STRING
                WHEN OTHER
                  STRING "ERROR COD.: ",
                         X25-RETCODE delimited by size
                    INTO WS-DES-RTA
                  END-STRING
               END-EVALUATE
               MOVE 409 TO WS-COD-RTA
               PERFORM ERRORES
               GO TO FOPERA-X25
           END-IF.

TMP   D    DISPLAY "RETCODE-X25=" RETCODE-X25.
TMP   D    IF RETCODE-X25 > 0 AND RETCODE-X25 <= 4096
TMP   D      DISPLAY R4096-X25(1:RETCODE-X25)
TMP   D    END-IF.
       FOPERA-X25.
           EXIT.
      
      *Convierte FI-COD a hexadecimal y lo deja en w-hex-value y lo rellena con low-values
       PASO-A-HEX.
           CALL "ASCII2HEX" USING FI-COD, HEX-VALUE
           EVALUATE TRUE               
             WHEN hex-value(1:1) <> "0"
               move 1 TO WS-IND2
             WHEN hex-value(2:1) <> "0" 
               move 2 TO WS-IND2
             WHEN hex-value(3:1) <> "0" 
               move 3 TO WS-IND2
             WHEN OTHER
               move 4 TO WS-IND2
           end-evaluate.
           move hex-value(WS-IND2:) to w-hex-value.
           INSPECT w-hex-value 
             REPLACING TRAILING SPACES BY LOW-VALUES.
      D    MOVE SPACES TO W-MENSAJE-LOG.
      D    STRING "DATA.SIZE: ", FI-COD DELIMITED BY SIZE,
      D           " ", w-hex-value , " ", 
      D           hex-value,  PTR-TAM-DATA DELIMITED BY SIZE
      D      INTO W-MENSAJE-LOG
      D    END-STRING  .
      D    CALL "logger" USING W-MENSAJE-LOG.

       END PROGRAM.
