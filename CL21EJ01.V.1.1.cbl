      ******************************************************************
      * Author: DIEGO ZABALA
      * Date: 21/10/2023
      * Purpose: CLASE 21 - EJERCICIO 1
      * DESCRIPCION: MODIFICAR TELEFONO O DIRECCION
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL21EJ1.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

       SELECT AGENDA
           ASSIGN TO '../AGENDA.VSAM'
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS AGENDA-ID
           ALTERNATE RECORD KEY IS AGENDA-TELEFONO WITH DUPLICATES
           FILE STATUS IS FS-AGENDA.

      *----------------------------------------------------------------*
       DATA DIVISION.

       FILE SECTION.

       FD AGENDA.
       01 REG-AGENDA.
          05 AGENDA-ID                  PIC 9(08).
          05 AGENDA-APELLIDO            PIC X(25).
          05 AGENDA-NOMBRE              PIC X(25).
          05 AGENDA-TELEFONO            PIC X(09).
          05 AGENDA-DIRECCION           PIC X(22).

       WORKING-STORAGE SECTION.

       01 FS-STATUS.
          05 FS-AGENDA              PIC X(2).
             88 FS-AGENDA-OK             VALUE '00'.
             88 FS-AGENDA-EOF            VALUE '10'.
             88 FS-AGENDA-NFD            VALUE '35'.

       01 WS-CONTADORES.
           05 WS-CONT-REG-AGENDA    PIC 9(04) VALUE 0.


       01 WS-VARIABLES-GENERALES.
           05 WS-ID                          PIC 9(08).
           05 WS-TELEFONO                    PIC X(09).
           05 WS-AUX-CANT                    PIC 9(05).
           05 WS-IMP-ACUM                    PIC 9(10)V9(02) VALUE 0.
           05 WS-FORMAT-IMPORTE              PIC ZZZ.ZZ9.
           05 WS-OPCION                      pic x(03).
           05 WS-DIR                         PIC X(22).
           05 WS-APE                         PIC X(25).
           05 WS-NOM                         PIC X(25).
           05 WS-SINO                        PIC X(02).
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 1000-INICIAR
              THRU 1000-INICIAR-EXIT.

           PERFORM 2200-PROCESAR
              THRU 2200-PROCESAR-EXIT.

           PERFORM 3100-FINALIZAR
              THRU 3100-FINALIZAR-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       1000-INICIAR.

           INITIALIZE WS-CONTADORES.

           PERFORM 1100-ABRIR-AGENDA
              THRU 1100-ABRIR-AGENDA-EXIT.

       1000-INICIAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1100-ABRIR-AGENDA.

           OPEN I-O AGENDA.

           EVALUATE TRUE
               WHEN FS-AGENDA-OK
                    CONTINUE
               WHEN FS-AGENDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
               WHEN OTHER
                    DISPLAY 'ERROR AL ABRIR EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
           END-EVALUATE.

       1100-ABRIR-AGENDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       1110-LEER-AGENDA.

           READ AGENDA next RECORD.

           EVALUATE TRUE
               WHEN FS-AGENDA-OK
                    ADD 1                   TO WS-CONT-REG-AGENDA
               WHEN FS-AGENDA-EOF
                    CONTINUE
               WHEN OTHER
                    DISPLAY 'ERROR AL LEER EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
                    STOP RUN
           END-EVALUATE.

       1110-LEER-AGENDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2200-PROCESAR.

           PERFORM UNTIL FUNCTION UPPER-CASE(WS-OPCION) = 'SAL'
               DISPLAY '*---------------------------------------------*'
               DISPLAY '*Ingresa la opcion deseada:                   *'
               DISPLAY '*   - VER - para ver la agenda                *'
               DISPLAY '*   - TEL - Para modificar el telefono        *'
               DISPLAY '*   - APE - Para modificar el apellido        *'
               DISPLAY '*   - NOM - Para modificar el nombre          *'
               DISPLAY '*   - DIR - Para modificar la direccion       *'
               DISPLAY '*   - DEL - Para borrar un contacto !!        *'
               DISPLAY '*   - NEW - Insertar un nuevo contacto        *'
               DISPLAY '*   - SAL - Para salir.                       *'
               DISPLAY '*---------------------------------------------*'
               ACCEPT WS-OPCION

               EVALUATE FUNCTION UPPER-CASE(WS-OPCION)
               WHEN 'TEL'
                  PERFORM 2300-MODIFICAR-TEL
                     THRU 2300-MODIFICAR-TEL-EXIT
               WHEN 'DIR'
                  PERFORM 2600-MODIFICAR-DIR
                     THRU 2600-MODIFICAR-DIR-EXIT
               WHEN 'NOM'
                  PERFORM 2800-MODIFICAR-NOM
                     THRU 2800-MODIFICAR-NOM-EXIT
               WHEN 'APE'
                  PERFORM 2700-MODIFICAR-APE
                     THRU 2700-MODIFICAR-APE-EXIT
               WHEN 'NEW'
                   PERFORM 3000-INSERTAR
                      THRU 3000-INSERTAR-EXIT
               WHEN 'SAL'
                   CONTINUE
               WHEN 'DEL'
                   PERFORM 2900-BORRAR
                      THRU 2900-BORRAR-EXIT
               WHEN 'VER'
                   PERFORM 2400-MOSTRAR-AGENDA
                      THRU 2400-MOSTRAR-AGENDA-EXIT
               WHEN OTHER
                   DISPLAY 'Opción ingresada inválida, reintenta'
               END-EVALUATE
           END-PERFORM.

       2200-PROCESAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2300-MODIFICAR-TEL.

           DISPLAY 'Ingresa ID para modificar Tel: ' ACCEPT WS-ID.

           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es inválido. '
           ELSE
               MOVE WS-ID TO AGENDA-ID

               START AGENDA KEY IS = AGENDA-ID
               IF NOT FS-AGENDA-OK
                   DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
                   DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
               END-IF
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT
               DISPLAY 'Actual     : ' REG-AGENDA
               Display 'Ingrese nuevo Tel: ' ACCEPT WS-TELEFONO

               IF WS-TELEFONO > SPACES

                   MOVE WS-TELEFONO TO AGENDA-TELEFONO
                   PERFORM 2500-ACTUALIZAR-AGENDA
                      THRU 2500-ACTUALIZAR-AGENDA-EXIT

                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'Teléfono ingresado con error.'
               END-IF
           END-IF.

       2300-MODIFICAR-TEL-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2400-MOSTRAR-AGENDA.

           MOVE 0 TO AGENDA-ID

           START AGENDA KEY IS  >= AGENDA-ID

           IF NOT FS-AGENDA-OK
              DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
              DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
           END-IF.

           PERFORM 1110-LEER-AGENDA
              THRU 1110-LEER-AGENDA-EXIT.

           PERFORM UNTIL FS-AGENDA-EOF
               DISPLAY 'LEG: ' AGENDA-ID ' - '
                       'APE: ' AGENDA-APELLIDO ' - '
                       'NOM: ' AGENDA-NOMBRE ' - '
                       'TEL: ' AGENDA-TELEFONO ' - '
                       'DIR: ' AGENDA-DIRECCION

               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT

           END-PERFORM.

       2400-MOSTRAR-AGENDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2500-ACTUALIZAR-AGENDA.

           REWRITE REG-AGENDA.

           EVALUATE TRUE
               WHEN FS-AGENDA-OK
                    CONTINUE
               WHEN FS-AGENDA-NFD
                    DISPLAY 'NO SE ENCUENTRA EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
           END-EVALUATE.
       2500-ACTUALIZAR-AGENDA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2600-MODIFICAR-DIR.

           DISPLAY 'Ingresa ID para modificar DIR: ' ACCEPT WS-ID.

           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es inválido. '
           ELSE
               MOVE WS-ID TO AGENDA-ID

               START AGENDA KEY IS = AGENDA-ID
               IF NOT FS-AGENDA-OK
                   DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
                   DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
               END-IF
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT
               DISPLAY 'Actual     : ' REG-AGENDA
               Display 'Ingrese nueva Direccion: ' ACCEPT WS-DIR

               IF WS-DIR > SPACES

                   MOVE WS-DIR TO AGENDA-DIRECCION
                   PERFORM 2500-ACTUALIZAR-AGENDA
                      THRU 2500-ACTUALIZAR-AGENDA-EXIT

                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'DIRECCION ingresada con error.'
               END-IF
           END-IF.

       2600-MODIFICAR-DIR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2700-MODIFICAR-APE.

           DISPLAY 'Ingresa ID para modificar Apellido: ' ACCEPT WS-ID.

           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es inválido. '
           ELSE
               MOVE WS-ID TO AGENDA-ID

               START AGENDA KEY IS = AGENDA-ID
               IF NOT FS-AGENDA-OK
                   DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
                   DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
               END-IF
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT
               DISPLAY 'Actual     : ' REG-AGENDA
               Display 'Ingrese nueva Apellido: ' ACCEPT WS-APE

               IF WS-APE > SPACES

                   MOVE WS-APE TO AGENDA-APELLIDO
                   PERFORM 2500-ACTUALIZAR-AGENDA
                      THRU 2500-ACTUALIZAR-AGENDA-EXIT

                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'APELLIDO ingresado con error.'
               END-IF
           END-IF.

       2700-MODIFICAR-APE-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2800-MODIFICAR-NOM.

           DISPLAY 'Ingresa ID para modificar Nombre: ' ACCEPT WS-ID.

           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es inválido. '
           ELSE
               MOVE WS-ID TO AGENDA-ID

               START AGENDA KEY IS = AGENDA-ID
               IF NOT FS-AGENDA-OK
                   DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
                   DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
               END-IF
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT
               DISPLAY 'Actual     : ' REG-AGENDA
               Display 'Ingrese nuevo nombre: ' ACCEPT WS-NOM

               IF WS-NOM > SPACES

                   MOVE WS-NOM TO AGENDA-NOMBRE
                   PERFORM 2500-ACTUALIZAR-AGENDA
                      THRU 2500-ACTUALIZAR-AGENDA-EXIT

                   DISPLAY 'Actualizado: ' REG-AGENDA
               ELSE
                   DISPLAY 'Nombre ingresado con error.'
               END-IF
           END-IF.

       2800-MODIFICAR-NOM-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       2900-BORRAR.

           DISPLAY 'Ingresa ID que queres borrar: ' ACCEPT WS-ID.

           IF WS-ID IS NOT NUMERIC
               DISPLAY 'El ID ingresado es inválido. '
           ELSE
               MOVE WS-ID TO AGENDA-ID

               START AGENDA KEY IS = AGENDA-ID
               IF NOT FS-AGENDA-OK
                   DISPLAY 'ERROR AL START AGENDA: ' FS-AGENDA
                   DISPLAY 'PARRAFO : 2400-MOSTRAR-AGENDA'
               END-IF
               PERFORM 1110-LEER-AGENDA
                  THRU 1110-LEER-AGENDA-EXIT
               DISPLAY 'Registro a borrar : ' REG-AGENDA
               Display 'Estas seguro que queres borrar (SI o NO): '
                                  ACCEPT WS-SINO

               IF FUNCTION UPPER-CASE(WS-SINO) = 'SI'

                   DELETE AGENDA
                   EVALUATE TRUE
                   WHEN FS-AGENDA-OK
                      DISPLAY 'Contacto borrado con éxito.'

                   WHEN OTHER
                       DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE AGENDA'
                       DISPLAY 'FILE STATUS: ' FS-AGENDA
                   END-EVALUATE

                   DISPLAY 'Contacto borrado con éxito.'
               ELSE
                   DISPLAY 'Por ahora no borramos nada. :)'
               END-IF
           END-IF.

       2900-BORRAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3000-INSERTAR.
           DISPLAY 'Vamos a ingresar los datos del contacto.'
           DISPLAY 'Ingresa nuevo ID : ' ACCEPT WS-ID.
           DISPLAY 'Ingresa Apellido : ' ACCEPT WS-APE.
           DISPLAY 'Ingresa Nombre   : ' ACCEPT WS-NOM.
           DISPLAY 'Ingresa Telefono : ' ACCEPT WS-TELEFONO.
           DISPLAY 'Ingresa Direccion: ' ACCEPT WS-DIR.

           MOVE WS-ID            TO AGENDA-ID.
           DISPLAY 'WS-ID:' WS-ID

           MOVE WS-APE           TO AGENDA-APELLIDO.
           MOVE WS-NOM           TO AGENDA-NOMBRE.
           MOVE WS-APE           TO AGENDA-APELLIDO.
           MOVE WS-DIR           TO AGENDA-DIRECCION.
           MOVE WS-TELEFONO      TO AGENDA-TELEFONO.

           WRITE REG-AGENDA.

           EVALUATE TRUE
               WHEN FS-AGENDA-OK
                    DISPLAY 'Contacto insertado con exito.'
               WHEN OTHER
                    DISPLAY 'ERROR AL GRABAR EL ARCHIVO DE AGENDA'
                    DISPLAY 'FILE STATUS: ' FS-AGENDA
           END-EVALUATE.

       3000-INSERTAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-FINALIZAR.

           PERFORM 3200-CERRAR-ARCHIVOS
              THRU 3200-CERRAR-ARCHIVOS-FIN.

       3100-FINALIZAR-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3200-CERRAR-ARCHIVOS.

           CLOSE AGENDA.

           IF NOT FS-AGENDA-OK
              DISPLAY 'ERROR AL CERRAR ARCHIVO AGENDA: ' FS-AGENDA
           END-IF.

       3200-CERRAR-ARCHIVOS-FIN.
           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL21EJ1.
