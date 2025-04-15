"! Utility Class for Statement MESSAGE
"! <p>Based on SAP class CL_MESSAGE_HELPER</p>
CLASS zcl_message_statement_helper DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! Transfer structure for parameters in OTR texts (type String)
    TYPES otr_parameter       TYPE sbtfr_param.

    "! Table with assignment of parameter and value (type String)
    TYPES otr_parameters      TYPE STANDARD TABLE OF otr_parameter WITH DEFAULT KEY.

    "! Transfer structure for parameters in OTR texts (type String)
    TYPES otr_char_parameter  TYPE sotr_param.

    "! Table with assignment of parameter and value (type String)
    TYPES otr_char_parameters TYPE STANDARD TABLE OF otr_char_parameter WITH DEFAULT KEY.

    "! OTR Id
    TYPES otr_id              TYPE sotr_conc.

    "! The message object used by MESSAGE
    CLASS-DATA message_object TYPE REF TO if_message.

    "! Gets the top T100 exception from PREVIOUS chain
    "! @parameter EXCEPTION | Top object of scanned exception objects
    "! @parameter RESULT    | Top exception object with non-empty T100 text
    CLASS-METHODS get_latest_t100_exception
      IMPORTING !exception    TYPE REF TO cx_root
      RETURNING VALUE(result) TYPE REF TO if_t100_message.

    "! Returns Long Text for Parameter(s)
    "! @parameter TEXT               | Message
    "! @parameter PRESERVE_NEWLINES  | Preserves new line in message
    "! @parameter T100_PREPEND_SHORT | True if the short text is comes first in T100 texts
    "! @parameter RESULT             | Text
    CLASS-METHODS get_longtext_for_message
      IMPORTING VALUE(text)               TYPE REF TO if_message
                VALUE(preserve_newlines)  TYPE abap_bool OPTIONAL
                VALUE(t100_prepend_short) TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(result)             TYPE string.

    "! Returns Short Text for Parameter(s)
    "! @parameter TEXT   | Message containing text
    "! @parameter RESULT | result string
    CLASS-METHODS get_text_for_message
      IMPORTING VALUE(text)   TYPE REF TO if_message
      RETURNING VALUE(result) TYPE string.

    "! Sets MESSAGE variables, if TEXT is of type CLIKE
    "! @parameter TEXT | text
    CLASS-METHODS set_msg_vars_for_clike
      IMPORTING VALUE(text) TYPE clike.

    "! Sets MESSAGE Variables, REF TO IF_T100_MESSAGE
    "! @parameter TEXT                       | Message object
    "! @raising   CX_SY_MESSAGE_ILLEGAL_TEXT | Invalid MESSAGE Text Parameter
    CLASS-METHODS set_msg_vars_for_if_t100_msg
      IMPORTING VALUE(text) TYPE REF TO if_t100_message
      RAISING   cx_sy_message_illegal_text.

    "! Sets MESSAGE Variables, if Text Is of Type ANY
    "! @parameter TEXT                       | Message
    "! @parameter STRING                     | Result
    "! @raising   CX_SY_MESSAGE_ILLEGAL_TEXT | Invalid MESSAGE Text Parameter
    CLASS-METHODS set_msg_vars_for_any
      IMPORTING !text         TYPE any
      EXPORTING VALUE(string) TYPE string
      RAISING   cx_sy_message_illegal_text.

    "! Sets MESSAGE Variables, REF TO IF_MESSAGE
    "! @parameter TEXT                       | Message object
    "! @parameter STRING                     | String for transfer to MESSAGE
    "! @raising   CX_SY_MESSAGE_ILLEGAL_TEXT | Invalid MESSAGE Text Parameter
    CLASS-METHODS set_msg_vars_for_if_msg
      IMPORTING VALUE(text)   TYPE REF TO if_message
      EXPORTING VALUE(string) TYPE string
      RAISING   cx_sy_message_illegal_text.

    "! Sets Appropriate Key for MESSAGE
    "! @parameter MSG     | Message object
    "! @parameter T100KEY | T100 key with mapping of parameters to attribute names
    "! @parameter TEXTID  | Key for logical object in OTR
    CLASS-METHODS check_msg_kind
      IMPORTING VALUE(msg)     TYPE REF TO if_message
      EXPORTING VALUE(t100key) TYPE scx_t100key
                VALUE(textid)  TYPE otr_id.

    "! Returns Text Parameter
    "! @parameter OBJ    | Object, the attributes are read from
    "! @parameter PARAMS | Table with assignment of parameters and values
    CLASS-METHODS get_text_params
      IMPORTING VALUE(obj) TYPE REF TO object
      EXPORTING params     TYPE otr_char_parameters.

    "! Returns OTR Short Text Without Parameter Setting
    "! @parameter TEXTID | Key for logical object in OTR
    "! @parameter RESULT | Short text
    CLASS-METHODS get_otr_text_raw
      IMPORTING textid  TYPE otr_id
      EXPORTING !result TYPE string.

    "! Returns T100 short text
    "! @parameter OBJ     | Object
    "! @parameter T100KEY | T100 key with mapping of parameters to attribute names
    "! @parameter RESULT  | Short text
    CLASS-METHODS get_t100_text_for
      IMPORTING VALUE(obj) TYPE REF TO object
                t100key    TYPE scx_t100key
      EXPORTING !result    TYPE string.

    "! Gets T100 message
    "! @parameter OBJ    | Message object
    "! @parameter RESULT | T100 message
    CLASS-METHODS get_t100_for_object
      IMPORTING obj           TYPE REF TO if_t100_message
      RETURNING VALUE(result) TYPE symsg.

    "! Replaces text parameter
    "! @parameter OBJ    | Object
    "! @parameter RESULT | Text
    CLASS-METHODS replace_text_params
      IMPORTING VALUE(obj) TYPE REF TO object
      CHANGING  !result    TYPE string.

    "! Replaces new lines with blank characters
    "! @parameter MESSAGE | Text
    CLASS-METHODS strip_newlines_from
      CHANGING !message TYPE string.

    "! Return text parameters
    "! @parameter OBJ    | Object
    "! @parameter PARAMS | Table with assignment of parameter and value
    CLASS-METHODS get_text_sparams
      IMPORTING VALUE(obj) TYPE REF TO object
      EXPORTING params     TYPE otr_parameters.

  PRIVATE SECTION.
    CLASS-METHODS get_t100_longtext_for
      IMPORTING VALUE(obj)               TYPE REF TO object
                t100key                  TYPE scx_t100key
                VALUE(text)              TYPE REF TO if_message OPTIONAL
                VALUE(preserve_newlines) TYPE abap_bool         OPTIONAL
                VALUE(prepend_short)     TYPE abap_bool         DEFAULT 'X'
      EXPORTING VALUE(result)            TYPE string.

    CLASS-METHODS set_single_msg_var
      IMPORTING arg        TYPE clike
                VALUE(obj) TYPE REF TO object
      EXPORTING !target    TYPE c.

    CLASS-METHODS set_single_msg_var_clike
      IMPORTING arg     TYPE clike
      EXPORTING !target TYPE c.

    CLASS-METHODS set_single_msg_var_numeric
      IMPORTING arg     TYPE numeric
      EXPORTING !target TYPE c.

    CLASS-METHODS set_single_msg_var_xseq
      IMPORTING arg     TYPE xsequence
      EXPORTING !target TYPE c.

    CLASS-METHODS get_otr_longtext_raw
      IMPORTING textid  TYPE sotr_conc
      EXPORTING !result TYPE string.

    CLASS-METHODS get_t100_longtext_itf
      IMPORTING t100key    TYPE scx_t100key
                VALUE(obj) TYPE REF TO object
      EXPORTING itf_itab   TYPE tline_tab.
ENDCLASS.


CLASS zcl_message_statement_helper IMPLEMENTATION.

  METHOD strip_newlines_from.
    DATA space_newline TYPE string.

    CONCATENATE ` ` cl_abap_char_utilities=>cr_lf INTO space_newline.
    REPLACE ALL OCCURRENCES OF space_newline
            IN message WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
            IN message WITH ` `.
    CONCATENATE ` ` cl_abap_char_utilities=>newline INTO space_newline.
    REPLACE ALL OCCURRENCES OF space_newline
            IN message WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
            IN message WITH ` `.
  ENDMETHOD.


  METHOD set_single_msg_var_xseq.

    " a kind of MOVE where all conversion errors are signaled by exceptions
    WRITE arg LEFT-JUSTIFIED TO target.

  ENDMETHOD.


  METHOD set_single_msg_var_numeric.

    " a kind of MOVE where all conversion errors are signaled by exceptions
    WRITE arg LEFT-JUSTIFIED TO target.

  ENDMETHOD.


  METHOD set_single_msg_var_clike.

    " a kind of MOVE where all conversion errors are signaled by exceptions
    WRITE arg LEFT-JUSTIFIED TO target.

  ENDMETHOD.


  METHOD set_single_msg_var.

    FIELD-SYMBOLS <fs> TYPE any.

    IF arg IS INITIAL.

      CLEAR target.

    ELSE.

      TRY.
          ASSIGN obj->(arg) TO <fs>.
          IF sy-subrc NE 0.
            RAISE EXCEPTION TYPE cx_sy_assign_cast_illegal_cast.
          ENDIF.
          " We cannot catch all conversion exceptions on MOVE => use CALL
          set_single_msg_var_clike( EXPORTING arg    = <fs>
                                    IMPORTING target = target ).

        CATCH cx_sy_dyn_call_illegal_type.
          TRY.
              set_single_msg_var_numeric( EXPORTING arg    = <fs>
                                          IMPORTING target = target ).

            CATCH cx_sy_dyn_call_illegal_type.
              TRY.
                  set_single_msg_var_xseq( EXPORTING arg    = <fs>
                                           IMPORTING target = target ).
                CATCH cx_sy_dyn_call_illegal_type.
                  CONCATENATE '&' arg '&' INTO target.
              ENDTRY.
          ENDTRY.

        CATCH cx_sy_assign_cast_illegal_cast.
          CONCATENATE '&' arg '&' INTO target.

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD set_msg_vars_for_if_t100_msg.

    IF text IS INITIAL.
      RAISE EXCEPTION TYPE cx_sy_message_illegal_text
        EXPORTING textid = cx_sy_message_illegal_text=>initial_ref.
    ENDIF.

    sy-msgid = text->t100key-msgid.
    sy-msgno = text->t100key-msgno.

    set_single_msg_var( EXPORTING arg    = text->t100key-attr1
                                  obj    = text
                        IMPORTING target = sy-msgv1 ).

    set_single_msg_var( EXPORTING arg    = text->t100key-attr2
                                  obj    = text
                        IMPORTING target = sy-msgv2 ).

    set_single_msg_var( EXPORTING arg    = text->t100key-attr3
                                  obj    = text
                        IMPORTING target = sy-msgv3 ).

    set_single_msg_var( EXPORTING arg    = text->t100key-attr4
                                  obj    = text
                        IMPORTING target = sy-msgv4 ).

  ENDMETHOD.


  METHOD set_msg_vars_for_if_msg.

    DATA iref TYPE REF TO if_t100_message.

    TRY.
        iref ?= text.
        set_msg_vars_for_if_t100_msg( text = iref ).
        CLEAR string.

      CATCH cx_sy_move_cast_error.
        string = text->get_text( ).
        set_msg_vars_for_clike( text = string ).
        message_object = text.

    ENDTRY.

  ENDMETHOD.


  METHOD set_msg_vars_for_clike.

    DATA offset TYPE i.
    DATA c200   TYPE c LENGTH 200.

    CLEAR message_object.
    sy-msgid = '00'.
    sy-msgno = '001'.
    c200 = text.

    sy-msgv1 = c200(50).
    IF sy-msgv1+49(1) IS INITIAL.
      offset = 49.
    ELSE.
      offset = 50.
    ENDIF.

    sy-msgv2 = c200+offset(50).
    IF sy-msgv2+49(1) IS INITIAL.
      offset = offset + 49.
    ELSE.
      offset = offset + 50.
    ENDIF.

    sy-msgv3 = c200+offset(50).
    IF sy-msgv3+49(1) IS INITIAL.
      offset = offset + 49.
    ELSE.
      offset = offset + 50.
    ENDIF.

    sy-msgv4 = c200+offset(50).

  ENDMETHOD.


  METHOD set_msg_vars_for_any.

    DATA iref TYPE REF TO if_t100_message.

    TRY.
        iref ?= text.

        set_msg_vars_for_if_t100_msg( text = iref ).
        CLEAR string.

      CATCH cx_sy_move_cast_error.
        TRY.
            set_msg_vars_for_clike( text = text ).
            string = text.

          CATCH cx_sy_dyn_call_illegal_type.
            RAISE EXCEPTION TYPE cx_sy_message_illegal_text
              EXPORTING textid = cx_sy_message_illegal_text=>illegal_type.

        ENDTRY.

    ENDTRY.

  ENDMETHOD.


  METHOD replace_text_params.

    DATA params TYPE sbtfr_param_tt.

    get_text_sparams( EXPORTING obj    = obj
                      IMPORTING params = params ).
    CALL FUNCTION 'SOTR_REPLACE_PARAMS'
      EXPORTING sparameter = params
      CHANGING  text       = result.

  ENDMETHOD.


  METHOD get_text_sparams.

    DATA descr   TYPE REF TO cl_abap_objectdescr.
    DATA par     TYPE sbtfr_param.
    DATA char255 TYPE c LENGTH 255.
    DATA cref    TYPE REF TO data.
    DATA clen    TYPE i.

    FIELD-SYMBOLS <attr> TYPE abap_attrdescr.
    FIELD-SYMBOLS <fs>   TYPE any.
    FIELD-SYMBOLS <cfs>  TYPE c.

    descr ?= cl_abap_objectdescr=>describe_by_object_ref( obj ).

    LOOP AT descr->attributes ASSIGNING <attr>.

      IF    <attr>-visibility  EQ cl_abap_classdescr=>private
        " we can only access protected attributes if we are a friend
         OR <attr>-is_class    NE abap_false
         OR <attr>-is_constant NE abap_false.
        CONTINUE.
      ENDIF.

      par-param = <attr>-name.

      IF <attr>-name IS INITIAL.

        CLEAR par-value.

      ELSEIF <attr>-type_kind EQ cl_abap_typedescr=>typekind_string
          OR <attr>-type_kind EQ cl_abap_typedescr=>typekind_xstring.

        ASSIGN obj->(<attr>-name) TO <fs>.
        IF sy-subrc NE 0.
          CONCATENATE '&' <attr>-name '&' INTO par-value.
        ELSE.
          par-value = <fs>.
        ENDIF.

      ELSEIF <attr>-length LT 128
          OR     <attr>-length LT 256
             AND (    <attr>-type_kind EQ cl_abap_typedescr=>typekind_char
                   OR <attr>-type_kind EQ cl_abap_typedescr=>typekind_num ).

        set_single_msg_var( EXPORTING arg    = <attr>-name
                                      obj    = obj
                            IMPORTING target = char255 ).   " WRITE can not have destination of type string
        par-value = char255.

      ELSE.

        IF <attr>-type_kind EQ cl_abap_typedescr=>typekind_hex.
          clen = <attr>-length * 2.
        ELSE.
          clen = <attr>-length.
        ENDIF.
        CREATE DATA cref TYPE c LENGTH clen.
        ASSIGN cref->* TO <cfs>.
        set_single_msg_var( EXPORTING arg    = <attr>-name
                                      obj    = obj
                            IMPORTING target = <cfs> ).   " WRITE can not have destination of type string
        par-value = <cfs>.

      ENDIF.

      APPEND par TO params.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_text_params.

    DATA descr TYPE REF TO cl_abap_objectdescr.
    DATA par   TYPE sotr_param.

    FIELD-SYMBOLS <attr> TYPE abap_attrdescr.

    descr ?= cl_abap_objectdescr=>describe_by_object_ref( obj ).

    LOOP AT descr->attributes ASSIGNING <attr>.
      " enable constants - ARO 01/2012
      IF <attr>-visibility NE cl_abap_classdescr=>private.
        " we can only access protected attributes if we are a friend
        par-param = <attr>-name.
        set_single_msg_var( EXPORTING arg    = <attr>-name
                                      obj    = obj
                            IMPORTING target = par-value ).
        APPEND par TO params.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_text_for_message.

    DATA t100key TYPE scx_t100key.
    DATA textid  TYPE sotr_conc.

    IF text IS INITIAL.
      RETURN.
    ENDIF.

    cl_message_helper=>check_msg_kind( EXPORTING msg     = text
                                       IMPORTING t100key = t100key
                                                 textid  = textid ).

    IF t100key IS NOT INITIAL.
      cl_message_helper=>get_t100_text_for( EXPORTING obj     = text
                                                      t100key = t100key
                                            IMPORTING result  = result ).
    ELSEIF textid IS NOT INITIAL.
      cl_message_helper=>get_otr_text_raw( EXPORTING textid = textid
                                           IMPORTING result = result ).
      cl_message_helper=>replace_text_params( EXPORTING obj    = text
                                              CHANGING  result = result ).
    ENDIF.

  ENDMETHOD.


  METHOD get_t100_text_for.

    DATA msgid    TYPE symsgid.
    DATA msgty    TYPE symsgty.
    DATA msgno    TYPE symsgno.
    DATA msgv1    TYPE symsgv.
    DATA msgv2    TYPE symsgv.
    DATA msgv3    TYPE symsgv.
    DATA msgv4    TYPE symsgv.
    DATA str      TYPE string.
    DATA str_len  TYPE i.
    DATA ind_ofs  TYPE i.
    DATA ind      TYPE c LENGTH 1.
    DATA auto_ind TYPE n LENGTH 1.
    DATA start    TYPE i.
    DATA len      TYPE i.

    msgid = sy-msgid.
    msgty = sy-msgty.
    msgno = sy-msgno.
    msgv1 = sy-msgv1.
    msgv2 = sy-msgv2.
    msgv3 = sy-msgv3.
    msgv4 = sy-msgv4.

    " the following is basically SET_MSG_VARS_FOR_IF_T100 ----
    sy-msgid = t100key-msgid.
    sy-msgno = t100key-msgno.

    set_single_msg_var( EXPORTING arg    = t100key-attr1
                                  obj    = obj
                        IMPORTING target = sy-msgv1 ).

    set_single_msg_var( EXPORTING arg    = t100key-attr2
                                  obj    = obj
                        IMPORTING target = sy-msgv2 ).

    set_single_msg_var( EXPORTING arg    = t100key-attr3
                                  obj    = obj
                        IMPORTING target = sy-msgv3 ).

    set_single_msg_var( EXPORTING arg    = t100key-attr4
                                  obj    = obj
                        IMPORTING target = sy-msgv4 ).
    " end of similarity with SET_MSG_VARS_FOR_IF_T100 ----

    " MESSAGE uses logon language, but exceptions should respect text environment
    CLEAR result.
    IF cl_abap_syst=>get_logon_language( ) NE sy-langu.
      SELECT SINGLE text FROM t100
        INTO str
        WHERE sprsl EQ sy-langu AND arbgb EQ sy-msgid AND msgnr EQ sy-msgno.
      IF sy-subrc EQ 0.
        str_len = strlen( str ).
        start = 0.
        auto_ind = 1.
        FIND ALL OCCURRENCES OF '&' IN str RESULTS DATA(amps).
        LOOP AT amps ASSIGNING FIELD-SYMBOL(<amp>).
          " copy string before parameter to result
          len = <amp>-offset - start.
          IF len GT 0.
            CONCATENATE result str+start(len) INTO result.
          ENDIF.
          " determine param index (either from following digit or from position) and start of next string part
          ind_ofs = <amp>-offset + 1.
          IF ind_ofs LT str_len AND str+ind_ofs(1) CA '12345678'.
            "&0 and &9 are positional param "&" + "0"/"9"
            "&5..&8 are ignored
            ind = str+ind_ofs(1).
            start = ind_ofs + 1.
          ELSE.
            ind = auto_ind.
            IF auto_ind LE 4.
              auto_ind = auto_ind + 1.
            ENDIF.
            start = ind_ofs.
          ENDIF.
          " insert parameter value into result
          CASE ind.
            WHEN '1'.
              CONCATENATE result sy-msgv1 INTO result.
            WHEN '2'.
              CONCATENATE result sy-msgv2 INTO result.
            WHEN '3'.
              CONCATENATE result sy-msgv3 INTO result.
            WHEN '4'.
              CONCATENATE result sy-msgv4 INTO result.
              " ignore others
          ENDCASE.
        ENDLOOP.
        IF start LT str_len.
          CONCATENATE result str+start INTO result.
        ENDIF.
      ENDIF.
    ENDIF.
    IF result IS INITIAL. " default/fallback: MESSAGE statement
      " please don't remove following line, as the ABAP Message Navigation is relying on it
      " in order to work for T100 based exception classes:
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              INTO result
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    sy-msgid = msgid.
    sy-msgty = msgty.
    sy-msgno = msgno.
    sy-msgv1 = msgv1.
    sy-msgv2 = msgv2.
    sy-msgv3 = msgv3.
    sy-msgv4 = msgv4.

  ENDMETHOD.


  METHOD get_t100_longtext_itf.

    DATA docu_key TYPE doku_obj.
    DATA itf_line TYPE tline.

    CONCATENATE t100key-msgid t100key-msgno INTO docu_key.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING  id     = 'NA'
                 langu  = sy-langu
                 object = docu_key
                 typ    = 'E'
      TABLES     line   = itf_itab
      EXCEPTIONS OTHERS = 1.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    " the following is basically SET_MSG_VARS_FOR_IF_T100 ----
    set_single_msg_var( EXPORTING arg    = t100key-attr1
                                  obj    = obj
                        IMPORTING target = sy-msgv1 ).

    set_single_msg_var( EXPORTING arg    = t100key-attr2
                                  obj    = obj
                        IMPORTING target = sy-msgv2 ).

    set_single_msg_var( EXPORTING arg    = t100key-attr3
                                  obj    = obj
                        IMPORTING target = sy-msgv3 ).

    set_single_msg_var( EXPORTING arg    = t100key-attr4
                                  obj    = obj
                        IMPORTING target = sy-msgv4 ).
    " end of similarity with SET_MSG_VARS_FOR_IF_T100 ----

    itf_line-tdformat = '/:'.
    itf_line-tdline   = sy-msgv4.
    REPLACE ALL OCCURRENCES OF '''' IN itf_line-tdline WITH ''''''.
    CONCATENATE 'DEFINE &V4& = ''' itf_line-tdline ''''
                INTO itf_line-tdline.
    INSERT itf_line INTO itf_itab INDEX 1.
    itf_line-tdline = sy-msgv3.
    REPLACE ALL OCCURRENCES OF '''' IN itf_line-tdline WITH ''''''.
    CONCATENATE 'DEFINE &V3& = ''' itf_line-tdline ''''
                INTO itf_line-tdline.
    INSERT itf_line INTO itf_itab INDEX 1.
    itf_line-tdline = sy-msgv2.
    REPLACE ALL OCCURRENCES OF '''' IN itf_line-tdline WITH ''''''.
    CONCATENATE 'DEFINE &V2& = ''' itf_line-tdline ''''
                INTO itf_line-tdline.
    INSERT itf_line INTO itf_itab INDEX 1.
    itf_line-tdline = sy-msgv1.
    REPLACE ALL OCCURRENCES OF '''' IN itf_line-tdline WITH ''''''.
    CONCATENATE 'DEFINE &V1& = ''' itf_line-tdline ''''
                INTO itf_line-tdline.
    INSERT itf_line INTO itf_itab INDEX 1.

  ENDMETHOD.


  METHOD get_t100_longtext_for.

    " Long text often has no parameters -> prepend short text

    DATA len      TYPE i.
    DATA itf_itab TYPE tline_tab.
    DATA asc_itab TYPE string_table.

    FIELD-SYMBOLS <asc_line> LIKE LINE OF asc_itab.

    IF prepend_short IS NOT INITIAL.
      IF text IS INITIAL.
        get_t100_text_for( EXPORTING obj     = obj
                                     t100key = t100key
                           IMPORTING result  = result ).
      ELSE.
        result = text->get_text( ).
      ENDIF.
      IF result IS NOT INITIAL.
        len = strlen( result ). len = len - 1.
        IF result+len(1) NE '.'.
          CONCATENATE result '.' INTO result.
        ENDIF.
      ENDIF.
    ENDIF.

    get_t100_longtext_itf( EXPORTING obj      = obj
                                     t100key  = t100key
                           IMPORTING itf_itab = itf_itab ).

    " replace variables
    CALL FUNCTION 'REPLACE_TEXTSYMBOL'
      EXPORTING startline = 1
                endline   = 99999
      TABLES    lines     = itf_itab.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING lf           = abap_true
      IMPORTING stream_lines = asc_itab
      TABLES    itf_text     = itf_itab.

    IF preserve_newlines EQ abap_true.
      IF result IS NOT INITIAL.
        INSERT result INTO asc_itab INDEX 1.
      ENDIF.
      CONCATENATE LINES OF asc_itab INTO result SEPARATED BY cl_abap_char_utilities=>newline.
    ELSE.
      LOOP AT asc_itab ASSIGNING <asc_line>.
        IF <asc_line> IS NOT INITIAL.
          CONCATENATE result ` ` <asc_line> INTO result.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_t100_for_object.

    CHECK obj->t100key IS NOT INITIAL.

    result-msgid = obj->t100key-msgid.
    result-msgno = obj->t100key-msgno.
    result-msgty = 'E'.

    set_single_msg_var( EXPORTING arg    = obj->t100key-attr1
                                  obj    = obj
                        IMPORTING target = result-msgv1 ).

    set_single_msg_var( EXPORTING arg    = obj->t100key-attr2
                                  obj    = obj
                        IMPORTING target = result-msgv2 ).

    set_single_msg_var( EXPORTING arg    = obj->t100key-attr3
                                  obj    = obj
                        IMPORTING target = result-msgv3 ).

    set_single_msg_var( EXPORTING arg    = obj->t100key-attr4
                                  obj    = obj
                        IMPORTING target = result-msgv4 ).

  ENDMETHOD.


  METHOD get_otr_text_raw.

    DATA text TYPE sotr_txt.

    CALL FUNCTION 'SOTR_GET_TEXT_KEY'
      EXPORTING  concept               = textid
                 search_in_third_langu = 'X'
      IMPORTING  e_text                = text
      EXCEPTIONS no_entry_found        = 1
                 OTHERS                = 2.
    IF sy-subrc EQ 0.
      result = text.
    ENDIF.

  ENDMETHOD.


  METHOD get_otr_longtext_raw.

    CALL FUNCTION 'SOTR_LINK_GET_ENTRY_RUNTIME'
      EXPORTING  concept_1 = textid
                 langu     = sy-langu
      IMPORTING  text      = result
      EXCEPTIONS no_links  = 1
                 OTHERS    = 2.
    IF sy-subrc NE 0.
      CLEAR result.
    ENDIF.

  ENDMETHOD.


  METHOD get_longtext_for_message.

    DATA t100key TYPE scx_t100key.
    DATA textid  TYPE sotr_conc.

    IF text IS NOT INITIAL.
      cl_message_helper=>check_msg_kind( EXPORTING msg     = text
                                         IMPORTING t100key = t100key
                                                   textid  = textid ).

      IF t100key IS NOT INITIAL.
        get_t100_longtext_for( EXPORTING obj               = text
                                         t100key           = t100key
                                         text              = text
                                         preserve_newlines = preserve_newlines
                                         prepend_short     = t100_prepend_short
                               IMPORTING result            = result ).
      ELSEIF textid IS NOT INITIAL.
        get_otr_longtext_raw( EXPORTING textid = textid
                              IMPORTING result = result ).
        cl_message_helper=>replace_text_params( EXPORTING obj    = text
                                                CHANGING  result = result ).
      ENDIF.
    ENDIF.

    IF result IS NOT INITIAL AND preserve_newlines EQ abap_false.
      cl_message_helper=>strip_newlines_from( CHANGING message = result ).
    ENDIF.

  ENDMETHOD.


  METHOD get_latest_t100_exception.

    DATA cur TYPE REF TO cx_root.

    cur = exception.
    WHILE cur IS BOUND.

      IF cur IS INSTANCE OF if_t100_message.
        result ?= cur.
        IF     result->t100key-msgid IS NOT INITIAL
           AND ( result->t100key-msgid NE 'SY' OR result->t100key-msgno NE 530 ).
          RETURN. " found
        ENDIF.
      ENDIF.

      cur = cur->previous.

    ENDWHILE.

    CLEAR result.

  ENDMETHOD.


  METHOD check_msg_kind.

    DATA iref TYPE REF TO if_t100_message.
    DATA exc  TYPE REF TO cx_root.
    DATA obj  TYPE REF TO object.

    FIELD-SYMBOLS <id> LIKE sy-msgid.
    FIELD-SYMBOLS <no> LIKE sy-msgno.
    FIELD-SYMBOLS <fs> TYPE any ##NEEDED.

    TRY.
        iref ?= msg.
        t100key = iref->t100key.
        " if t100key is initial. t100key = if_t100_message=>default_textid. endif.
      CATCH cx_sy_move_cast_error.
        TRY.
            exc ?= msg.
            " get OTR key from attribute TEXTID
            textid = exc->textid.
            " if there is no relevant OTR key, get T100 key from attributes
            " T100_MSGID and T100_MSGNO, parameter values(!) are in
            " T100_MSGV1, T100_MSGV2, T100_MSGV3 and T100_MSGV4.
            IF textid EQ cx_root=>cx_root OR textid IS INITIAL.
              TRY.
                  ASSIGN exc->('T100_MSGID') TO <id>.
                  IF sy-subrc EQ 0 AND <id> IS NOT INITIAL.
                    ASSIGN exc->('T100_MSGNO') TO <no>.
                    IF sy-subrc EQ 0.
                      t100key-msgid = <id>.
                      t100key-msgno = <no>.
                      ASSIGN exc->('T100_MSGV1') TO <fs>.
                      IF sy-subrc EQ 0. t100key-attr1 = 'T100_MSGV1'. ENDIF.
                      ASSIGN exc->('T100_MSGV2') TO <fs>.
                      IF sy-subrc EQ 0. t100key-attr2 = 'T100_MSGV2'. ENDIF.
                      ASSIGN exc->('T100_MSGV3') TO <fs>.
                      IF sy-subrc EQ 0. t100key-attr3 = 'T100_MSGV3'. ENDIF.
                      ASSIGN exc->('T100_MSGV4') TO <fs>.
                      IF sy-subrc EQ 0. t100key-attr4 = 'T100_MSGV4'. ENDIF.
                      CLEAR textid.
                    ENDIF.
                  ENDIF.
                CATCH cx_sy_assign_cast_illegal_cast.
                  CLEAR t100key.
              ENDTRY.
            ENDIF.
          CATCH cx_sy_move_cast_error.
            FIELD-SYMBOLS <otr> LIKE textid. " TYPE sotr_conc.
            obj = msg.
            TRY.
                ASSIGN obj->('TEXTID') TO <otr>.
                IF sy-subrc EQ 0.
                  textid = <otr>.
                ENDIF.
              CATCH cx_sy_assign_cast_illegal_cast.
                CLEAR textid.
            ENDTRY.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
