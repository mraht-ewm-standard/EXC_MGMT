"! <p class="shorttext synchronized">ABAP Unit Test Class</p>
CLASS ltc_static_check DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF s_tdc_data,
             lgnum TYPE /scwm/lgnum,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt           TYPE etobj_name VALUE 'ZCX_STATIC_CHECK'.
    CONSTANTS mc_tdc_dflt_var_name TYPE etvar_id   VALUE 'ECATTDEFAULT'.

    CLASS-DATA mo_tdc          TYPE REF TO cl_apl_ecatt_tdc_api.
    CLASS-DATA mv_tdc_var_name TYPE etvar_id.
    CLASS-DATA ms_tdc_data     TYPE s_tdc_data.

    CLASS-METHODS class_setup
      RAISING cx_ecatt_tdc_access.

    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS t0001 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_static_check IMPLEMENTATION.

  METHOD class_setup.

    mo_tdc = cl_apl_ecatt_tdc_api=>get_instance( i_testdatacontainer = mc_tdc_cnt ).

    mv_tdc_var_name = |{ sy-sysid }{ sy-mandt }|.
    DATA(lt_tdc_var) = mo_tdc->get_variant_list( ).
    IF NOT line_exists( lt_tdc_var[ table_line = mv_tdc_var_name ] ).
      mv_tdc_var_name = mc_tdc_dflt_var_name.
    ENDIF.

    LOOP AT mo_tdc->get_variant_content( mv_tdc_var_name ) ASSIGNING FIELD-SYMBOL(<ls_tdc_var_content>).
      ASSIGN COMPONENT <ls_tdc_var_content>-parname OF STRUCTURE ms_tdc_data TO FIELD-SYMBOL(<lv_tdc_value>).
      CHECK <lv_tdc_value> IS ASSIGNED.
      ASSIGN <ls_tdc_var_content>-value_ref->* TO FIELD-SYMBOL(<lv_tdc_var_value>).
      CHECK <lv_tdc_var_value> IS ASSIGNED.
      <lv_tdc_value> = <lv_tdc_var_value>.
      UNASSIGN: <lv_tdc_value>, <lv_tdc_var_value>.
    ENDLOOP.

    /scwm/cl_tm=>set_lgnum( ms_tdc_data-lgnum ).

  ENDMETHOD.


  METHOD class_teardown.
  ENDMETHOD.


  METHOD setup.
  ENDMETHOD.


  METHOD teardown.
  ENDMETHOD.


  METHOD t0001.

    " CHECK 1 = 2. ##DEACTIVATED.

    TRY.
        RAISE EXCEPTION TYPE zcx_error
          MESSAGE s007(e2) WITH '/SCWM/LGNUM'
          EXPORTING it_input_data = VALUE #( ( fnam = 'DATA_TYPE'
                                               low  = '/SCWM/LGNUM' ) ).

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_longtext) = lx_error->get_longtext( ). " DOCU_GET
        DATA(lv_msg_text) = lx_error->get_text( ).
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(ls_message)  = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_msg_text
                                        act = lv_longtext ).

  ENDMETHOD.

ENDCLASS.
