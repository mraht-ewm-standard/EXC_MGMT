"! <p class="shorttext synchronized">ABAP Unit Test Class</p>
CLASS ltc_static_check DEFINITION FINAL
  CREATE PUBLIC
  FOR TESTING RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: BEGIN OF s_tdc_data,
             dummy TYPE dummy,
           END OF s_tdc_data.

    CONSTANTS mc_tdc_cnt TYPE etobj_name VALUE 'ZCX_STATIC_CHECK'.

    CLASS-DATA mo_aunit    TYPE REF TO zial_cl_aunit.
    CLASS-DATA ms_tdc_data TYPE s_tdc_data.

    CLASS-METHODS class_setup
      RAISING cx_ecatt_tdc_access.

    CLASS-METHODS class_teardown.

    METHODS setup.
    METHODS teardown.

    METHODS t0001 FOR TESTING RAISING cx_static_check.
    METHODS t0002 FOR TESTING RAISING cx_static_check.
    METHODS t0003 FOR TESTING RAISING cx_static_check.
    METHODS t0004 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_static_check IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup( iv_tdc_cnt  = mc_tdc_cnt
                                              ir_tdc_data = REF #( ms_tdc_data ) ).

  ENDMETHOD.


  METHOD setup.

    mo_aunit->on_setup( ).

  ENDMETHOD.


  METHOD teardown.

    mo_aunit->on_teardown( ).

  ENDMETHOD.


  METHOD class_teardown.

    mo_aunit->on_class_teardown( ).

  ENDMETHOD.


  METHOD t0001.

    TRY.
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_msgtx) ##NEEDED.
        DATA(ls_bapiret) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          MESSAGE s002(sy) WITH '/SCWM/LGNUM'
          EXPORTING input_data = VALUE #( ( fnam = 'DATA_TYPE'
                                            low  = '/SCWM/LGNUM' ) ).

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_exc_text)    = lx_error->get_text( ).
        DATA(ls_exc_bapiret) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_msgtx
                                        act = lv_exc_text ).

    cl_abap_unit_assert=>assert_equals( exp = ls_bapiret
                                        act = ls_exc_bapiret ).

  ENDMETHOD.


  METHOD t0002.

    TRY.
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_msgtx) ##NEEDED.
        DATA(ls_bapiret) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING message = ls_bapiret.

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_exc_text)    = lx_error->get_text( ).
        DATA(ls_exc_bapiret) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_msgtx
                                        act = lv_exc_text ).

    cl_abap_unit_assert=>assert_equals( exp = ls_bapiret
                                        act = ls_exc_bapiret ).

  ENDMETHOD.


  METHOD t0003.

    TRY.
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_msgtx) ##NEEDED.
        DATA(ls_bapiret) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING messages = VALUE #( ( ls_bapiret ) ).

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_exc_text)    = lx_error->get_text( ).
        DATA(ls_exc_bapiret) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_msgtx
                                        act = lv_exc_text ).

    cl_abap_unit_assert=>assert_equals( exp = ls_bapiret
                                        act = ls_exc_bapiret ).

  ENDMETHOD.


  METHOD t0004.

    TRY.
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_msgtx) ##NEEDED.
        DATA(ls_bapiret) = zial_cl_log=>to_bapiret( ).

        DATA(lx_previous) = NEW zcx_error( message = ls_bapiret ).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING previous = lx_previous.

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_exc_text)    = lx_error->get_text( ).
        DATA(ls_exc_bapiret) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_msgtx
                                        act = lv_exc_text ).

    cl_abap_unit_assert=>assert_equals( exp = ls_bapiret
                                        act = ls_exc_bapiret ).

  ENDMETHOD.

ENDCLASS.
