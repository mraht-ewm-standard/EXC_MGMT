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

    METHODS t0001 FOR TESTING.
    METHODS t0002 FOR TESTING.
    METHODS t0003 FOR TESTING.
    METHODS t0004 FOR TESTING.
    METHODS t0005 FOR TESTING.
    METHODS t0006 FOR TESTING.
    METHODS t0007 FOR TESTING.
    METHODS t0008 FOR TESTING.

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
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          MESSAGE s002(sy) WITH '/SCWM/LGNUM'
          EXPORTING it_input_data = VALUE #( ( fnam = 'DATA_TYPE'
                                               low  = '/SCWM/LGNUM' ) ).

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).
        DATA(ls_act_message) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

    cl_abap_unit_assert=>assert_equals( exp = ls_exp_message
                                        act = ls_act_message ).

  ENDMETHOD.


  METHOD t0002.

    TRY.
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING is_message = ls_exp_message.

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).
        DATA(ls_act_message) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

    cl_abap_unit_assert=>assert_equals( exp = ls_exp_message
                                        act = ls_act_message ).

  ENDMETHOD.


  METHOD t0003.

    TRY.
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING it_messages = VALUE #( ( ls_exp_message ) ).

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).
        DATA(ls_act_message) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

    cl_abap_unit_assert=>assert_equals( exp = ls_exp_message
                                        act = ls_act_message ).

  ENDMETHOD.


  METHOD t0004.

    TRY.
        MESSAGE s002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        DATA(lx_previous) = NEW zcx_error( is_message = ls_exp_message ).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING io_previous = lx_previous.

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).
        DATA(ls_act_message) = lx_error->get_message( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

    cl_abap_unit_assert=>assert_equals( exp = ls_exp_message
                                        act = ls_act_message ).

  ENDMETHOD.


  METHOD t0005.

    TRY.
        DATA(lv_exp_msgtx) = |The database table 'TOKEN' is unknown|.

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING iv_textid = cx_sy_dynamic_osql_semantics=>unknown_table_name.

      CATCH cx_root INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

  ENDMETHOD.


  METHOD t0006.

    TRY.
        DATA(lv_exp_msgtx) = |An exception was raised|.

        RAISE EXCEPTION TYPE zcx_error.

      CATCH cx_root INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

  ENDMETHOD.


  METHOD t0007.

    TRY.
        MESSAGE e002(sy) WITH '/SCWM/LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING is_t100key = VALUE #( msgid = 'SY'
                                          msgno = '002'
                                          attr1 = 'ZCX_IF_CHECK_CLASS~OBJ_ID' )
                    iv_obj_id  = '/SCWM/LGNUM'.

      CATCH cx_root INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

  ENDMETHOD.


  METHOD t0008.

    TRY.
        " T100KEY
        MESSAGE s002(sy) WITH '/SCWM/LGNUM1' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          MESSAGE s002(sy) WITH '/SCWM/LGNUM1'
          EXPORTING it_input_data = VALUE #( ( fnam = 'DATA_TYPE'
                                               low  = '/SCWM/LGNUM' ) ).

      CATCH cx_root INTO DATA(lx_error_1).
        TRY.
            " MESSAGE
            MESSAGE s002(sy) WITH '/SCWM/LGNUM2' INTO lv_exp_msgtx ##NEEDED.
            ls_exp_message = zial_cl_log=>to_bapiret( ).

            RAISE EXCEPTION TYPE zcx_error
              EXPORTING io_previous = lx_error_1
                        is_message  = ls_exp_message.

          CATCH cx_root INTO DATA(lx_error_2).
            TRY.
                " MESSAGES
                MESSAGE s002(sy) WITH '/SCWM/LGNUM3' INTO lv_exp_msgtx ##NEEDED.
                ls_exp_message = zial_cl_log=>to_bapiret( ).

                RAISE EXCEPTION TYPE zcx_error
                  EXPORTING io_previous = lx_error_2
                            it_messages = VALUE #( ( ls_exp_message ) ).

              CATCH cx_root INTO DATA(lx_error_3).
                TRY.
                    " TEXTID
                    lv_exp_msgtx = |The database table 'TOKEN' is unknown|.
                    ls_exp_message = zial_cl_log=>to_bapiret( iv_msgtx = CONV #( lv_exp_msgtx ) ).

                    RAISE EXCEPTION TYPE zcx_error
                      EXPORTING io_previous = lx_error_3
                                iv_textid   = cx_sy_dynamic_osql_semantics=>unknown_table_name.

                  CATCH cx_root INTO DATA(lx_error_4).
                    TRY.
                        " T100KEY
                        MESSAGE e002(sy) WITH '/SCWM/LGNUM5' INTO lv_exp_msgtx ##NEEDED.
                        ls_exp_message = zial_cl_log=>to_bapiret( ).

                        RAISE EXCEPTION TYPE zcx_error
                          EXPORTING io_previous = lx_error_4
                                    is_t100key  = VALUE #( msgid = 'SY'
                                                           msgno = '002'
                                                           attr1 = 'ZCX_IF_CHECK_CLASS~OBJ_ID' )
                                    iv_obj_id   = '/SCWM/LGNUM5'.

                      CATCH zcx_error INTO DATA(lx_error_5).
                        TRY.
                            " PREVIOUS
                            RAISE EXCEPTION TYPE zcx_error
                              EXPORTING io_previous = lx_error_5.

                          CATCH zcx_error INTO DATA(lx_error_6).
                            DATA(lt_act_messages) = lx_error_6->get_messages( ).
                            DATA(lv_act_msgtx) = lx_error_6->get_text( ).
                            DATA(ls_act_message) = lx_error_6->get_message( ).

                        ENDTRY.

                        cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                                            act = lv_act_msgtx ).

                        cl_abap_unit_assert=>assert_equals( exp = ls_exp_message
                                                            act = ls_act_message ).

                        cl_abap_unit_assert=>assert_equals( exp = 5
                                                            act = lines( lt_act_messages ) ).

                    ENDTRY.

                ENDTRY.

            ENDTRY.

        ENDTRY.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
