"! <p class="shorttext synchronized">ABAP Unit Test Class</p>
CLASS ltc_root DEFINITION FINAL
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
    METHODS t0009 FOR TESTING.
    METHODS t0010 FOR TESTING.
    METHODS t0011 FOR TESTING.
    METHODS t0012 FOR TESTING.
    METHODS t0013 FOR TESTING.

ENDCLASS.


CLASS ltc_root IMPLEMENTATION.

  METHOD class_setup.

    mo_aunit = zial_cl_aunit=>on_class_setup( iv_tdc_cnt  = mc_tdc_cnt
                                              ir_tdc_data = REF #( ms_tdc_data ) ).

  ENDMETHOD.


  METHOD setup.

    mo_aunit->on_setup( ).

  ENDMETHOD.


  METHOD teardown.

    zial_cl_log=>free( ).

    mo_aunit->on_teardown( ).

  ENDMETHOD.


  METHOD class_teardown.

    mo_aunit->on_class_teardown( ).

  ENDMETHOD.


  METHOD t0001.

    TRY.
        MESSAGE s499(sy) WITH 'LGNUM' 'HUID' 'RSRC' 'NLPLA' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
              MESSAGE s499(sy) WITH 'LGNUM' 'HUID' 'RSRC' 'NLPLA'
              EXPORTING input_data = VALUE #( ( fnam = 'DATA_TYPE'
                                                low  = 'LGNUM' ) ).

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
        MESSAGE s499(sy) WITH 'LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING message = ls_exp_message.

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
        MESSAGE s499(sy) WITH 'LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING messages = VALUE #( ( ls_exp_message ) ).

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
        MESSAGE s499(sy) WITH 'LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        DATA(lx_previous) = NEW zcx_error( message = ls_exp_message ).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING previous = lx_previous.

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
          EXPORTING textid = cx_sy_dynamic_osql_semantics=>unknown_table_name.

      CATCH cx_root INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

  ENDMETHOD.


  METHOD t0006.

    TRY.
        DATA(lv_exp_msgtx) = |Exception ZCX_ERROR was raised|.

        RAISE EXCEPTION TYPE zcx_error.

      CATCH cx_root INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

  ENDMETHOD.


  METHOD t0007.

    TRY.
        MESSAGE e499(sy) WITH 'LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING t100key = VALUE #( msgid = 'SY'
                                       msgno = '499'
                                       attr1 = 'ZCX_IF_CHECK_CLASS~OBJ_ID' )
                    obj_id  = 'LGNUM'.

      CATCH cx_root INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

  ENDMETHOD.


  METHOD t0008.

    TRY.
        " T100KEY
        MESSAGE s499(sy) WITH 'LGNUM1' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
              MESSAGE s499(sy) WITH 'LGNUM1'
              EXPORTING input_data = VALUE #( ( fnam = 'DATA_TYPE'
                                                low  = 'LGNUM' ) ).

      CATCH cx_root INTO DATA(lx_error_1).
        TRY.
            " MESSAGE
            MESSAGE s499(sy) WITH 'LGNUM2' INTO lv_exp_msgtx ##NEEDED.
            ls_exp_message = zial_cl_log=>to_bapiret( ).

            RAISE EXCEPTION TYPE zcx_error
              EXPORTING previous = lx_error_1
                        message  = ls_exp_message.

          CATCH zcx_error INTO DATA(lx_error_2).
            TRY.
                " MESSAGES
                MESSAGE s499(sy) WITH 'LGNUM3' INTO lv_exp_msgtx ##NEEDED.
                DATA(lt_exp_messages) = zial_cl_log=>to_bapirets( ).

                RAISE EXCEPTION TYPE zcx_error
                  EXPORTING previous = lx_error_2
                            messages = lt_exp_messages.

              CATCH zcx_static_check INTO DATA(lx_error_3).
                TRY.
                    " TEXTID
                    lv_exp_msgtx = |The database table 'TOKEN' is unknown|.
                    lt_exp_messages = zial_cl_log=>to_bapirets( iv_msgtx = lv_exp_msgtx ).

                    RAISE EXCEPTION TYPE zcx_error
                      EXPORTING previous = lx_error_3
                                textid   = cx_sy_dynamic_osql_semantics=>unknown_table_name.

                  CATCH cx_root INTO DATA(lx_error_4).
                    TRY.
                        " T100KEY
                        MESSAGE e499(sy) WITH 'LGNUM5' INTO lv_exp_msgtx ##NEEDED.
                        ls_exp_message = zial_cl_log=>to_bapiret( ).

                        RAISE EXCEPTION TYPE zcx_error
                          EXPORTING previous = lx_error_4
                                    t100key  = VALUE #( msgid = 'SY'
                                                        msgno = '499'
                                                        attr1 = 'ZCX_IF_CHECK_CLASS~OBJ_ID' )
                                    obj_id   = 'LGNUM5'.

                      CATCH zcx_error INTO DATA(lx_error_5).
                        TRY.
                            " PREVIOUS
                            RAISE EXCEPTION TYPE zcx_error
                              EXPORTING previous = lx_error_5.

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


  METHOD t0009.

    TRY.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING input_data = VALUE #( ( fnam = 'TEST' low = '1234' ) ).

      CATCH zcx_error INTO DATA(lx_error).
    ENDTRY.

    DATA(lo_root) = NEW zcx_root( io_exception        = lx_error
                                  is_t100key          = lx_error->if_t100_message~t100key
                                  iv_obj_id           = lx_error->zcx_if_check_class~obj_id
                                  is_message          = lx_error->zcx_if_check_class~message
                                  it_messages         = lx_error->zcx_if_check_class~messages
                                  iv_subrc            = lx_error->zcx_if_check_class~subrc
                                  it_input_data       = lx_error->zcx_if_check_class~input_data
                                  is_auto_log_enabled = lx_error->zcx_if_check_class~is_auto_log_enabled ).
    DATA(lt_msgde) = lo_root->create_log_msgde( ).

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_msgde ) ).

  ENDMETHOD.


  METHOD t0010.

    TRY.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING input_data = VALUE #( ( fnam = 'TEST' low = '1234' ) ).

      CATCH zcx_error INTO DATA(lx_error).
    ENDTRY.

    DATA(lo_root) = NEW zcx_root( io_exception        = lx_error
                                  is_t100key          = lx_error->if_t100_message~t100key
                                  iv_obj_id           = lx_error->zcx_if_check_class~obj_id
                                  is_message          = lx_error->zcx_if_check_class~message
                                  it_messages         = lx_error->zcx_if_check_class~messages
                                  iv_subrc            = lx_error->zcx_if_check_class~subrc
                                  it_input_data       = lx_error->zcx_if_check_class~input_data
                                  is_auto_log_enabled = lx_error->zcx_if_check_class~is_auto_log_enabled ).
    lo_root->log_info( ).

    DATA(lt_messages) = zial_cl_log=>get( )->get_messages( ).
    cl_abap_unit_assert=>assert_not_initial( act = lt_messages ).

  ENDMETHOD.


  METHOD t0011.

    TRY.
        MESSAGE s499(sy) WITH 'LGNUM' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(ls_exp_message) = zial_cl_log=>to_bapiret( ).

        RAISE EXCEPTION TYPE zcx_error
          EXPORTING message = ls_exp_message.

      CATCH zcx_error INTO DATA(lx_error).
    ENDTRY.

    DATA(lo_root) = NEW zcx_root( io_exception        = lx_error
                                  is_t100key          = lx_error->if_t100_message~t100key
                                  iv_obj_id           = lx_error->zcx_if_check_class~obj_id
                                  is_message          = lx_error->zcx_if_check_class~message
                                  it_messages         = lx_error->zcx_if_check_class~messages
                                  iv_subrc            = lx_error->zcx_if_check_class~subrc
                                  it_input_data       = lx_error->zcx_if_check_class~input_data
                                  is_auto_log_enabled = lx_error->zcx_if_check_class~is_auto_log_enabled ).
    lo_root->log( ).

    DATA(lt_messages) = zial_cl_log=>get( )->get_messages( ).
    cl_abap_unit_assert=>assert_not_initial( act = lt_messages ).

  ENDMETHOD.


  METHOD t0012.

    TRY.
        MESSAGE e499(sy) WITH 'ABCD is not a valid integer.' INTO DATA(lv_exp_msgtx) ##NEEDED.
        DATA(lt_exp_messages) = zial_cl_log=>to_bapirets( ).
        DATA(ls_exp_message) = VALUE #( lt_exp_messages[ 1 ] OPTIONAL ).

        DATA(lx_previous) = NEW cx_abap_not_an_integer( value = 'ABCD' ).
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING previous = lx_previous.

      CATCH zcx_error INTO DATA(lx_error).
        DATA(lv_act_msgtx) = lx_error->get_text( ).
        DATA(ls_act_message) = lx_error->get_message( ).
        DATA(lt_act_messages) = lx_error->get_messages( ).

    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = lv_exp_msgtx
                                        act = lv_act_msgtx ).

    cl_abap_unit_assert=>assert_equals( exp = ls_exp_message
                                        act = ls_act_message ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp_messages
                                        act = lt_act_messages ).

  ENDMETHOD.


  METHOD t0013.

    TRY.
        RAISE EXCEPTION TYPE zcx_error
          EXPORTING textid              = cx_sy_dynamic_osql_semantics=>unknown_table_name
                    is_auto_log_enabled = abap_true.

      CATCH cx_root INTO DATA(lx_error) ##NEEDED.
    ENDTRY.

    DATA(lt_messages) = zial_cl_log=>get( )->get_messages( ).
    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = lines( lt_messages ) ).

  ENDMETHOD.

ENDCLASS.
