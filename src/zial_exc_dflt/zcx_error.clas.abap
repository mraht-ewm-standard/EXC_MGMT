"! <p class="shorttext synchronized" lang="en">Dynamic error</p>
CLASS zcx_error DEFINITION
  PUBLIC
  INHERITING FROM zcx_dev
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS is_log_class_enabled
      RETURNING
        VALUE(rv_log) TYPE cx_bool.
    CLASS-METHODS enable_log_class
      IMPORTING
        log_enabled TYPE abap_bool.

  PROTECTED SECTION.
    CLASS-DATA log_class_enabled TYPE cx_bool VALUE zcx_root=>undef.

ENDCLASS.



CLASS zcx_error IMPLEMENTATION.

  METHOD enable_log_class.
    log_class_enabled = zcx_root=>det_bool( log_enabled ).
  ENDMETHOD.


  METHOD is_log_class_enabled.
    rv_log = log_class_enabled.
  ENDMETHOD.

ENDCLASS.
