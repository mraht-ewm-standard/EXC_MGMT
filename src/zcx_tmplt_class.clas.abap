"! <p class="shorttext synchronized" lang="en">[MEANING OF EXCEPTION]</p>
CLASS zcx_tmplt_class DEFINITION
  PUBLIC
  INHERITING FROM zcx_tmplt_group
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_cx_class.

  PROTECTED SECTION.
    CLASS-DATA log_class_enabled TYPE de_bool VALUE mc_log_enabled-undef.

ENDCLASS.


CLASS zcx_tmplt_class IMPLEMENTATION.

  METHOD zif_cx_class~enable_log.
    log_class_enabled = det_bool( iv_enable ).
  ENDMETHOD.


  METHOD zif_cx_class~is_log_enabled.
    rv_is_enabled = log_class_enabled.
  ENDMETHOD.

ENDCLASS.
