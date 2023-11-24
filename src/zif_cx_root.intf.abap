INTERFACE zif_cx_root
  PUBLIC.

  CLASS-METHODS is_log_enabled
    RETURNING
      VALUE(rv_is_enabled) TYPE cx_bool.
  CLASS-METHODS enable_log
    IMPORTING
      iv_enable TYPE abap_bool.

ENDINTERFACE.
